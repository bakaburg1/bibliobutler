#' Retrieve Articles from Semantic Scholar API
#'
#' This function fetches article data from the Semantic Scholar API using either
#' article IDs, a search query, or a filter query. It allows for customization
#' of returned fields and pagination of results. Only one of `ids`, `query`, or
#' `filter_query` should be provided.
#'
#' @param ids A vector of Semantic Scholar Paper IDs, DOIs, ArXiv IDs, MAG IDs,
#'   ACL IDs, PubMed IDs, or Corpus IDs.
#' @param query A string to search for in the Semantic Scholar database.
#' @param filter_query A string to filter the search results (used with advanced
#'   query syntax).
#' @param fields A vector of field names to be returned in the API response. If
#'   NULL, default fields are used.
#' @param per_page Number of results to return per page (default is 100, max is
#'   100).
#' @param max_results Maximum number of total results to return (default is
#'   Inf).
#'
#' @return A list of article data from Semantic Scholar.
#'
#' @details This function interacts with the Semantic Scholar API to retrieve
#'   article information. We suggest getting an API key from Semantic Scholar
#'   and setting the 'bibliobutler_semanticscholar_key' option to not incur in
#'   rate limits.
#'
#' @examples
#' \dontrun{
#' # Search for articles about "machine learning"
#' results <- get_semanticscholar_article(query = "machine learning", max_results = 500)
#'
#' # Get specific articles by their IDs
#' results <- get_semanticscholar_article(ids = c("649def34f8be52c8b66281af98ae884c09aef38b"))
#' }
#'
#' @importFrom httr add_headers RETRY
#'
#' @export
get_semanticscholar_article <- function(
    ids = NULL,
    query = NULL,
    filter_query = NULL,
    fields = NULL,
    per_page = 100,
    max_results = Inf) {
  # Input validation: ensure only one of ids, query, or filter_query is provided
  inputs <- c(!is.null(ids), !is.null(query), !is.null(filter_query))
  if (sum(inputs) != 1) {
    stop("Use only one between `ids`, `query`, or `filter_query` as arguments")
  }
  if (sum(inputs) == 0) {
    stop("One `ids`, `query`, or `filter_query` must be provided.")
  }

  # Define default fields and combine with user-specified fields
  id_fields <- c("paperId", "externalIds")

  default_fields <- c(
    "title", "abstract", "authors", "year",
    "venue", "publicationTypes",
    "references.externalIds", "citations.externalIds"
  )

  fields <- c(id_fields, fields %||% default_fields) |>
    unique() |>
    paste(collapse = ",")

  # Prepare the query data and headers
  query_data <- list(fields = fields)
  headers <- httr::add_headers(
    "x-api-key" = getOption("bibliobutler_semanticscholar_key")
  )

  # Function to make a single API call
  make_api_call <- function(endpoint, method, body = NULL, query = NULL) {
    response <- httr::RETRY(
      method,
      endpoint,
      headers = headers,
      query = query,
      body = body,
      encode = "json",
      times = 3,
      pause_min = 1,
      pause_cap = 60,
      terminate_on = getOption("newis_terminate_retry_on")
    )

    if (httr::http_error(response)) {
      stop("Error in API request: ", httr::http_status(response)$message)
    }

    content <- httr::content(response, "text", encoding = "UTF-8")
    jsonlite::fromJSON(content, flatten = TRUE)
  }

  # Function to process the API response
  process_response <- function(data) {
    if ("data" %in% names(data)) data <- data$data

    data |> dplyr::mutate(
      .api = "semanticscholar",
      paperId = coalesce(
        col_get("externalIds.DOI"),
        col_get("paperId"),
        NA_character_
      ),
      title = col_get("title"),
      abstract = col_get("abstract"),
      across(any_of("authors"), ~ .x |> purrr::map_chr(\(.y) {
        paste(.y[["name"]], collapse = ", ") |>
          stringr::str_remove_all("\\.")
      })),
      year = col_get("year"),
      journal = col_get("venue"),
      pubtype = if (!is.null(data[["publicationTypes"]])) {
        purrr::map_chr(data[["publicationTypes"]], ~ paste(.x, collapse = ", "))
      } else {
        NA_character_
      },
      .references = purrr::map(
        data[["references"]], ~ coalesce(
          .x[["externalIds.DOI"]], .x[["paperId"]]
        )
      ),
      .citations = purrr::map(
        data[["citations"]], ~ coalesce(
          .x[["externalIds.DOI"]], .x[["paperId"]]
        )
      ),
      .keep = "none"
    )
  }

  # Function to process a batch of IDs
  process_batch <- function(batch_ids) {
    endpoint <- "https://api.semanticscholar.org/graph/v1/paper/batch"
    body <- list(ids = as.list(unname(batch_ids)))

    results <- make_api_call(endpoint, "POST", body = body, query = query_data)
    process_response(results)
  }

  # Initialize results
  all_results <- list()

  # Process IDs if provided
  if (!is.null(ids)) {
    valid_ids <- prepare_semanticscholar_ids(ids)

    # Process IDs in batches of 500
    id_batches <- split(valid_ids, ceiling(seq_along(valid_ids) / 500))

    all_results <- furrr::future_map(
      id_batches, process_batch,
      .progress = TRUE
    ) |>
      dplyr::bind_rows()
  } else {
    # Use paper/search/bulk for query or filter_query
    endpoint <- "https://api.semanticscholar.org/graph/v1/paper/search/bulk"
    query_data$limit <- per_page

    if (!is.null(query)) {
      query_data$query <- query
    } else {
      query_data$filter <- filter_query
    }

    all_results <- data.frame()
    token <- NULL

    # Fetch results in batches until max_results is reached or no more results
    repeat {
      if (!is.null(token)) {
        query_data$token <- token
      }

      batch_results <- make_api_call(endpoint, "GET", query = query_data)

      all_results <- bind_rows(all_results, batch_results$data)

      token <- batch_results$token

      if (is.null(token) || nrow(all_results) >= max_results) {
        break
      }
    }
  }

  # Combine all results and generate record names
  results <- all_results |>
    mutate(
      RecordName = generate_record_name(all_results),
      .after = ".api"
    )

  if (nrow(results) == 0) {
    warning(
      "No results found for the given query.",
      call. = FALSE, immediate. = TRUE
    )
  }

  return(results)
}

#' Get linked articles from Semantic Scholar
#'
#' This function retrieves linked articles (citations, references, and/or
#' related papers) for given article IDs from the Semantic Scholar API.
#'
#' @param ids A character vector of article identifiers. Can be DOIs, PMIDs, or
#'   Semantic Scholar IDs.
#' @param links A character vector specifying which types of linked articles to
#'   retrieve. Can include any combination of "citations", "references", and
#'   "related".
#'
#' @return A list containing data frames for each requested link type:
#'   \itemize{
#'     \item citations: Articles that cite the input articles
#'     \item references: Articles cited by the input articles
#'     \item related: Articles recommended as related to the input articles
#'   }
#'   Each data frame contains source_id (the input article ID) and linked_id
#'   (the ID of the linked article).
#'
#' @details Citations and references are retrieved using the main Semantic
#' Scholar API, while related articles are fetched using the recommendations
#' API. The function handles ID conversion and validation internally.
#'
#' @examples
#' \dontrun{
#' # Get all types of linked articles
#' links <- get_semanticscholar_linked("10.1016/j.cell.2023.01.001")
#'
#' # Get only citations and references
#' links <- get_semanticscholar_linked(
#'   "10.1016/j.cell.2023.01.001",
#'   links = c("citations", "references")
#' )
#' }
#'
get_semanticscholar_linked <- function(
    ids,
    links = c("citations", "references", "related")) {
  # Validate and select the link types
  links <- match.arg(links, several.ok = TRUE)

  # Convert and validate IDs
  ids <- prepare_semanticscholar_ids(ids)

  if (length(ids) == 0) {
    return(list())
  }

  results <- list()

  # Fetch citations and references using get_semanticscholar_article
  if (any(c("citations", "references") %in% links)) {
    fields <- c(references = "references.externalIds", citations = "citations.externalIds")[links] |>
      purrr::discard(is.na)

    article_data <- get_semanticscholar_article(ids = ids, fields = fields)

    if ("citations" %in% links) {
      results$citations <- article_data |>
        dplyr::select(source_id = "paperId", ".citations") |>
        tidyr::unnest(".citations") |>
        dplyr::select("source_id", linked_id = ".citations")
    }

    if ("references" %in% links) {
      results$references <- article_data |>
        dplyr::select(source_id = "paperId", ".references") |>
        tidyr::unnest(".references") |>
        dplyr::select("source_id", linked_id = ".references")
    }
  }

  # Fetch related papers using the recommendations API
  if ("related" %in% links) {
    recommendations_url <- "https://api.semanticscholar.org/recommendations/v1/papers"

    body <- list(
      positivePaperIds = as.list(ids),
      fields = list("paperId")
    )

    response <- httr::RETRY(
      "POST",
      recommendations_url,
      body = body,
      encode = "json",
      times = 3,
      pause_min = 1,
      pause_cap = 60
    )

    if (httr::http_error(response)) {
      warning("Error fetching related papers: ", httr::http_status(response)$message,
        call. = FALSE, immediate. = TRUE
      )
    } else {
      content <- httr::content(response, "parsed")
      results$related <- purrr::map_dfr(content$recommendedPapers, function(paper) {
        data.frame(
          linked_id = paper$paperId,
          source_id = ids,
          stringsAsFactors = FALSE
        )
      })
    }
  }

  # Remove potential duplicates
  results <- purrr::map(results, dplyr::distinct)

  return(results)
}


#' Prepare IDs for Semantic Scholar API
#'
#' This function takes a vector of article identifiers and prepares them for use
#' with the Semantic Scholar API by converting them to the appropriate format.
#'
#' @param ids A character vector of article identifiers. Can be DOIs, PMIDs, or
#'   Semantic Scholar IDs.
#'
#' @return A character vector of properly formatted IDs for use with the
#'   Semantic Scholar API. DOIs will be prefixed with "DOI:", PMIDs with
#'   "PMID:", and Semantic Scholar IDs will be left as-is. Invalid or
#'   unconvertible IDs are removed.
#'
#' @examples
#' \dontrun{
#' # Prepare mixed IDs
#' ids <- c(
#'   "10.1016/j.cell.2023.01.001", "12345678",
#'   "649def34f8be52c8b66281af98ae884c09aef38b"
#' )
#' prepared_ids <- prepare_semanticscholar_ids(ids)
#' }
#'
prepare_semanticscholar_ids <- function(ids) {
  valid_ids <- ids |>
    as.character() |>
    setdiff(NA_character_)

  # Return early if no valid IDs are passed
  if (length(valid_ids) == 0) {
    warning("No valid IDs passed",
      call. = FALSE, immediate. = TRUE
    )

    return(character())
  }

  # Convert or prefix IDs using case_match
  valid_ids <- dplyr::case_match(
    get_article_id_type(ids),
    "semanticscholar" ~ ids,
    "pmid" ~ paste0("PMID:", ids),
    "doi" ~ paste0("DOI:", ids),
    NA ~ NA_character_,
    .default = "convert"
  )

  # Convert any IDs marked for conversion
  to_convert <- which(valid_ids == "convert")

  if (length(to_convert) > 0) {
    converted <- convert_article_id(ids[to_convert], to = "doi")
    valid_ids[to_convert] <- paste0("DOI:", converted)
  }

  # Remove any NAs or NFs (ie, could not be converted)
  valid_ids <- setdiff(valid_ids, c(NA, "DOI:NF"))

  if (length(valid_ids) == 0) {
    warning("No valid IDs after conversion.", call. = FALSE, immediate. = TRUE)
  }

  valid_ids
}
