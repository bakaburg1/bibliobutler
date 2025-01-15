#' Retrieve Articles from Semantic Scholar API
#'
#' This function fetches article data from the Semantic Scholar API
#' (https://api.semanticscholar.org/api-docs) using either article IDs (via the
#' `/paper/batch` endpoint), a search query (via the `/paper/search/bulk`
#' endpoint), or a filter query. It allows for customization of returned fields
#' and pagination of results. Only one of `ids` or `query` should be provided.
#'
#' @param ids A vector of Semantic Scholar Paper IDs, DOIs, ArXiv IDs, MAG IDs,
#'   ACL IDs, PubMed IDs, or Corpus IDs.
#' @param query A string to search for in the Semantic Scholar database.
#' @param year_filter Publication year or range (e.g., "2019", "2016-2020",
#'   "2010-", "-2015"). If provided, this overrides any year filter in the
#'   filters argument.
#' @param filters A list of filters to apply to the search. Available filters
#'   include:
#'   \itemize{
#'     \item publicationTypes (string): Filter by publication type, as a
#'            comma-separated list,
#'     \item openAccessPdf (string): Restricts to papers with public PDF. No
#'            value needed.
#'     \item minCitationCount (string): Minimum number of citations
#'     \item publicationDateOrYear (string): Date range in YYYY-MM-DD:YYYY-MM-DD
#'            format. Partial dates (YYYY-MM or YYYY) and open ranges supported.
#'     \item year (string): Publication year or range (see year_filter
#'            parameter)
#'     \item venue (string): Filter by venue/journal names, comma-separated
#'            list. Accepts full names or ISO4 abbreviations.
#'     \item fieldsOfStudy (string): Comma-separated list.
#'   }
#' @param fields A vector of field names to be returned in the API response. If
#'   NULL, default fields are used. Note that "citations" and "references" are
#'   not supported in the bulk search and will be ignored.
#' @param per_page Number of results per page (default is 100).
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
#' results <- get_semanticscholar_articles(
#'   query = "machine learning",
#'   year_filter = "2023-",
#'   max_results = 500
#' )
#'
#' # Get specific articles by their IDs
#' results <- get_semanticscholar_articles(
#'   ids = c(
#'     "649def34f8be52c8b66281af98ae884c09aef38b",
#'     "10.1145/3197026.3197040"
#'   )
#' )
#' }
#'
#' @export
get_semanticscholar_articles <- function(
    ids = NULL,
    query = NULL,
    year_filter = NULL,
    filters = NULL,
    fields = NULL,
    per_page = 100,
    max_results = Inf) {

  # Input validation: ensure only one of ids or query is provided

  if (!is.null(ids) && !is.null(query)) {
    stop("Use only one of `ids` or `query` as arguments")
  }
  if (is.null(ids) && is.null(query)) {
    stop("Either `ids` or `query` must be provided")
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

  # Prepare the query data
  query_data <- list(fields = fields)

  # Initialize results
  all_results <- list()

  # Process IDs if provided
  if (!is.null(ids)) {
    if (!is.null(filters) || !is.null(year_filter)) {
      msg_warn("Filters and year_filter are ignored when using IDs")
    }

    valid_ids <- s2_prepare_ids(ids)

    # Process IDs in batches of 500
    id_batches <- split(valid_ids, ceiling(seq_along(valid_ids) / 500))

    all_results <- furrr::future_map(
      id_batches,
      \(batch) s2_process_batch(batch, query_data),
      .progress = TRUE
    ) |>
      dplyr::bind_rows()
  } else {
    # Use paper/search/bulk for query
    endpoint <- "https://api.semanticscholar.org/graph/v1/paper/search/bulk"

    query_data$query <- query

    # Process filters if provided
    if (!is.null(filters)) {
      # Validate that filters is a list
      if (!is.list(filters)) {
        stop("filters must be a list")
      }

      # Append all filters to query_data with proper prefix
      query_data <- c(
        query_data,
        filters
      )
    }

    # Add year_filter if provided (overrides any year filter)
    if (!is.null(year_filter)) {
      if (!is.null(filters$year)) {
        msg_warn("year_filter overrides 'year' in filters argument")
      }
      query_data$year <- year_filter
    }

    # Citations and references are not supported in the bulk search
    query_data$fields <- query_data$fields |>
      stringr::str_remove_all(",?(references|citations)\\.externalIds")

    all_results <- data.frame()
    token <- NULL
    page <- 1
    print_total <- TRUE

    # Fetch results in batches until max_results is reached or no more results
    repeat {
      if (!is.null(token)) {
        query_data$token <- token
      }

      batch_results <- s2_make_api_call(
        endpoint, "GET", query = query_data
      )

      all_results <- bind_rows(all_results, batch_results$data)

      token <- batch_results$token

      if (print_total) {
        print_total <- FALSE
        msg_info("Total results: ", batch_results$total)
        if (batch_results$total > max_results) {
          msg_info("(retrieving first ", max_results, " results)")
        }
      }

      msg_status(
        "Collected result page: ", page, " of ",
        ceiling(min(max_results, batch_results$total) / 1000), "\r",
        appendLF = FALSE
      )

      page <- page + 1

      if (is.null(token) || nrow(all_results) >= max_results) {
        break
      }
    }

    all_results <- all_results |>
      head(max_results) |>
      s2_process_response()

  }

  if (nrow(all_results) == 0) {
    warning(
      "No results found for the given query.",
      call. = FALSE, immediate. = TRUE
    )
  }

  return(all_results)
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
    links = c("citations", "references", "related")
) {
  # Validate and select the link types
  links <- match.arg(links, several.ok = TRUE)

  # Convert and validate IDs
  ids <- s2_prepare_ids(ids)

  if (length(ids) == 0) {
    return(list())
  }

  results <- list()

  # Fetch citations and references using get_semanticscholar_articles
  if (any(c("citations", "references") %in% links)) {
    fields <- c(
      references = "references.externalIds",
      citations = "citations.externalIds"
    )[links] |>
      purrr::discard(is.na)

    article_data <- get_semanticscholar_articles(ids = ids, fields = fields)

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
    endpoint <- "https://api.semanticscholar.org/recommendations/v1/papers"

    body <- list(
      positivePaperIds = as.list(ids),
      fields = list("paperId")
    )

    content <- s2_make_api_call(
      endpoint = endpoint,
      method = "POST",
      body = body,
      query = list(fields = "externalIds")
    )

    results$related <- purrr::map(content$recommendedPapers, function(paper) {
      data.frame(
        source_id = ids,
        linked_id = s2_buld_id(paper),
        stringsAsFactors = FALSE
      )
    }) |>
      dplyr::bind_rows()
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
#' prepared_ids <- s2_prepare_ids(ids)
#' }
#'
s2_prepare_ids <- function(ids) {
  valid_ids <- ids |>
    as.character() |>
    setdiff(NA_character_)

  # Return early if no valid IDs are passed
  if (length(valid_ids) == 0) {
    msg_warn("No valid IDs passed")

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
    msg_warn("No valid IDs after conversion.")
  }

  valid_ids
}

#' Build a Semantic Scholar ID from paper data
#'
#' This function takes a list of paper data and attempts to build a unique
#' identifier for the paper. It checks the `externalIds` field for DOI, ArXiv,
#' and PubMed IDs, and if none of those are available, it uses the `paperId`
#' field.
#'
#' @param paper_data A list of paper data, typically from the Semantic Scholar
#'   API.
#'
#' @return A character vector of Semantic Scholar IDs.

s2_buld_id <- function(paper_data) {

  if (length(paper_data) == 0) {
    return(NA)
  }

  paper_data <- as.data.frame(paper_data)

  if (!"externalIds" %in% names(paper_data)) {
    return(paper_data$paper_data)
  }

  dplyr::coalesce(
    paper_data$externalIds$DOI,
    if (!is.null(paper_data$externalIds$ArXiv)) {
      paste0("10.48550/arXiv.", paper_data$externalIds$ArXiv)
    },
    paper_data$externalIds$PubMed,
    paper_data$paperId
  ) |> tolower()

}


#' Process Response from Semantic Scholar API
#'
#' This function processes the raw response data from the Semantic Scholar API
#' and transforms it into a standardized format. It extracts and formats key
#' paper metadata including IDs, title, abstract, authors, year, journal,
#' publication type, references, and citations.
#'
#' @param data A list or data frame containing the raw API response data from
#'   Semantic Scholar. Can be either the full response object or just the data
#'   portion.
#'
#' @return A data frame containing the processed paper metadata with
#'   standardized column names and formats. Includes:
#'   \itemize{
#'     \item paperId: Unique identifier built from available IDs
#'     \item title: Paper title
#'     \item abstract: Paper abstract
#'     \item authors: Formatted author names
#'     \item year: Publication year
#'     \item journal: Publication venue/journal
#'     \item pubtype: Publication type(s)
#'     \item .references: List of reference paper IDs
#'     \item .citations: List of citing paper IDs
#'     \item .api: API source identifier
#'     \item .ids: Data frame of all available identifiers
#'   }
#'
s2_process_response <- function(data) {
  if ("data" %in% names(data)) data <- data$data

  original_id <- data$paperId

  data <- data |> dplyr::mutate(
    paperId = s2_buld_id(data),
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
    across(
      any_of(c("references", "citations")),
      ~ purrr::map(.x, s2_buld_id),
      .names = ".{col}"
    ),
    .api = "semanticscholar",
    .ids = data.frame(
      semanticscholar = original_id,
      data$externalIds |>
        select(
          doi = any_of("DOI"),
          pmid = any_of("PubMed"),
          pmcid = any_of("PubMedCentral"),
          arxiv = any_of("ArXiv")
        )
    ) |> purrr::discard(~ all(is.na(.x))),
    .keep = "none"
  )

  data |> dplyr::mutate(
    record_name = generate_record_name(data)
  )
}

#' Process a batch of article IDs through the Semantic Scholar API
#'
#' This internal function processes a batch of article IDs by making a POST
#' request to the Semantic Scholar batch endpoint. It formats the IDs into the
#' required structure, makes the API call, and processes the response.
#'
#' @param batch_ids A character vector of article IDs to process in this batch
#' @param query_data Optional list of query parameters to include in the API
#'   request
#'
#' @return A processed data frame containing the article data returned by the
#'   API, with standardized column names and formats
#'
#' @keywords internal
s2_process_batch <- function(batch_ids, query_data = NULL) {
  endpoint <- "https://api.semanticscholar.org/graph/v1/paper/batch"
  body <- list(ids = as.list(unname(batch_ids)))

  results <- s2_make_api_call(
    endpoint, "POST", body = body,
    query = query_data
  )

  s2_process_response(results)
}

#' Make a single API call to the Semantic Scholar API
#'
#' This internal function makes a single API call to the Semantic Scholar API
#' using the httr package. It handles retries, error handling, and encoding.
#'
#' @param endpoint The API endpoint URL
#' @param method The HTTP method to use (e.g., "GET", "POST")
#' @param body Optional list of body parameters to include in the API request
#' @param query Optional list of query parameters to include in the API request
#' @param headers Optional additional HTTP headers to include in the API request
#'
#' @return The parsed JSON response from the API
#'
#' @keywords internal
s2_make_api_call <- function(
  endpoint,
  method,
  body = NULL,
  query = NULL
) {
  # Create base request with endpoint
  req <- httr2::request(endpoint) |>
    # Set the method
    httr2::req_method(method) |>
    # Add the API key, will be ignored if NULL
    httr2::req_headers(
    "x-api-key" = getOption("bibliobutler_semanticscholar_key")
    ) |>
    # Add retry functionality
    httr2::req_retry(
      max_tries = 3,
      is_transient = \(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 503)
      }
    )

  # Add query parameters if provided
  if (!is.null(query)) {
    req <- req |> httr2::req_url_query(!!!query)
  }

  # Add body with json encoding if provided
  if (!is.null(body)) {
    req <- req |> httr2::req_body_json(body)
  }

  # Perform request and parse JSON response
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  return(resp)
}
