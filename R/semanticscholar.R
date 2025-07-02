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
#' @return A data frame of article data from Semantic Scholar.
#'
#' @details This function interacts with the Semantic Scholar API to retrieve
#'   article information. We suggest getting an API key from Semantic Scholar
#'   and setting the 'bibliobutler.semanticscholar_key' option to not incur in
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
  max_results = Inf
) {
  debug_mode <- isTRUE(getOption("bibliobutler.dev_mode", FALSE))
  if (debug_mode) func_start <- Sys.time()
  on.exit(
    {
      if (debug_mode) {
        elapsed <- round(as.numeric(Sys.time() - func_start, units = "secs"), 2)
        msg_status(
          "DEBUG: Total get_semanticscholar_articles() time: {elapsed} s"
        )
      }
    },
    add = TRUE
  )

  # Input validation: ensure only one of ids or query is provided

  if (!is.null(ids) && !is.null(query)) {
    stop("Use only one of `ids` or `query` as arguments")
  }
  if (is.null(ids) && (is.null(query) || !nzchar(trimws(query)))) {
    stop("A valid query must be provided when no IDs are given.")
  }

  # Define default fields and combine with user-specified fields
  id_fields <- c("paperId", "externalIds")

  default_fields <- c(
    "title",
    "abstract",
    "authors",
    "year",
    "isOpenAccess",
    "venue",
    "openAccessPdf",
    "publicationTypes"
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

    # Create request objects for parallel HTTP execution
    reqs <- purrr::map(id_batches, function(batch) {
      s2_make_api_call(
        "https://api.semanticscholar.org/graph/v1/paper/batch",
        "POST",
        body = list(ids = as.list(unname(batch))),
        query = query_data,
        as_req = TRUE
      )
    })

    msg_status(
      "Fetching {length(reqs)} Semantic Scholar batches in parallel..."
    )
    # Perform requests in parallel
    resps <- httr2::req_perform_parallel(reqs, on_error = "continue")

    # Process the responses
    all_results <- purrr::map(resps, function(resp) {
      if (inherits(resp, "httr2_response")) {
        # Parse the JSON response
        json_resp <- httr2::resp_body_json(resp, simplifyVector = TRUE)
        s2_process_response(json_resp)
      } else {
        # This was an error object
        msg_warn("Failed to fetch a batch: {conditionMessage(resp)}")
        return(data.frame())
      }
    })

    all_results <- dplyr::bind_rows(all_results)
  } else {
    # Use paper/search/bulk for query
    endpoint <- "https://api.semanticscholar.org/graph/v1/paper/search/bulk"

    # Citations and references are not supported in the bulk search
    query_data$fields <- query_data$fields |>
      stringr::str_remove_all(",?(references|citations)\\.externalIds")

    query_data$query <- query

    # Process filters if provided
    if (!is.null(filters)) {
      # Validate that filters is a list
      if (!is.list(filters)) {
        stop("Filters must be a list")
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

    all_results <- data.frame()
    token <- NULL
    page <- 1
    print_total <- TRUE

    # Fetch results in batches until max_results is reached or no more results.
    # Bulk search returns 1000 results per page and next page token to fetch
    # the next page. Parallelization is not possible AFAIK.

    # Fetch results in batches using pagination token until max_results is
    # reached or no more results are available
    repeat {
      # Add pagination token to query if available from previous request
      if (!is.null(token)) {
        query_data$token <- token
      }

      # Make API call to get batch of results
      batch_results <- s2_make_api_call(
        endpoint,
        "GET",
        query = query_data
      )

      # Append batch results to accumulated results
      all_results <- bind_rows(all_results, batch_results$data)

      # Get token for next page
      token <- batch_results$token

      total_pages <- ceiling(min(max_results, batch_results$total) / 1000)

      # Print total results count on first page
      if (print_total) {
        print_total <- FALSE
        msg_info("Total results: {batch_results$total} for query")
        if (batch_results$total > max_results) {
          msg_info("(retrieving first {max_results} results)")
        }
        msg_status("Fetching {total_pages} pages of Semantic Scholar results")
      }

      # Show progress message for current page
      msg_status(
        "[{page} / {total_pages}]",
        appendLF = FALSE
      )

      page <- page + 1

      # Break if no more pages or max results reached
      if (is.null(token) || nrow(all_results) >= max_results) {
        cat("\n") # Add newline after page counter message
        break
      }

      cat("\r") # Reset page counter message
    }

    # Trim results to max_results and process into standard format
    all_results <- all_results |>
      head(max_results) |>
      s2_process_response()

    msg_success("Fetched {nrow(all_results)} results from Semantic Scholar")
  }

  if (nrow(all_results) == 0) {
    warning(
      "No results found for the given query.",
      call. = FALSE,
      immediate. = TRUE
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
  debug_mode <- isTRUE(getOption("bibliobutler.dev_mode", FALSE))
  if (debug_mode) func_start <- Sys.time()
  on.exit(
    {
      if (debug_mode) {
        elapsed <- round(as.numeric(Sys.time() - func_start, units = "secs"), 2)
        msg_status(
          "DEBUG: Total get_semanticscholar_linked() time: {elapsed} s"
        )
      }
    },
    add = TRUE
  )

  # Validate and select the link types from allowed values
  links <- match.arg(links, several.ok = TRUE)

  # Initialize empty data frames for each link type
  results <- list(
    citations = data.frame(),
    references = data.frame(),
    related = data.frame()
  )

  # Return early if no IDs provided
  if (length(ids) == 0) {
    msg_warn("No IDs passed")
    return(results)
  }

  # Validate IDs and warn about any invalid ones
  id_types <- get_article_id_type(ids)
  if (any(is.na(id_types))) {
    msg_warn("Some IDs are not valid:")
    msg_warn(ids[is.na(id_types)])
  }

  # Keep only valid IDs for processing
  ids <- ids[!is.na(id_types)]

  # Return early if no valid IDs remain
  if (length(ids) == 0) {
    msg_warn("No valid IDs passed")
    return(results)
  }

  # Process citations and references if requested
  if (any(c("citations", "references") %in% links)) {
    # Select the appropriate fields based on requested link types
    fields <- c(
      references = "references.externalIds",
      citations = "citations.externalIds"
    )[links] |>
      purrr::discard(is.na)

    msg_status(
      "Fetching {paste(names(fields), collapse = \" and \")} ",
      "for {length(ids)} IDs"
    )

    # Fetch article data including citations/references
    article_data <- get_semanticscholar_articles(
      ids = ids,
      fields = fields
    )

    # Return early if no articles found
    if (nrow(article_data) == 0) {
      msg_warn("No articles found for the given IDs")
      return(results)
    }

    # Process citations if requested
    if ("citations" %in% links) {
      # Extract and format citation data
      results$citations <- article_data |>
        dplyr::select(source_id = ".paperId", ".citations") |>
        tidyr::unnest(".citations") |>
        dplyr::select("source_id", linked_id = ".citations")

      # Remove empty results
      if (all(is.na(results$citations$linked_id))) {
        results$citations <- results$citations[0, ]
      }
    }

    # Process references if requested
    if ("references" %in% links) {
      # Extract and format reference data
      results$references <- article_data |>
        dplyr::select(source_id = ".paperId", ".references") |>
        tidyr::unnest(".references") |>
        dplyr::select("source_id", linked_id = ".references")

      # Remove empty results
      if (all(is.na(results$references$linked_id))) {
        results$references <- results$references[0, ]
      }
    }
  }

  # Process related papers if requested using recommendations API
  if ("related" %in% links) {
    msg_status("Fetching related papers for {length(ids)} IDs")

    endpoint <- "https://api.semanticscholar.org/recommendations/v1/papers"

    # Prepare IDs in correct format for recommendations API
    ids <- s2_prepare_ids(ids)

    # Build request body with paper IDs
    body <- list(
      positivePaperIds = as.list(ids)
    )

    # Make API call to get recommendations
    content <- s2_make_api_call(
      endpoint = endpoint,
      method = "POST",
      body = body,
      query = list(fields = "externalIds")
    )

    # Extract recommended papers
    results$related <- content$recommendedPapers

    # Process recommendations if any found
    if (nrow(results$related) == 0) {
      results$related <- data.frame()
    } else {
      # Transform recommendations into source-target pairs
      results$related <- results$related |>
        nrow() |>
        seq_len() |>
        purrr::map(\(x) {
          paper <- content$recommendedPapers[x, ]

          # Create data frame linking source to recommended paper
          data.frame(
            source_id = ids |> stringr::str_remove("^DOI:"),
            linked_id = s2_buld_id(paper),
            stringsAsFactors = FALSE
          )
        }) |>
        dplyr::bind_rows()
    }
  }

  # Clean up results by removing any duplicate entries
  results <- purrr::map(results, distinct)

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
  ) |>
    tolower()
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
#' @return A data frame containing the processed paper metadata with a set of
#'   sanitized columns at the beginning, identified by a dot prefix.
#'
s2_process_response <- function(data) {
  # Handle nested data if present
  if ("data" %in% names(data)) data <- data$data

  # Return early if no data
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }

  # Process each paper row-by-row to build the final tibble
  purrr::map(seq_len(nrow(data)), \(i) {
    paper_data <- data[i, ]

    # Create the .ids data frame for the current paper
    ids_df <- data.frame(
      doi = paper_data$externalIds$DOI[[1]] %||% NA_character_,
      pmid = paper_data$externalIds$PubMed[[1]] %||% NA_character_,
      pmcid = paper_data$externalIds$PubMedCentral[[1]] %||% NA_character_,
      arxiv = paper_data$externalIds$ArXiv[[1]] %||% NA_character_,
      corpusid = paper_data$externalIds$CorpusId[[1]] %||% NA_character_,
      semanticscholar = paper_data$paperId %||% NA_character_,
      stringsAsFactors = FALSE
    )

    # Process authors for the current paper
    authors_str <- if (
      "authors" %in% names(paper_data) && !is.null(paper_data$authors[[1]])
    ) {
      paper_data$authors[[1]]$name |>
        parse_authors(to_string = TRUE)
    } else {
      NA_character_
    }

    # Process references
    references_vec <- if (
      "references" %in%
        names(paper_data) &&
        !is.null(paper_data$references[[1]])
    ) {
      s2_buld_id(paper_data$references[[1]])
    } else {
      NA_character_
    }

    # Process citations
    citations_vec <- if (
      "citations" %in% names(paper_data) && !is.null(paper_data$citations[[1]])
    ) {
      s2_buld_id(paper_data$citations[[1]])
    } else {
      NA_character_
    }

    # Construct the final tibble for this row
    result <- dplyr::tibble(
      .paperId = paper_data$paperId %||% NA_character_,
      .url = purrr::pluck(paper_data, "openAccessPdf", 1, "url") %||%
        paste0(
          "https://www.semanticscholar.org/paper/",
          paper_data$paperId %||% ""
        ),
      .title = paper_data$title %||% NA_character_,
      .abstract = (paper_data$abstract %||% NA_character_) |>
        stringr::str_remove_all("<[^>]+>"),
      .authors = authors_str,
      .year = suppressWarnings(as.integer(paper_data$year %||% NA_integer_)),
      .journal = paper_data$venue %||% NA_character_,
      .pubtype = if (
        "publicationTypes" %in%
          names(paper_data) &&
          !is.null(paper_data$publicationTypes[[1]])
      ) {
        paste(paper_data$publicationTypes[[1]], collapse = ", ")
      } else {
        NA_character_
      },
      .is_open_access = paper_data$isOpenAccess %||% NA,
      .references = I(list(references_vec)),
      .citations = I(list(citations_vec)),
      .related = I(list(NA_character_)), # Not available from this endpoint
      .ids = I(list(ids_df)),
      .api = "semantic_scholar",
      # Add scalar IDs for merging
      doi = ids_df$doi,
      pmid = ids_df$pmid,
      pmcid = ids_df$pmcid
    )

    # Generate record name and select final columns
    result$.record_name <- generate_record_name(result)
    dplyr::select(
      result,
      .record_name,
      dplyr::starts_with("."),
      dplyr::everything()
    )
  }) |>
    dplyr::bind_rows()
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
    endpoint,
    "POST",
    body = body,
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
#' @param as_req Logical indicating whether to return the request object instead of the response
#'
#' @return The parsed JSON response from the API
#'
#' @keywords internal
s2_make_api_call <- function(
  endpoint,
  method,
  body = NULL,
  query = NULL,
  as_req = FALSE
) {
  # Create base request with endpoint
  req <- httr2::request(endpoint) |>
    # Set the method
    httr2::req_method(method) |>
    # Add the API key, will be ignored if NULL
    httr2::req_headers(
      "x-api-key" = getOption("bibliobutler.semanticscholar_key")
    ) |>
    # Add retry functionality with exponential backoff
    httr2::req_retry(
      max_tries = 5,
      backoff = \(i) max(rpois(1, 2 * 2^i), 1),
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

  if (getOption("bibliobutler.dev_mode")) {
    msg_status("Requesting url: {req$url}")
  }

  if (as_req) {
    return(req)
  }

  # Perform request and parse JSON response
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  return(resp)
}
