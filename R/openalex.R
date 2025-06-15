#' Retrieve Articles from OpenAlex API (Direct httr2 Implementation)
#'
#' This function provides a direct implementation of OpenAlex API access using
#' httr2, offering better performance through parallel HTTP requests and more
#' control over API interactions. It follows the same interface patterns as
#' other API functions in the package.
#'
#' @param ids A character vector of identifiers (DOI, PMID, PMC ID, or OpenAlex
#'   IDs). OpenAlex IDs can be provided as full URLs (e.g.,
#'   `"https://openalex.org/W2741809807"`) or as short IDs (e.g.,
#'   `"W2741809807"`). DOIs should be provided without the "doi:" prefix. PMIDs
#'   should be numeric strings. PMC IDs should include the "PMC" prefix.
#' @param query A character string to search for in titles/abstracts/fulltext.
#'   Only one of `ids` or `query` can be non-NULL.
#' @param year_filter Publication year or range (e.g., `"2019"`, `"2016-2020"`,
#'   `"2010-"`, `"-2015"`). If provided, it overrides any year filter in the
#'   `filters` list.
#' @param filters A named list of additional filters to pass to OpenAlex. For
#'   example, `filters = list(is_oa = TRUE, type = "journal-article")`. See
#'   OpenAlex API documentation for available filters.
#' @param fields A character vector of fields to return. If `NULL`, default
#'   fields are used (id, doi, title, abstract_inverted_index, authorships,
#'   publication_year, publication_date, primary_location, locations, type,
#'   type_crossref, open_access, cited_by_count, referenced_works,
#'   related_works, ids).
#' @param per_page Number of results per page (default 200, max 200).
#' @param max_results Maximum number of total results to return (default `Inf`).
#'   If your query matches more records than `max_results`, the rest are
#'   omitted.
#'
#' @return A data frame of article data from OpenAlex with standardized
#'   columns following the package conventions:
#'   \itemize{
#'     \item `.record_name` - Unique record identifier (e.g., "openalex_1")
#'     \item `.paperId` - OpenAlex work ID (without URL prefix)
#'     \item `.url` - Open access URL if available
#'     \item `.title` - Article title
#'     \item `.abstract` - Article abstract (reconstructed from inverted index)
#'     \item `.authors` - Formatted author names (parsed and standardized)
#'     \item `.year` - Publication year
#'     \item `.journal` - Journal/venue name from primary location
#'     \item `.pubtype` - Publication type (e.g., "journal-article")
#'     \item `.is_open_access` - Logical indicating open access status
#'     \item `.references` - List column of referenced work IDs
#'     \item `.related` - List column of related work IDs
#'     \item `.api` - Source API identifier ("openalex")
#'     \item `.ids` - List column of data frames containing all available IDs
#'   }
#'
#' @examples \dontrun{
#' # Get by specific DOIs
#' get_openalex_articles(ids =
#'   c("10.1371/journal.pone.0266781", "10.1371/journal.pone.0267149"))
#'
#' # Get by OpenAlex IDs (full URL or short form)
#' get_openalex_articles(
#'   ids = c("W2741809807", "https://openalex.org/W123456")
#' )
#'
#' # Get by PMIDs
#' get_openalex_articles(ids = c("12345678", "87654321"))
#'
#' # Search for works with "machine learning" in title/abstract
#' get_openalex_articles(query = "machine learning")
#'
#' # Filtering by is_oa and a year range
#' get_openalex_articles(
#'   query = "machine learning",
#'   year_filter = "2021-2022",
#'   filters = list(is_oa = TRUE)
#' )
#' }
#'
#' @export
get_openalex_articles <- function(
  ids = NULL,
  query = NULL,
  year_filter = NULL,
  filters = NULL,
  fields = NULL,
  per_page = 200,
  max_results = Inf
) {
  # Input validation
  if (!is.null(ids) && !is.null(query)) {
    stop("Use only one of `ids` or `query` as arguments.")
  }
  if (is.null(ids) && is.null(query)) {
    stop("Either `ids` or `query` must be provided.")
  }

  # Prepare default fields
  default_fields <- c(
    "id",
    "doi",
    "title",
    "abstract_inverted_index",
    "authorships",
    "publication_year",
    "publication_date",
    "primary_location",
    "locations",
    "type",
    "type_crossref",
    "open_access",
    "cited_by_count",
    "referenced_works",
    "related_works",
    "ids"
  )

  fields <- fields %||% default_fields
  select_param <- paste(fields, collapse = ",")

  # Parse year filter if provided
  if (!is.null(year_filter)) {
    year_params <- oa_parse_year_filter(year_filter)
    if (is.null(filters)) filters <- list()
    if (!is.null(year_params$from)) filters$from_publication_date <- year_params$from
    if (!is.null(year_params$to)) filters$to_publication_date <- year_params$to
  }

  # Process IDs or query
  if (!is.null(ids)) {
    # Handle ID-based requests
    results <- oa_fetch_by_ids(ids, select_param, filters, per_page, max_results)
  } else {
    # Handle search-based requests
    results <- oa_fetch_by_query(query, select_param, filters, per_page, max_results)
  }

  # Process and standardize the response
  if (getOption("bibliobutler.dev_mode", FALSE)) {
    msg_status("🔍 DEBUG: Raw results before processing: {nrow(results)} rows")
  }
  output <- oa_process_response(results)
  if (getOption("bibliobutler.dev_mode", FALSE)) {
    msg_status("🔍 DEBUG: Processed results: {nrow(output)} rows")
  }

  if (nrow(output) == 0) {
    msg_warn("No OpenAlex results found for the given input.")
  } else {
    msg_success("Fetched {nrow(output)} results from OpenAlex")
  }

  output
}

#' Get linked articles (references, citations, or related) from OpenAlex
#'
#' This function retrieves linked articles (references, citations, or related
#' works) for given article IDs using the OpenAlex API. It uses the new
#' httr2-based implementation for better performance and reliability.
#'
#' @param ids A character vector of identifiers (DOI, PMID, PMC ID, or OpenAlex
#'   IDs). OpenAlex IDs can be provided as full URLs or short IDs. See
#'   `get_openalex_articles()` for details on supported ID formats.
#' @param links A character vector specifying which types of linked articles to
#'   retrieve. Any combination of `c("citations", "references", "related")`.
#'   Default is all three types.
#' @param max_results Maximum number of results to return for citations
#'   (references and related works are limited by what's available in the source
#'   articles).
#'
#' @return A named list containing up to three data frames: `references`,
#'   `citations`, `related`. Each data frame has columns:
#'   \itemize{
#'     \item `source_id` - The OpenAlex ID of the queried article
#'       (without URL prefix)
#'     \item `linked_id` - The OpenAlex ID of the linked article
#'       (without URL prefix)
#'   }
#'
#'   Note: References and related works are extracted directly from the source
#'   articles' metadata. Citations are found by querying for works that cite
#'   the input articles.
#'
#' @examples
#' \dontrun{
#' # Get all types of linked articles for a single DOI
#' get_openalex_linked("10.1371/journal.pone.0266781")
#' 
#' # Get only citations and references for multiple articles
#' get_openalex_linked(
#'   c("10.1371/journal.pone.0266781", "10.1371/journal.pone.0267149"),
#'   links = c("citations", "references")
#' )
#' 
#' # Get only references with OpenAlex IDs
#' get_openalex_linked(
#'   c("W2741809807", "W123456789"),
#'   links = "references"
#' )
#' }
#' @export
get_openalex_linked <- function(
  ids,
  links = c("citations", "references", "related"),
  max_results = Inf
) {
  links <- match.arg(links, several.ok = TRUE)

  # Helper: given source_ids and a list column of linked IDs, return a
  # data.frame
  build_link_df <- function(source_ids, linked_list) {
    out <- vector("list", length(source_ids))
    for (i in seq_along(source_ids)) {
      if (length(linked_list[[i]]) == 0 || all(is.na(linked_list[[i]]))) {
        out[[i]] <- NULL
      } else {
        out[[i]] <- data.frame(
          source_id = rep(source_ids[i], length(linked_list[[i]])),
          linked_id = linked_list[[i]],
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(out)) do.call(rbind, out) else data.frame()
  }

  # Initialize empty data frames
  out <- list(
    citations = data.frame(),
    references = data.frame(),
    related = data.frame()
  )

  msg_status("Fetching general article data for {length(ids)} IDs")

  # Fetch the works themselves to get references and related works
  works_df <- get_openalex_articles(
    ids = ids,
    fields = c("id", "doi", "referenced_works", "related_works")
  )

  if (nrow(works_df) == 0) {
    msg_error("No matching works found in OpenAlex for these IDs.")
    return(out)
  }

  # Vector of OpenAlex IDs (already without URL prefix)
  source_ids_vec <- works_df$.paperId

  # Process references and related links using lightweight helper
  if ("references" %in% links && ".references" %in% names(works_df)) {
    out$references <- build_link_df(source_ids_vec, works_df$.references)
  }
  if ("related" %in% links && ".related" %in% names(works_df)) {
    out$related <- build_link_df(source_ids_vec, works_df$.related)
  }

  # Handle citations separately - need to query works that cite these IDs
  if ("citations" %in% links) {
    openalex_ids <- source_ids_vec

    # Create filter for works that cite any of our IDs
    cites_filter <- paste(openalex_ids, collapse = "|")

    msg_status("Fetching citations for {length(openalex_ids)} IDs")

    citations_df <- oa_fetch_by_query(
      query = NULL,
      select_param = "id,referenced_works",
      filters = list(cites = cites_filter),
      per_page = 200,
      max_results = max_results
    )

    if (nrow(citations_df) > 0) {
      citations_processed <- oa_process_response(citations_df)

      # Build citation edges without tidyverse
      cit_out <- list()
      for (i in seq_len(nrow(citations_processed))) {
        refs <- citations_processed$.references[[i]]
        if (length(refs)) {
          # keep only refs that are in our original set
          refs <- refs[refs %in% openalex_ids]
          if (length(refs)) {
            cit_out[[length(cit_out) + 1]] <- data.frame(
              source_id = refs,
              linked_id = rep(citations_processed$.paperId[i], length(refs)),
              stringsAsFactors = FALSE
            )
          }
        }
      }
      if (length(cit_out)) out$citations <- do.call(rbind, cit_out)
    }
  }

  out
}

#' Fetch OpenAlex works by IDs using parallel HTTP requests
#'
#' This function handles ID-based queries to the OpenAlex API with intelligent
#' batching and parallel processing. It automatically groups IDs by type (DOI,
#' PMID, PMC ID, OpenAlex ID) and respects the API's 50-ID limit per filter type
#' by creating multiple batches when necessary.
#'
#' @param ids Character vector of prepared IDs (output from oa_prepare_ids)
#' @param select_param Comma-separated string of fields to select from the API
#' @param filters List of additional filters to apply to the query
#' @param per_page Number of results per page (max 200)
#' @param max_results Maximum number of results to return
#'
#' @return Data frame of raw results from the OpenAlex API
#'
#' @keywords internal
oa_fetch_by_ids <- function(ids, select_param, filters, per_page, max_results) {
  # Prepare and validate IDs
  prepared_ids <- oa_prepare_ids(ids)
  
  msg_info("Total results: {length(prepared_ids)} for ID query")
  
  if (length(prepared_ids) > max_results) {
    msg_info("(retrieving first {max_results} results)")
    prepared_ids <- head(prepared_ids, max_results)
  }

  # Build helper to convert a named list of ids-by-type into query params
  make_query <- function(type_vec) {
    filter_parts <- purrr::imap_chr(type_vec, ~ paste0(.y, ":", paste(.x, collapse = "|")))
    list(
      filter   = paste(filter_parts, collapse = ","),
      select   = select_param,
      per_page = min(per_page, 200)
    )
  }

  # Split IDs by type (doi / pmid / openalex / pmcid)
  ids_by_type <- split(prepared_ids, purrr::map_chr(prepared_ids, ~ .x$type))
  ids_by_type <- purrr::map(ids_by_type, ~ purrr::map_chr(.x, `[[`, "id"))

  # Check if every id vector is <=50 → single call path
  if (all(purrr::map_int(ids_by_type, ~ length(.x)) <= 50)) {
    msg_status("Using single OpenAlex call (<=50 ids per attribute)")
    query_params <- make_query(ids_by_type)
    resp <- oa_make_api_call("https://api.openalex.org/works", "GET", query = query_params)
    out_df <- if (!is.null(resp$results)) {
      as.data.frame(resp$results, stringsAsFactors = FALSE)
    } else {
      data.frame()
    }
    return(out_df)
  }

  # Otherwise, build batches respecting the 50-id rule per attribute  
  # Determine number of chunks needed per type
  chunks_per_type <- purrr::map_int(ids_by_type, ~ ceiling(length(.x)/50))
  max_chunks <- max(chunks_per_type)

  batch_list <- vector("list", max_chunks)
  for (i in seq_len(max_chunks)) {
    slice_by_type <- purrr::map(ids_by_type, \(vec) {
      start <- (i-1)*50 + 1
      end   <- min(i*50, length(vec))
      if (start > length(vec)) return(character(0))
      vec[start:end]
    })
    # remove empty
    slice_by_type <- slice_by_type[lengths(slice_by_type) > 0]
    if (length(slice_by_type)) {
      batch_list[[i]] <- make_query(slice_by_type)
    }
  }
  # Drop NULL (if some chunks had no ids)
  batch_list <- batch_list[!purrr::map_lgl(batch_list, ~ is.null(.x))]

  msg_status("Fetching {length(batch_list)} OpenAlex ID batches in parallel…")

  # Create request objects
  reqs <- purrr::map(batch_list, ~ oa_make_api_call(
    "https://api.openalex.org/works",
    "GET",
    query = .x,
    as_req = TRUE
  ))

  # Perform in parallel (if more than one batch) or sequential
  if (length(reqs) == 1) {
    resps <- list(httr2::req_perform(reqs[[1]]))
  } else {
    resps <- httr2::req_perform_parallel(reqs, on_error = "continue")
  }

  # Parse responses
  all_results <- purrr::map(resps, \(resp) {
    if (inherits(resp, "httr2_response")) {
      dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      if (!is.null(dat$results)) {
        if (is.data.frame(dat$results)) {
          return(dat$results)
        } else {
          return(as.data.frame(dat$results, stringsAsFactors = FALSE))
        }
      }
    }
    data.frame()
  })

  combined <- dplyr::bind_rows(all_results)
  combined
}

#' Fetch OpenAlex works by search query
#'
#' This function handles search-based queries to the OpenAlex API with automatic
#' pagination. For small queries (≤3 pages), it uses sequential processing to
#' avoid overhead. For larger queries, it uses parallel HTTP requests for
#' better performance.
#'
#' @param query Search query string (can be NULL for filter-only queries)
#' @param select_param Comma-separated string of fields to select from the API
#' @param filters List of additional filters to apply to the query
#' @param per_page Number of results per page (max 200)
#' @param max_results Maximum number of results to return
#'
#' @return Data frame of raw results from the OpenAlex API
#'
#' @keywords internal
oa_fetch_by_query <- function(query, select_param, filters, per_page, max_results) {
  # First, get the total count
  count_params <- list(
    per_page = 1
  )
  
  # Add search query if provided
  if (!is.null(query)) {
    count_params$search <- query
  }
  
  # Add filters to count query
  if (!is.null(filters)) {
    filter_strings <- purrr::imap_chr(filters, ~ paste0(.y, ":", .x))
    count_params$filter <- paste(filter_strings, collapse = ",")
  }

  count_resp <- oa_make_api_call(
    "https://api.openalex.org/works",
    "GET",
    query = count_params
  )

  total_count <- count_resp$meta$count %||% 0
  msg_info("Total results: {total_count} for search query")

  if (total_count == 0) {
    return(data.frame())
  }

  if (total_count > max_results) {
    msg_info("(retrieving first {max_results} results)")
    total_count <- max_results
  }

  # Calculate number of pages needed
  pages_needed <- ceiling(total_count / per_page)
  msg_status("Fetching {pages_needed} pages of OpenAlex results")

  # For small queries, use sequential processing to avoid overhead
  if (pages_needed <= 3) {
    # Sequential processing for small queries
    all_results <- list()
    for (page in seq_len(pages_needed)) {
      query_params <- list(
        select = select_param,
        per_page = per_page,
        page = page
      )
      
      # Add search query if provided
      if (!is.null(query)) {
        query_params$search <- query
      }
      
      # Add filters
      if (!is.null(filters)) {
        filter_strings <- purrr::imap_chr(filters, ~ paste0(.y, ":", .x))
        query_params$filter <- paste(filter_strings, collapse = ",")
      }

      resp_data <- oa_make_api_call(
        "https://api.openalex.org/works",
        "GET",
        query = query_params
      )
      
      if (!is.null(resp_data$results)) {
        all_results[[page]] <- as.data.frame(resp_data$results, stringsAsFactors = FALSE)
      }
    }
  } else {
    # Parallel processing for large queries
    reqs <- purrr::map(seq_len(pages_needed), \(page) {
      query_params <- list(
        select = select_param,
        per_page = per_page,
        page = page
      )
      
      # Add search query if provided
      if (!is.null(query)) {
        query_params$search <- query
      }
      
      # Add filters
      if (!is.null(filters)) {
        filter_strings <- purrr::imap_chr(filters, ~ paste0(.y, ":", .x))
        query_params$filter <- paste(filter_strings, collapse = ",")
      }

      oa_make_api_call(
        "https://api.openalex.org/works",
        "GET",
        query = query_params,
        as_req = TRUE
      )
    })

    # Perform requests in parallel
    resps <- httr2::req_perform_parallel(reqs, on_error = "continue")

    # Process responses
    all_results <- purrr::map(resps, \(resp) {
      if (inherits(resp, "httr2_response")) {
        json_resp <- httr2::resp_body_json(resp, simplifyVector = TRUE)
        if (!is.null(json_resp$results)) {
          # Check if results is already a data frame (single result) or needs conversion
          if (is.data.frame(json_resp$results)) {
            return(json_resp$results)
          } else {
            # Multiple results - convert to data frame
            return(as.data.frame(json_resp$results, stringsAsFactors = FALSE))
          }
        }
      } else {
        msg_warn("Failed to fetch a page: {conditionMessage(resp)}")
      }
      return(data.frame())
    })
  }

  # Combine all results and limit to max_results
  results <- dplyr::bind_rows(all_results)
  head(results, max_results)
}

#' Make OpenAlex API call using httr2
#'
#' This function creates and executes HTTP requests to the OpenAlex API using
#' httr2. It includes automatic retry logic, proper user agent configuration,
#' and support for API keys. Can return either the response data or the
#' request object for use in parallel processing.
#'
#' @param url API endpoint URL
#' @param method HTTP method (default "GET")
#' @param query Named list of query parameters to include in the URL
#' @param body Request body (for POST requests)
#' @param as_req If TRUE, return httr2 request object instead of performing request
#'
#' @return If `as_req = FALSE`, returns parsed JSON response. If `as_req = TRUE`,
#'   returns httr2 request object for use with `httr2::req_perform_parallel()`.
#'   
#' @details The function automatically includes:
#'   - User agent string with package name and email (if configured)
#'   - API key from `getOption("bibliobutler.openalex_key")` if available
#'   - 30-second timeout
#'   - Retry logic (3 attempts with exponential backoff)
#'   - Debug logging when `getOption("bibliobutler.dev_mode")` is TRUE
#'   
#' @keywords internal
oa_make_api_call <- function(url, method = "GET", query = NULL, body = NULL, as_req = FALSE) {
  # Get email and API key from options
  email <- getOption("bibliobutler.openalex_email")
  api_key <- getOption("bibliobutler.openalex_key")
  
  # Build user agent string
  user_agent <- "bibliobutler R package"
  if (!is.null(email)) {
    user_agent <- paste0(user_agent, " (mailto:", email, ")")
  }

  # Debug: print request details
  if (getOption("bibliobutler.dev_mode", FALSE)) {
    msg_status("🔍 DEBUG: URL: {url}")
    msg_status("🔍 DEBUG: User-Agent: {user_agent}")
    if (!is.null(query)) {
      msg_status("🔍 DEBUG: Query params: {paste(names(query), query, sep='=', collapse=', ')}")
    }
  }

  # Create base request
  req <- httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_timeout(30)

  # Add API key if available
  if (!is.null(api_key)) {
    req <- req |> httr2::req_url_query(api_key = api_key)
  }

  # Add query parameters
  if (!is.null(query)) {
    req <- req |> httr2::req_url_query(!!!query)
  }

  # Add body for POST requests
  if (method == "POST" && !is.null(body)) {
    req <- req |> 
      httr2::req_method("POST") |>
      httr2::req_body_json(body)
  }

  # Return request object if requested
  if (as_req) {
    return(req)
  }

  # Perform request with retry logic
  resp <- req |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2^.x) |>
    httr2::req_perform()

  # Parse JSON response
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' Prepare and validate IDs for OpenAlex API
#'
#' This function takes a vector of mixed identifier types and formats them
#' for use with the OpenAlex API. It automatically detects ID types (DOI,
#' PMID, PMC ID, OpenAlex ID) and standardizes their format.
#'
#' @param ids Character vector of identifiers in various formats
#'
#' @return List of ID information, where each element contains:
#'   \itemize{
#'     \item `type` - One of "doi", "pmid", "pmcid", or "openalex"
#'     \item `id` - Cleaned and standardized ID string
#'   }
#'   
#' @details ID type detection rules:
#'   - OpenAlex IDs: URLs starting with "https://openalex.org/W" or strings matching "W\\d+"
#'   - DOIs: Strings starting with "10."
#'   - PMIDs: Numeric strings (digits only)
#'   - PMC IDs: Strings matching "PMC\\d+" (case insensitive)
#'   - Other: Treated as DOI by default
#'   
#' @keywords internal
oa_prepare_ids <- function(ids) {
  # Remove NAs and empty strings
  valid_ids <- ids |>
    as.character() |>
    setdiff(c(NA_character_, ""))

  if (length(valid_ids) == 0) {
    stop("All IDs are NA or empty.")
  }

  # Determine ID type and clean each ID
  purrr::map(valid_ids, \(id) {
    # Clean the ID
    clean_id <- trimws(id)
    
    # Determine the type and format
    if (grepl("^https://openalex.org/W", clean_id)) {
      # Full OpenAlex URL
      list(type = "openalex", id = clean_id)
    } else if (grepl("^W\\d+$", clean_id)) {
      # OpenAlex ID without URL
      list(type = "openalex", id = paste0("https://openalex.org/", clean_id))
    } else if (grepl("^10\\.", clean_id)) {
      # DOI
      list(type = "doi", id = clean_id)
    } else if (grepl("^\\d+$", clean_id)) {
      # Likely PMID
      list(type = "pmid", id = clean_id)
    } else if (grepl("^PMC\\d+$", clean_id, ignore.case = TRUE)) {
      # PMC ID
      list(type = "pmcid", id = toupper(clean_id))
    } else {
      # Default to treating as DOI or other external ID
      list(type = "doi", id = clean_id)
    }
  })
}

#' Parse year filter for OpenAlex API
#'
#' This function converts various year filter formats into the date range
#' format expected by the OpenAlex API (YYYY-MM-DD).
#'
#' @param year_filter Year filter string in one of several supported formats
#'
#' @return List with `from` and `to` date strings (YYYY-MM-DD format) or NULL values
#'   
#' @details Supported year filter formats:
#'   - Single year: "2020" → from "2020-01-01" to "2020-12-31"
#'   - Year range: "2018-2020" → from "2018-01-01" to "2020-12-31"
#'   - Open-ended from: "2018-" → from "2018-01-01", no end date
#'   - Open-ended to: "-2020" → no start date, to "2020-12-31"
#'   
#' @keywords internal
oa_parse_year_filter <- function(year_filter) {
  year_filter <- trimws(year_filter)
  
  if (grepl("^\\d{4}$", year_filter)) {
    # Single year: "2020"
    list(
      from = paste0(year_filter, "-01-01"),
      to = paste0(year_filter, "-12-31")
    )
  } else if (grepl("^\\d{4}-\\d{4}$", year_filter)) {
    # Year range: "2018-2020"
    parts <- strsplit(year_filter, "-")[[1]]
    list(
      from = paste0(parts[1], "-01-01"),
      to = paste0(parts[2], "-12-31")
    )
  } else if (grepl("^\\d{4}-$", year_filter)) {
    # Open-ended from: "2018-"
    year <- sub("-$", "", year_filter)
    list(
      from = paste0(year, "-01-01"),
      to = NULL
    )
  } else if (grepl("^-\\d{4}$", year_filter)) {
    # Open-ended to: "-2020"
    year <- sub("^-", "", year_filter)
    list(
      from = NULL,
      to = paste0(year, "-12-31")
    )
  } else {
    warning("Could not parse year_filter '", year_filter, "'. Ignoring.", call. = FALSE)
    list(from = NULL, to = NULL)
  }
}

#' Process OpenAlex API response to standardized format
#'
#' This function transforms raw OpenAlex API response data into the standardized
#' data frame format used throughout the package. It extracts and processes
#' various fields including authors, abstracts, references, and metadata.
#'
#' @param data Raw data frame from OpenAlex API response
#'
#' @return Standardized data frame with package-specific column names and formats:
#'   \itemize{
#'     \item `.record_name` - Sequential record identifiers
#'     \item `.paperId` - OpenAlex work IDs (cleaned of URL prefixes)
#'     \item `.url` - Open access URLs when available
#'     \item `.title` - Article titles
#'     \item `.abstract` - Reconstructed abstracts from inverted index
#'     \item `.authors` - Parsed and formatted author names
#'     \item `.year` - Publication years
#'     \item `.journal` - Journal names from primary location
#'     \item `.pubtype` - Publication types
#'     \item `.is_open_access` - Open access status flags
#'     \item `.references` - List column of referenced work IDs
#'     \item `.related` - List column of related work IDs
#'     \item `.api` - Source API identifier ("openalex")
#'     \item `.ids` - List column of data frames with all available IDs
#'   }
#'   
#' @keywords internal
oa_process_response <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }

  n_rows <- nrow(data)

  # Extract and process authors
  authors_vec <- purrr::map_chr(seq_len(n_rows), \(i) {
    if (!"authorships" %in% names(data) || is.null(data[["authorships"]]) || 
        length(data[["authorships"]]) < i) {
      return(NA_character_)
    }
    auth_data <- data[["authorships"]][[i]]
    
    if (is.null(auth_data) || nrow(auth_data) == 0) {
      return(NA_character_)
    }
    
    # Extract author names
    auth_names <- auth_data$author$display_name %||% character(0)
    
    if (length(auth_names) == 0) {
      return(NA_character_)
    }
    
    # Parse and format author names
    parsed_names <- try(
      parse_authors(auth_names, to_string = TRUE),
      silent = TRUE
    )
    
    if (inherits(parsed_names, "try-error")) {
      # Fallback to simple concatenation
      return(paste(auth_names, collapse = "; "))
    }
    
    parsed_names
  })

  # Extract references and related works
  refs_list <- purrr::map(seq_len(n_rows), \(i) {
    if (!"referenced_works" %in% names(data) || is.null(data[["referenced_works"]]) || 
        length(data[["referenced_works"]]) < i) {
      return(character(0))
    }
    refs <- data[["referenced_works"]][[i]]
    if (is.null(refs)) return(character(0))
    remove_url_from_id(refs)
  })
  
  related_list <- purrr::map(seq_len(n_rows), \(i) {
    if (!"related_works" %in% names(data) || is.null(data[["related_works"]]) || 
        length(data[["related_works"]]) < i) {
      return(character(0))
    }
    related <- data[["related_works"]][[i]]
    if (is.null(related)) return(character(0))
    remove_url_from_id(related)
  })

  # Extract IDs
  ids_list <- purrr::map(seq_len(n_rows), \(i) {
    if (!"ids" %in% names(data) || is.null(data[["ids"]]) || 
        length(data[["ids"]]) < i) {
      return(data.frame())
    }
    ids_data <- data[["ids"]][[i]]
    if (is.null(ids_data)) {
      return(data.frame())
    }
    
    # Convert to data frame and clean URLs
    ids_df <- as.data.frame(ids_data, stringsAsFactors = FALSE)
    ids_df[] <- purrr::map(ids_df, ~ remove_url_from_id(.x))
    ids_df
  })

  # Extract journal/venue information
  journal_vec <- purrr::map_chr(seq_len(n_rows), \(i) {
    if (!"primary_location" %in% names(data) || is.null(data[["primary_location"]]) || 
        length(data[["primary_location"]]) < i) {
      return(NA_character_)
    }
    primary_loc <- data[["primary_location"]][[i]]
    if (is.null(primary_loc) || length(primary_loc) == 0) {
      return(NA_character_)
    }
    
    # Handle different possible structures
    if (is.list(primary_loc) && "source" %in% names(primary_loc)) {
      source_info <- primary_loc[["source"]]
      if (is.list(source_info) && "display_name" %in% names(source_info)) {
        return(source_info[["display_name"]] %||% NA_character_)
      }
    }
    
    return(NA_character_)
  })

  # Extract URLs
  url_vec <- purrr::map_chr(seq_len(n_rows), \(i) {
    if (!"open_access" %in% names(data) || is.null(data[["open_access"]]) || 
        length(data[["open_access"]]) < i) {
      return(NA_character_)
    }
    oa_data <- data[["open_access"]][[i]]
    if (is.list(oa_data) && "oa_url" %in% names(oa_data)) {
      return(oa_data[["oa_url"]] %||% NA_character_)
    }
    return(NA_character_)
  })

  # Extract open access status
  is_oa_vec <- purrr::map_lgl(seq_len(n_rows), \(i) {
    if (!"open_access" %in% names(data) || is.null(data[["open_access"]]) || 
        length(data[["open_access"]]) < i) {
      return(FALSE)
    }
    oa_data <- data[["open_access"]][[i]]
    if (is.list(oa_data) && "is_oa" %in% names(oa_data)) {
      return(oa_data[["is_oa"]] %||% FALSE)
    }
    return(FALSE)
  })

  # Extract abstracts
  abstract_vec <- oa_extract_abstract(data[["abstract_inverted_index"]])
  if (length(abstract_vec) != n_rows) {
    abstract_vec <- rep(NA_character_, n_rows)
  }

  # Build standardized data frame
  result <- data.frame(
    .record_name = paste0("openalex_", seq_len(n_rows)),
    .paperId = remove_url_from_id(data[["id"]] %||% rep(NA_character_, n_rows)),
    .url = url_vec,
    .title = data[["title"]] %||% rep(NA_character_, n_rows),
    .abstract = abstract_vec,
    .authors = authors_vec,
    .year = data[["publication_year"]] %||% rep(NA_integer_, n_rows),
    .journal = journal_vec,
    .pubtype = data[["type"]] %||% rep(NA_character_, n_rows),
    .is_open_access = is_oa_vec,
    .references = I(refs_list),
    .related = I(related_list),
    .api = "openalex",
    .ids = I(ids_list),
    stringsAsFactors = FALSE
  )

  result
}

#' Extract abstract from inverted index
#'
#' OpenAlex stores abstracts as inverted indexes (word -> position mappings)
#' for efficiency. This function reconstructs the original abstract text by
#' sorting words by their positions and concatenating them.
#'
#' @param abstract_inverted_index List of inverted index data from OpenAlex API,
#'   where each element is a named list with words as names and position vectors
#'   as values
#'
#' @return Character vector of reconstructed abstracts, with NA for missing
#'   or invalid abstract data
#'   
#' @details The function handles errors gracefully and falls back to NA when
#'   the inverted index cannot be processed (e.g., malformed data, missing
#'   positions).
#'   
#' @keywords internal
oa_extract_abstract <- function(abstract_inverted_index) {
  if (is.null(abstract_inverted_index)) {
    return(character(0))
  }
  
  purrr::map_chr(abstract_inverted_index, \(inv_index) {
    if (is.null(inv_index) || length(inv_index) == 0) {
      return(NA_character_)
    }
    
    # Convert inverted index back to text
    try({
      # Create word-position pairs
      word_positions <- purrr::imap_dfr(inv_index, ~ data.frame(
        word = .y,
        position = as.integer(.x),
        stringsAsFactors = FALSE
      ))
      
      # Sort by position and reconstruct text
      if (nrow(word_positions) > 0) {
        word_positions <- word_positions[order(word_positions$position), ]
        paste(word_positions$word, collapse = " ")
      } else {
        NA_character_
      }
    }, silent = TRUE) %||% NA_character_
  })
} 