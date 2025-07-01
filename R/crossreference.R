#' Retrieve Articles from Crossref API
#'
#' This function queries the Crossref REST API (https://api.crossref.org/works)
#' to fetch article metadata. You can either supply a vector of DOIs (via `ids`)
#' or perform a free‐text search (via `query`). A year filter (e.g., "2016",
#' "2010-2015", "2018-", or "-2019") and additional filters can also be
#' provided.
#'
#' @param ids A character vector of DOIs.
#' @param query A character string to search in article metadata. Only one of
#'   `ids` or `query` should be non-NULL.
#' @param year_filter A string indicating a publication year or range. Examples:
#'   "2020", "2015-2020", "2010-", "-2018".
#' @param filters A named list of additional filters to pass. For example,
#'   `filters = list(type = "journal-article")` will add
#'   `filter=type:journal-article`.
#' @param fields A character vector of fields to return. If NULL, a set of
#'   default fields is returned.
#' @param per_page Number of results per page (default is 100).
#' @param max_results Maximum number of results to return (default is Inf).
#'
#' @return A data frame of article metadata from Crossref with standardized
#'   columns: `.record_name`, `.paperId`, `.title`, `.abstract`, `.authors`,
#'   `.year`, `.journal`, `.is_open_access`, `.url`, `.pubtype`, `.api`, `.ids`,
#'   `.references`, `.citations`, and `.related`.
#'
#' @examples
#' \dontrun{
#' # Get specific articles by DOI
#' get_crossref_articles(ids = c("10.1037/0003-066X.59.1.29"))
#'
#' # Search for articles about "machine learning" published in 2020
#' get_crossref_articles(query = "machine learning", year_filter = "2020")
#'
#' # Filtering by type and a year range
#' get_crossref_articles(
#'   query = "deep learning",
#'   year_filter = "2018-2020",
#'   filters = list(type = "journal-article")
#' )
#' }
#'
#' @export
get_crossref_articles <- function(
  ids = NULL,
  query = NULL,
  year_filter = NULL,
  filters = NULL,
  fields = NULL,
  per_page = 1000,
  max_results = Inf
) {
  # DEBUG TIMING (overall)
  debug_mode <- isTRUE(getOption("bibliobutler.dev_mode", FALSE))
  if (debug_mode) func_start <- Sys.time()
  on.exit(
    {
      if (debug_mode) {
        elapsed <- round(as.numeric(Sys.time() - func_start, units = "secs"), 2)
        msg_status("DEBUG: Total get_crossref_articles() time: {elapsed} s")
      }
    },
    add = TRUE
  )

  if (getOption("bibliobutler.dev_mode", FALSE)) {
    msg_status("🔍 DEBUG: get_crossref_articles called")
    msg_status(
      "🔍 DEBUG: ids = {if(is.null(ids)) 'NULL' else paste(head(ids, 3), collapse=', ')}"
    )
    msg_status("🔍 DEBUG: query = {if(is.null(query)) 'NULL' else query}")
    msg_status("🔍 DEBUG: max_results = {max_results}")
  }

  if (!is.null(ids) && !is.null(query)) {
    stop("Use only one of `ids` or `query` as arguments.")
  }

  # Removed interactive browser() call used during development.

  # Set empty filters if NULL
  if (is.null(filters)) filters <- list()

  # Process year filter into Crossref filter parameters (from-pub-date and
  # until-pub-date)
  if (!is.null(year_filter)) {
    yr_list <- cr_parse_year_filter(year_filter)
    # Merge year filter into filters list (overriding any year filters supplied)
    if (!is.null(yr_list$from)) {
      filters[["from-pub-date"]] <- yr_list$from
    }
    if (!is.null(yr_list$to)) {
      filters[["until-pub-date"]] <- yr_list$to
    }
  }

  # Build the select (fields) parameter (if needed)
  default_fields <- c(
    "DOI",
    "URL", # direct link to the work
    "title",
    "abstract",
    "author",
    "issued",
    "container-title",
    "type",
    "reference"
  )
  fields <- unique(fields %||% default_fields)

  # Crossref API supports the "select" parameter to limit fields, but note that
  # not all fields are supported. We append as a comma‐separated list.
  select_str <- paste(fields, collapse = ",")

  # Base URL for Crossref API
  base_url <- "https://api.crossref.org/works"

  # If DOIs are provided, fetch each work individually
  if (!is.null(ids)) {
    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status("🔍 DEBUG: Processing {length(ids)} DOIs")
    }

    valid_ids <- cr_prepare_ids(ids)

    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status("🔍 DEBUG: Valid IDs after preparation: {length(valid_ids)}")
      msg_status(
        "🔍 DEBUG: Sample valid IDs: {paste(head(valid_ids, 3), collapse=', ')}"
      )
    }

    # Create a list of request objects
    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status("🔍 DEBUG: Creating request objects...")
    }

    reqs <- purrr::map(valid_ids, function(doi) {
      url <- sprintf("%s/%s", base_url, utils::URLencode(doi, reserved = TRUE))
      if (getOption("bibliobutler.dev_mode", FALSE)) {
        msg_status("🔍 DEBUG: Creating request for DOI: {doi}")
        msg_status("🔍 DEBUG: Request URL: {url}")
      }
      # NOTE: Individual DOI endpoints don't support the select parameter
      # Only pass select_str for search queries, not individual DOI fetches
      cr_make_api_call(url, as_req = TRUE)
    })

    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status("🔍 DEBUG: Created {length(reqs)} request objects")
    }

    msg_status("Fetching {length(reqs)} Crossref records in parallel...")

    # Perform requests in parallel
    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status("🔍 DEBUG: Starting httr2::req_perform_parallel...")
    }

    resps <- httr2::req_perform_parallel(reqs, on_error = "continue")

    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status("🔍 DEBUG: Parallel requests completed")
      msg_status("🔍 DEBUG: Number of responses: {length(resps)}")
      msg_status(
        "🔍 DEBUG: Response types: {paste(sapply(resps, class), collapse=', ')}"
      )
    }

    # Process the responses
    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status("🔍 DEBUG: Processing responses...")
    }

    results_list <- purrr::map(resps, function(resp) {
      if (inherits(resp, "httr2_response")) {
        if (getOption("bibliobutler.dev_mode", FALSE)) {
          msg_status("🔍 DEBUG: Processing successful response")
        }
        # The actual response body is JSON, which needs to be parsed
        json_resp <- httr2::resp_body_json(resp, simplifyVector = FALSE)
        if (is.null(json_resp$message)) {
          # Log or warn about missing data
          doi <- sub(".*/", "", resp$url) #
          msg_warn("No data returned for DOI: {doi}")
          return(NULL)
        }
        result <- cr_process_response(json_resp)
        if (getOption("bibliobutler.dev_mode", FALSE)) {
          msg_status("🔍 DEBUG: Processed response, got {nrow(result)} rows")
        }
        return(result)
      } else {
        # This was an error object
        if (getOption("bibliobutler.dev_mode", FALSE)) {
          msg_status("🔍 DEBUG: Processing error response: {class(resp)}")
        }
        msg_warn("Failed to fetch a DOI: {conditionMessage(resp)}")
        return(NULL)
      }
    })

    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status("🔍 DEBUG: Processed all responses")
      msg_status(
        "🔍 DEBUG: Non-null results: {sum(!sapply(results_list, is.null))}"
      )
    }

    results <- dplyr::bind_rows(results_list)

    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status("🔍 DEBUG: Combined results: {nrow(results)} rows")
    }

    # Trim to max_results if necessary
    results <- head(results, max_results)
    msg_success("Fetched {nrow(results)} Crossref records by DOI.")
    return(results)
  }

  # Else, perform a search query
  if (getOption("bibliobutler.dev_mode", FALSE)) {
    msg_status("🔍 DEBUG: Processing search query: {query}")
  }

  # Build query parameters
  query_params <- list(
    query = query,
    select = select_str,
    mailto = getOption("bibliobutler.crossref_email", "your_email@example.com")
  )

  # Use `per_page` (capped at 1000 by Crossref) to control page size, but do
  # not request more than `max_results`.
  query_params$rows <- min(per_page, 1000L, max_results)

  # Add cursor for deep paging
  query_params$cursor <- "*"

  # Incorporate additional filters only when at least one filter was supplied.
  # An empty `filters` list would otherwise append an empty `filter=` query
  # parameter, which Crossref treats as an invalid request and returns HTTP
  # 400. We therefore add the `filter` parameter **only** when `filters` has
  # length greater than zero.
  if (!is.null(filters) && length(filters) > 0) {
    # Convert the filters list to a comma-separated string of key:value pairs.
    filter_vec <- purrr::imap_chr(filters, function(val, key) {
      paste0(key, ":", val)
    })

    # Only attach the filter parameter if at least one pair was generated.
    if (length(filter_vec) > 0) {
      query_params$filter <- paste(filter_vec, collapse = ",")
    }
  }

  # First call to get total results count
  msg_status("Querying Crossref for articles matching: {query}")

  if (getOption("bibliobutler.dev_mode", FALSE)) {
    msg_status("🔍 DEBUG: Making first API call to get count...")
  }

  resp <- cr_make_api_call(base_url, query = query_params)
  total_results <- resp$message$`total-results`
  msg_info("Total results: {total_results} for query")
  if (total_results > max_results) {
    msg_info("(retrieving first {max_results} results)")
  }

  # Initialize results list with first page
  results_pages <- list()
  results_count <- 0
  current_page <- 1

  # Process first page
  if (getOption("bibliobutler.dev_mode", FALSE)) {
    msg_status("🔍 DEBUG: Processing first page...")
  }

  first_page_results <- cr_process_response(resp)
  results_pages[[current_page]] <- first_page_results
  results_count <- results_count + length(resp$message$items)

  if (getOption("bibliobutler.dev_mode", FALSE)) {
    msg_status(
      "🔍 DEBUG: First page processed: {nrow(first_page_results)} results"
    )
  }

  # Continue fetching pages while we have a next cursor and haven't reached
  # max_results
  while (
    !is.null(resp$message$`next-cursor`) &&
      results_count < max_results &&
      length(resp$message$items) > 0
  ) {
    current_page <- current_page + 1
    query_params$cursor <- resp$message$`next-cursor`

    # Dynamically adjust rows to fetch, ensuring we do not exceed max_results
    remaining_results <- max_results - results_count
    query_params$rows <- min(per_page, 1000L, remaining_results)

    # If no more results are needed, break the loop
    if (query_params$rows <= 0) break

    msg_status("Fetching page {current_page} with cursor")

    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status("🔍 DEBUG: Fetching page {current_page}...")
    }

    resp <- cr_make_api_call(base_url, query = query_params)

    # Process page results
    page_results <- cr_process_response(resp)
    results_pages[[current_page]] <- page_results
    # Increment count by items returned from API, not items successfully
    # processed, to prevent infinite loops if processing fails.
    results_count <- results_count + length(resp$message$items)

    if (getOption("bibliobutler.dev_mode", FALSE)) {
      msg_status(
        "🔍 DEBUG: Page {current_page} processed: {nrow(page_results)} results"
      )
    }
  }

  results <- dplyr::bind_rows(results_pages)
  results <- head(results, max_results)
  msg_success("Fetched {nrow(results)} Crossref records by query.")

  if (getOption("bibliobutler.dev_mode", FALSE)) {
    msg_status("🔍 DEBUG: Final results: {nrow(results)} rows")
  }

  return(results)
}

#' Get Linked Articles (References) from Crossref
#'
#' Crossref metadata includes a list of references for each work (in the
#' "reference" field). This function retrieves the reference links for given
#' DOIs. Note that Crossref does not expose the list of articles that cite a
#' given work (i.e. "citations"), nor does it provide recommendations.
#'
#' @param ids A character vector of DOIs.
#' @param links A character vector specifying which types of linked articles to
#'   retrieve. Only "references" is supported; if other types (e.g. "citations",
#'   "related") are requested, a warning is issued and they are omitted.
#'
#' @return A named list containing a data frame `references` with columns:
#'   - `source_id`: the queried DOI.
#'   - `linked_id`: the DOI (if available) of the referenced work.
#'
#' @examples
#' \dontrun{
#' # Get references for a specific article
#' get_crossref_linked("10.1037/0003-066X.59.1.29")
#'
#' # Get references for multiple DOIs
#' get_crossref_linked(c("10.1037/0003-066X.59.1.29", "10.1371/journal.pone.0266781"),
#'   links = "references")
#' }
#'
#' @export
get_crossref_linked <- function(
  ids,
  links = c("references")
) {
  debug_mode <- isTRUE(getOption("bibliobutler.dev_mode", FALSE))
  if (debug_mode) func_start <- Sys.time()
  on.exit(
    {
      if (debug_mode) {
        elapsed <- round(as.numeric(Sys.time() - func_start, units = "secs"), 2)
        msg_status("DEBUG: Total get_crossref_linked() time: {elapsed} s")
      }
    },
    add = TRUE
  )

  # Check for empty input first
  if (length(ids) == 0) {
    stop("No valid IDs provided.")
  }

  # Try to convert to character and filter out NA values
  ids <- as.character(ids)
  ids <- ids[!is.na(ids)]

  if (length(ids) == 0) {
    stop("No valid IDs provided.")
  }

  links <- match.arg(links, choices = c("references"), several.ok = TRUE)

  if (any(!links %in% "references")) {
    msg_warn(
      "Crossref API supports only retrieval of references. Other link types are not available."
    )
  }

  out <- list(
    references = data.frame(
      source_id = character(0),
      linked_id = character(0),
      stringsAsFactors = FALSE
    )
  )

  # Validate IDs
  valid_ids <- try(cr_prepare_ids(ids), silent = TRUE)

  if (inherits(valid_ids, "try-error") || length(valid_ids) == 0) {
    stop("No valid IDs provided.")
  }

  msg_status("Fetching Crossref metadata for {length(valid_ids)} DOIs")

  # Try to get the articles
  works_df <- tryCatch(
    {
      get_crossref_articles(ids = valid_ids, fields = c("DOI", "reference"))
    },
    error = function(e) {
      msg_error("Error fetching Crossref metadata: {conditionMessage(e)}")
      return(data.frame())
    }
  )

  if (nrow(works_df) == 0) {
    msg_error("No matching works found in Crossref for these IDs.")
    return(out)
  }

  # Process references for each work
  all_refs <- data.frame(
    source_id = character(0),
    linked_id = character(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(works_df))) {
    # Get source ID from the IDs column
    ids_col <- works_df$.ids[[i]]
    source_id <- NA_character_

    if (is.data.frame(ids_col) && "DOI" %in% names(ids_col)) {
      source_id <- ids_col$DOI[1]
    }

    if (is.na(source_id)) {
      next
    }

    # Get references from the references column
    refs_raw <- works_df$.references[[i]]

    if (
      is.null(refs_raw) ||
        !is.data.frame(refs_raw) ||
        nrow(refs_raw) == 0 ||
        !"DOI" %in% names(refs_raw)
    ) {
      next
    }

    # Extract reference DOIs and create a data frame
    ref_dois <- refs_raw$DOI
    ref_dois <- ref_dois[!is.na(ref_dois)]

    if (length(ref_dois) > 0) {
      work_refs <- data.frame(
        source_id = rep(source_id, length(ref_dois)),
        linked_id = ref_dois,
        stringsAsFactors = FALSE
      )
      all_refs <- rbind(all_refs, work_refs)
    }
  }

  out$references <- all_refs
  msg_success(
    "Retrieved references for {length(unique(out$references$source_id))} works."
  )
  return(out)
}

#' Make an API Call to Crossref
#'
#' This function wraps httr2 requests to the Crossref REST API.
#'
#' @param endpoint The full URL for the API request.
#' @param method HTTP method, default "GET".
#' @param query A named list of query parameters.
#' @param headers A named list of additional HTTP headers.
#'
#' @return The parsed JSON response.
cr_make_api_call <- function(
  endpoint,
  method = "GET",
  query = NULL,
  headers = NULL,
  as_req = FALSE
) {
  # Get contact email - use package user's email if configured
  user_mail <- getOption("bibliobutler.crossref_email")

  # Ensure we always have a single, non-empty email for the User-Agent header
  if (is.null(user_mail) || length(user_mail) == 0 || user_mail == "") {
    # fallback placeholder per Crossref guidelines
    user_mail <- "your_email@example.com"
  }

  req <- httr2::request(endpoint) |>
    httr2::req_method(method) |>
    httr2::req_user_agent(
      sprintf(
        "bibliobutler/%s (https://github.com/bakaburg1/bibliobutler; mailto:%s)",
        utils::packageVersion("bibliobutler"),
        user_mail
      )
    ) |>
    httr2::req_retry(
      max_tries = 5,
      backoff = function(i) max(rpois(1, 2 * 2^i), 1),
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(429, 503)
      }
    )

  if (!is.null(query)) {
    req <- req |> httr2::req_url_query(!!!query)
  }
  if (!is.null(headers)) {
    req <- req |> httr2::req_headers(!!!headers)
  }

  if (getOption("bibliobutler.dev_mode", FALSE)) {
    msg_status("Requesting Crossref URL: {req$url}")
  }

  if (as_req) {
    return(req)
  }

  resp <- req |>
    httr2::req_perform() |>
    # Use `simplifyVector = FALSE` to preserve nested list structure. Vector
    # simplification can coerce the `items` element into a data frame, causing
    # a mismatch (number of columns instead of number of works) that led to the
    # observed 8 rows of empty output. Keeping the raw list ensures each work is
    # processed correctly downstream.
    httr2::resp_body_json(simplifyVector = FALSE)

  return(resp)
}

#' Parse a Year Filter String for Crossref
#'
#' Converts a year filter string (e.g., "2016", "2010-2015", "2018-", "-2019")
#' into Crossref filter parameters for publication dates.
#'
#' @param x A character string representing the year or range.
#'
#' @return A list with elements `from` and `to` (dates in "YYYY-MM-DD" format).
cr_parse_year_filter <- function(x) {
  x <- trimws(x)
  if (grepl("^\\d{4}$", x)) {
    from <- paste0(x, "-01-01")
    to <- paste0(x, "-12-31")
  } else if (grepl("^\\d{4}-\\d{4}$", x)) {
    parts <- strsplit(x, "-")[[1]]
    from <- paste0(parts[1], "-01-01")
    to <- paste0(parts[2], "-12-31")
  } else if (grepl("^\\d{4}-$", x)) {
    yr <- sub("-$", "", x)
    from <- paste0(yr, "-01-01")
    to <- NULL
  } else if (grepl("^-\\d{4}$", x)) {
    yr <- sub("^-", "", x)
    from <- NULL
    to <- paste0(yr, "-12-31")
  } else {
    warning("Could not parse year_filter '", x, "'. Ignoring.", call. = FALSE)
    from <- NULL
    to <- NULL
  }
  list(from = from, to = to)
}

#' Prepare DOIs for Crossref API Requests
#'
#' This function takes a character vector of DOIs, removes any NA or empty values,
#' and returns the cleaned vector.
#'
#' @param ids A character vector of DOIs.
#'
#' @return A character vector of valid DOIs.
cr_prepare_ids <- function(ids) {
  # Handle non-character inputs
  ids <- as.character(ids)

  # Remove NA values and empty strings
  valid_ids <- ids[!is.na(ids) & nzchar(ids)]

  if (length(valid_ids) == 0) {
    stop("No valid IDs provided.")
  }

  # Basic DOI format validation
  # This is a simple check - real DOIs typically start with "10." followed by numbers/dots
  valid_doi_pattern <- "^10\\.[0-9]+/.*"
  is_valid_doi <- grepl(valid_doi_pattern, valid_ids)

  if (!any(is_valid_doi)) {
    stop("No valid IDs provided. DOIs should typically start with '10.'")
  }

  valid_ids[is_valid_doi]
}

#' Process Crossref API Response into a Standard Data Frame
#'
#' This function converts the JSON response from Crossref into a standardized
#' data frame with columns for DOI, title, abstract, authors, year, journal,
#' publication type, and a list-column for additional IDs.
#'
#' @param resp The JSON response from Crossref.
#'
#' @return A data frame with standardized columns.
cr_process_response <- function(resp) {
  # Check for required fields
  if (!"message" %in% names(resp)) {
    return(data.frame())
  }

  msg_content <- resp$message

  # Extract items based on response type
  items <- if ("items" %in% names(msg_content)) {
    msg_content$items
  } else {
    list(msg_content)
  }

  # If automatic simplification turned `items` into a data.frame, convert each
  # row back to a list so that downstream processing treats every work
  # independently. This ensures that `length(items)` reflects the number of
  # works (rows) rather than the number of columns.
  if (is.data.frame(items)) {
    items <- lapply(seq_len(nrow(items)), function(i) {
      # Convert the i-th row to a list while keeping list-columns intact.
      row <- items[i, , drop = FALSE]
      # Preserve list columns (they become list columns already), just coerce
      # to list at top level.
      as.list(row)
    })
  }

  # Process each work - use tryCatch to handle errors gracefully
  results <- list()

  if (length(items) > 0) {
    for (i in seq_along(items)) {
      item <- items[[i]]
      result <- tryCatch(
        {
          cr_process_work(item)
        },
        error = function(e) {
          # If processing fails, return NULL so we can filter it out
          msg_warn("Error processing item {i}: {conditionMessage(e)}")
          NULL
        }
      )
      if (!is.null(result)) {
        results[[length(results) + 1]] <- result
      }
    }
    if (length(results) > 0) {
      return(dplyr::bind_rows(results))
    } else {
      return(data.frame())
    }
  } else {
    return(data.frame())
  }
}

#' Process a Single Crossref Work into a Data Frame Row
#'
#' Extracts key metadata from a single work record as returned by Crossref.
#'
#' @param work A list representing a Crossref work.
#'
#' @return A one-row data frame with standardized columns.
cr_process_work <- function(work) {
  # Safely extract DOI
  paperId <- tolower(work$DOI %||% NA_character_)

  # Title handling
  title <- NA_character_
  if (!is.null(work$title) && length(work$title) > 0) {
    title <- work$title[[1]]
  }

  # Abstract handling
  abstract <- NA_character_
  if (!is.null(work$abstract)) {
    abstract <- as.character(work$abstract) |>
      stringr::str_remove_all("<[^>]+>")
  }

  # Process authors
  authors <- NA_character_
  if (!is.null(work$author)) {
    # Each element is a list that should contain `given` and `family`.
    names_vec <- purrr::map_chr(work$author, function(a) {
      paste(a$given %||% "", a$family %||% "")
    })
    authors <- parse_authors(names_vec, to_string = TRUE)
  }

  # Extract year
  year <- NA_integer_
  date_parts <- work$issued$`date-parts`[[1]]
  if (!is.null(date_parts) && length(date_parts) > 0) {
    year <- suppressWarnings(as.integer(date_parts[[1]]))
  }

  # Extract journal/container title
  journal <- NA_character_
  if (!is.null(work$`container-title`) && length(work$`container-title`) > 0) {
    journal <- work$`container-title`[[1]]
  }

  # Extract publication type
  pubtype <- work$type %||% NA_character_

  # Extract URL
  url <- work$URL %||% NA_character_

  # Open access is a logical NA for consistency
  is_open_access <- NA

  # Process references
  references <- if (!is.null(work$reference)) {
    purrr::map_chr(work$reference, "DOI", .default = NA_character_)
  } else {
    NA_character_
  }
  references <- references[!is.na(references)]
  if (length(references) == 0) {
    references <- NA_character_
  }

  # Create ID dataframe with consistent lowercase 'doi'
  ids_df <- data.frame(doi = paperId, stringsAsFactors = FALSE)

  # Create standardized output tibble
  result <- dplyr::tibble(
    .paperId = paperId,
    .url = url,
    .title = title,
    .abstract = abstract,
    .authors = authors,
    .year = year,
    .journal = journal,
    .pubtype = pubtype,
    .is_open_access = is_open_access,
    .references = I(list(references)),
    .citations = I(list(NA_character_)),
    .related = I(list(NA_character_)),
    .ids = I(list(ids_df)),
    .api = "crossref",
    # Add scalar id for merging
    doi = paperId
  )

  # Generate record name and reorder
  result$.record_name <- generate_record_name(result)
  dplyr::select(result, .record_name, dplyr::everything())
}
