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
  if (!is.null(ids) && !is.null(query)) {
    stop("Use only one of `ids` or `query` as arguments.")
  }
  # Process year filter into Crossref filter parameters (from-pub-date and until-pub-date)
  if (!is.null(year_filter)) {
    yr_list <- cr_parse_year_filter(year_filter)
    # Merge year filter into filters list (overriding any year filters supplied)
    if (is.null(filters)) filters <- list()
    if (!is.null(yr_list$from)) {
      filters[["from-pub-date"]] <- yr_list$from
    }
    if (!is.null(yr_list$to)) {
      filters[["until-pub-date"]] <- yr_list$to
    }
  }

  # Build the select (fields) parameter (if needed)
  default_fields <- c("DOI", "title", "abstract", "author", "issued",
                      "container-title", "type", "reference")
  fields <- unique(fields %||% default_fields)

  # Crossref API supports the "select" parameter to limit fields, but note that
  # not all fields are supported. We append as a comma‐separated list.
  select_str <- paste(fields, collapse = ",")

  # Base URL for Crossref API
  base_url <- "https://api.crossref.org/works"

  # If DOIs are provided, fetch each work individually
  if (!is.null(ids)) {
    valid_ids <- cr_prepare_ids(ids)
    # Fetch works individually (could be parallelized with mirai::mirai_map if
    # desired)
    results_list <- purrr::map(valid_ids, function(doi) {
      # URL-encode the DOI to safely insert into the URL
      url <- sprintf("%s/%s", base_url, utils::URLencode(doi, reserved = TRUE))
      msg_status("Fetching Crossref record for DOI: {doi}")
      resp <- cr_make_api_call(url)
      if (is.null(resp$message)) {
        msg_warn("No data returned for DOI: {doi}")
        return(NULL)
      }
      # Process the single work response
      cr_process_response(resp)
    })
    results <- dplyr::bind_rows(results_list)
    # Trim to max_results if necessary
    results <- head(results, max_results)
    msg_success("Fetched {nrow(results)} Crossref records by DOI.")
    return(results)
  }

  # Else, perform a search query
  # Build query parameters
  query_params <- list(
    query = query,
    select = select_str,
    mailto = getOption("bibliobutler.crossref_email", "your_email@example.com")
  )

  # Rows is for results per page but also limits the number of results
  query_params$rows <- min(1000, max_results)

  # Add cursor for deep paging
  query_params$cursor <- "*"

  # Incorporate additional filters if provided.
  if (!is.null(filters)) {
    # Convert the filters list to a comma-separated string of key:value pairs.
    filter_vec <- purrr::imap_chr(filters, function(val, key) {
      paste0(key, ":", val)
    })
    query_params$filter <- paste(filter_vec, collapse = ",")
  }

  # First call to get total results count
  msg_status("Querying Crossref for articles matching: {query}")
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
  first_page_results <- cr_process_response(resp)
  results_pages[[current_page]] <- first_page_results
  results_count <- results_count + nrow(first_page_results)

  # Continue fetching pages while we have a next cursor and haven't reached
  # max_results
  while (!is.null(resp$message$`next-cursor`) &&
         results_count < max_results &&
         length(resp$message$items) > 0) {

    current_page <- current_page + 1
    query_params$cursor <- resp$message$`next-cursor`

    msg_status("Fetching page {current_page} with cursor")
    resp <- cr_make_api_call(base_url, query = query_params)

    # Process page results
    page_results <- cr_process_response(resp)
    results_pages[[current_page]] <- page_results
    results_count <- results_count + nrow(page_results)
  }

  results <- dplyr::bind_rows(results_pages)
  results <- head(results, max_results)
  msg_success("Fetched {nrow(results)} Crossref records by query.")

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
  links <- match.arg(links, choices = c("references"), several.ok = TRUE)

  if (any(!links %in% "references")) {
    msg_warn("Crossref API supports only retrieval of references. Other link types are not available.")
  }

  out <- list(
    references = data.frame()
  )

  msg_status("Fetching Crossref metadata for {length(ids)} DOIs")
  works_df <- get_crossref_articles(ids = ids, fields = c("DOI", "reference"))

  if (nrow(works_df) == 0) {
    msg_error("No matching works found in Crossref for these IDs.")
    return(out)
  }

  # Process references for each work
  refs <- purrr::map2(works_df$.ids, works_df$.references, function(id_info, refs_raw) {
    source_id <- id_info$DOI
    if (is.null(refs_raw) || length(refs_raw) == 0) {
      return(NULL)
    }
    # Each reference may have a DOI; if so, return it.
    df <- purrr::map_dfr(refs_raw, function(ref) {
      if (!is.null(ref$DOI)) {
        data.frame(source_id = source_id, linked_id = ref$DOI, stringsAsFactors = FALSE)
      } else {
        NULL
      }
    })
    df
  })

  out$references <- dplyr::bind_rows(refs)
  msg_success("Retrieved references for {length(unique(out$references$source_id))} works.")
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
    headers = NULL
) {
  # Get contact email - use package user's email if configured
  user_mail <- getOption("bibliobutler.crossref_email")

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

  resp <- req |> httr2::req_perform() |> httr2::resp_body_json(simplifyVector = TRUE)

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
    to   <- paste0(x, "-12-31")
  } else if (grepl("^\\d{4}-\\d{4}$", x)) {
    parts <- strsplit(x, "-")[[1]]
    from <- paste0(parts[1], "-01-01")
    to   <- paste0(parts[2], "-12-31")
  } else if (grepl("^\\d{4}-$", x)) {
    yr <- sub("-$", "", x)
    from <- paste0(yr, "-01-01")
    to   <- NULL
  } else if (grepl("^-\\d{4}$", x)) {
    yr <- sub("^-", "", x)
    from <- NULL
    to   <- paste0(yr, "-12-31")
  } else {
    warning("Could not parse year_filter '", x, "'. Ignoring.", call. = FALSE)
    from <- NULL
    to   <- NULL
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
  valid_ids <- ids |>
    as.character() |>
    setdiff(NA_character_) |>
    purrr::keep(nzchar)

  if (length(valid_ids) == 0) {
    stop("No valid IDs provided.")
  }

  valid_ids
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

  browser()

  # Extract items based on response type
  items <- if ("items" %in% names(msg_content)) {
    msg_content$items
  } else {
    list(msg_content)
  }

  # Process each work
  if (length(items) > 0) {
    purrr::map(items, cr_process_work) |>
      bind_rows()
  } else {
    data.frame()
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
  browser()
  # Extract basic metadata
  paperId <- (work$DOI %||% NA_character_) |> tolower()

  title <- work$title %||% NA_character_

  abstract <- (work$abstract %||% NA_character_) |>
    stringr::str_remove_all("</?jats:.*?>")

  # Process authors - work$author is a data.frame
  authors <- if (!rlang::is_empty(work$author)) {
    # Parse authors into standardized format
    parse_authors(
      paste(work$author$given, work$author$family),
      to_string = TRUE)
  } else {
    NA_character_
  }

  # Extract year from published-print or published-online
  year <- (work$`published-print` %||% work$`published-online`) |>
    purrr::pluck("date-parts", 1, 1, .default = NA_character_)

  # Extract journal/container title
  journal <- work$`container-title` %||% NA_character_

  # Extract publication type
  pubtype <- work$type %||% NA_character_

  # Extract URL
  url <- purrr::pluck(
    work, "resource", "primary", "URL", .default = NA_character_)

  # Not easy to check
  is_open_access <- NA_character_

  # Process references - work$reference is already a data.frame
  refs <- if (!rlang::is_empty(work$reference)) {
    work$reference |>
      dplyr::filter(!is.na(.data$DOI)) |>
      dplyr::select("DOI")
  } else {
    data.frame(DOI = character(0))
  }

  raw_fields <- work |>
    lapply(\(el) if (is.atomic(el)) el else I(list(el))) |>
    do.call(what = "data.frame")

  browser()

  # Create standardized output with dotted fields
  result <- data.frame(
    .paperId = paperId,
    .title = title,
    .abstract = abstract,
    .authors = authors,
    .year = year,
    .journal = journal,
    .is_open_access = is_open_access,
    .url = url,
    .pubtype = pubtype,
    .api = "crossref",
    .ids = I(list(data.frame(DOI = paperId))),
    .references = I(list(refs)),
    .citations = I(list(data.frame())),  # Crossref doesn't provide citation data
    .related = I(list(data.frame())),     # Crossref doesn't provide related works
    raw_fields
  )

  # Generate record name using the utility function
  result$.record_name <- generate_record_name(result)

  result |> select(".record_name", starts_with("."), everything())
}
