#' Retrieve Articles from OpenAlex (via openalexR)
#'
#' This function provides an interface similar to
#' \code{get_semanticscholar_articles()}, but calls the
#' \code{openalexR::oa_fetch()} function internally to query the OpenAlex
#' database of scholarly works. You can fetch works by ID (DOI, PMID, or
#' OpenAlex ID), or by a search query. Additional OpenAlex filters and a year
#' range can also be specified.
#'
#' @param ids A character vector of identifiers (DOI, PMID, or OpenAlex IDs).
#'   Example of OpenAlex IDs: \code{"https://openalex.org/W2741809807"}.
#' @param query A character string to match in titles/abstracts (similar to a
#'   free-text search). Only one of \code{ids} or \code{query} can be non-NULL.
#' @param year_filter Publication year or range (e.g., \code{"2019"},
#'   \code{"2016-2020"}, \code{"2010-"}, \code{"-2015"}). If provided, it
#'   overrides any year filter in the \code{filters} list. Internally converted
#'   to OpenAlex date filters.
#' @param filters A named list of additional filters to pass to
#'   \code{openalexR}. For example, \code{filters = list(is_oa = TRUE, type =
#'   "journal-article")}. See \code{\link[openalexR]{oa_query}} for details.
#' @param fields A character vector of fields to return. In \code{openalexR},
#'   you would typically pass these via \code{options = list(select = fields)}.
#'   If \code{NULL}, all default columns from \code{openalexR} are retrieved.
#' @param per_page Approximate number of results fetched per page (default 100).
#'   Passed to \code{openalexR::oa_fetch(..., per_page = ...)}.
#' @param max_results Maximum number of total results to return (default
#'   \code{Inf}). If your query matches more records than \code{max_results},
#'   the rest are omitted.
#'
#' @return A data frame of article data from OpenAlex, with a set of sanitized
#'   columns at the beginning, identified by a dot prefix.
#'
#' @examples \dontrun{
#' # Get by specific DOIs
#' get_openalex_articles(ids =
#'   c("10.1371/journal.pone.0266781", "10.1371/journal.pone.0267149"))
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
    ids         = NULL,
    query       = NULL,
    year_filter = NULL,
    filters     = NULL,
    fields      = NULL,
    per_page    = 200,
    max_results = Inf
) {

  # Ensure only one of `ids` or `query`
  if (!is.null(ids) && !is.null(query)) {
    stop("Use only one of `ids` or `query` as arguments.")
  }

  # If user gave a year_filter, incorporate it into filters, overriding any
  # existing year
  if (!is.null(year_filter)) {
    # parse year_filter into from_year and to_year
    yr_list <- oa_parse_year_filter(year_filter)
    # The user might also have some existing filters they want to combine
    if (is.null(filters)) {
      filters <- list()
    }
    # Overwrite from/to publication date
    filters$from_publication_date <- yr_list$from
    filters$to_publication_date   <- yr_list$to
  }

  # Prepare the default fields
  default_fields <- c(
    "id", "doi", "title", "abstract_inverted_index",
    "authorships", "publication_year",
    "primary_location", "type", "type_crossref", "ids"
  )

  fields <- (fields %||% default_fields) |>
    unique() |>
    paste(collapse = ",")

  # Prepare the arguments for oa_fetch
  # We want to handle IDs separately from queries
  oa_args <- list(
    entity    = "works",
    abstract  = TRUE,      # fetch abstracts by default
    per_page  = per_page,
    paging = "page",
    mailto = getOption("bibliobutler.openalex_email"),
    api_key = getOption("bibliobutler.openalex_key"),
    verbose = getOption("bibliobutler.dev_mode", FALSE),
    # openalexR uses `options = list(select=...)` to specify fields
    options   = if (!is.null(fields)) list(select = fields) else NULL
  )

  # Additional filters can be passed as `filter` in openalexR
  if (!is.null(filters)) {
    # We assume user is passing a list suitable for openalexR
    oa_args <- c(oa_args, filters)
  }

  # If we have IDs, parse them (DOI, PMID, or full OpenAlex ID)
  if (!is.null(ids)) {
    # Convert user IDs to the appropriate openalexR arguments
    parsed_ids <- oa_prepare_ids(ids)
    # Based on type, place into the correct parameters for `oa_fetch` If
    # multiple types appear, we need to do multiple calls or raise a warning For
    # simplicity, we can attempt to do them in a single call, because openalexR
    # supports e.g. doi=vec_of_dois, pmid=vec_of_pmids, id=vec_of_openalex_ids
    # But we must separate them out by type:
    if (length(parsed_ids$doi) > 0) {
      oa_args$doi <- parsed_ids$doi
    }
    if (length(parsed_ids$pmid) > 0) {
      oa_args$pmid <- parsed_ids$pmid
    }
    if (length(parsed_ids$openalex) > 0) {
      oa_args$identifier <- parsed_ids$openalex
    }

    # If user had a big mixture of IDs, some might be missing.
    # openalexR typically returns fewer rows if some IDs didn't match.

  } else {
    # handle the search query. openalexR offers a `search` parameter for
    # free-text searching:
    oa_args$search <- query

  }

  # Inspect how many results there are for this query
  request_info <- withCallingHandlers( # Wrap to restyle messages
    do.call(openalexR::oa_fetch, c(oa_args, count_only = TRUE)),
    message = \(m) {
      msg_status(m$message)
      invokeRestart("muffleMessage")
    }
  ) |>
    as.data.frame()

  msg_info("Total results: {request_info$count} for query")

  if (request_info$count > max_results) {
    msg_info("(retrieving first {max_results} results)")
  }

  # Get the corresponding number of pages to fetch
  pages <- ceiling(min(request_info$count, max_results, na.rm = T) / per_page)

  msg_status("Fetching {pages} pages of OpenAlex results")

  # Fetch the results in parallel
  with(mirai::daemons(
    min(get_parallel_process(), 9),
    .compute = "bibliobutler.openalex_articles"
  ), {
    results_pages <- mirai::mirai_map(
      seq_len(pages), \(page) {
        fetch_with_retry <- purrr::insistently(
          rate = purrr::rate_backoff(pause_base = 3, max_times = 4),
          \(args) {
            results <- try(do.call(openalexR::oa_fetch, args), silent = TRUE)
            if (!is.data.frame(results) || inherits(results, "try-error")) {
              stop("OpenAlex fetch failed")
            }
            results
          }
        )

        fetch_with_retry(c(oa_args, pages = page))
      },
      oa_args = oa_args,
      .compute = "bibliobutler.openalex_articles"
    )[mirai::.progress]
  })

  no_result_pages <- purrr::map_lgl(
    results_pages, \(x){
      rlang::is_empty(x) ||
        (is.character(x) && grepl("error", x, ignore.case = TRUE))
    }) |> sum()

  if (no_result_pages > 0) {
    stop(stringr::str_glue("Failed to fetch all OpenAlex results. ",
      "({no_result_pages} with no results)"))
  }

  # Aggregate results
  results <- try(bind_rows(results_pages), silent = TRUE)

  if (inherits(results, "try-error")) {
    msg_error("Failed to bind OpenAlex results")
    if (getOption("bibliobutler.dev_mode", FALSE)) {
      browser()
    } else stop()
  }

  # Trim to max_results
  results <- head(results, max_results)

  msg_success("Fetched {nrow(results)} results from OpenAlex")

  # Convert openalexR output to standardized format
  output <- oa_process_response(results)

  # If no results, warn
  if (nrow(output) == 0) {
    msg_warn("No OpenAlex results found for the given input.")
  }

  output
}

#' Get linked articles (references, citations, or related) from OpenAlex
#'
#' @param ids A character vector of identifiers (DOI, PMID, or OpenAlex IDs).
#' @param links A character vector specifying which types of linked articles to
#'   retrieve. Any combination of \code{c("citations", "references",
#'   "related")}.
#'
#' @return A named list containing up to three data frames: \code{references},
#'   \code{citations}, \code{related}. Each data frame has columns:
#'   \itemize{
#'     \item \code{source_id} - the queried ID
#'     \item \code{linked_id} - an OpenAlex ID (or other ID if convertible)
#'   }
#'
#' @examples
#' \dontrun{
#' get_openalex_linked("10.1371/journal.pone.0266781")
#' get_openalex_linked(
#'   c("10.1371/journal.pone.0266781", "10.1371/journal.pone.0267149"),
#'   links = c("citations", "references")
#' )
#' }
#' @export
get_openalex_linked <- function(
    ids, links = c("citations", "references", "related")
) {

  links <- match.arg(links, several.ok = TRUE)

  # Initialize empty data frames
  out <- list(
    citations  = data.frame(),
    references = data.frame(),
    related    = data.frame()
  )

  fields <- c(
    references = "referenced_works", related = "related_works",
    citations = "cited_by_api_url")

  msg_status("Fetching general article data for {length(ids)} IDs")

  # Fetch the works themselves, in order to parse references & related
  works_df <- get_openalex_articles(
    ids = ids, fields = c("ids", fields[links]))

  if (nrow(works_df) == 0) {
    msg_error("No matching works found in OpenAlex for these IDs.")
    return(out)
  }

  # Process references and related links
  for (link_type in c("references", "related")) {
    link_col <- paste0(".", link_type)
    if (link_type %in% links) {
      # Extract relevant columns and unnest
      df <- works_df |>
        select('.ids', any_of(link_col)) |>
        mutate(
          # Prefer DOI followed by OpenAlex ID
          source_id = with(.data$.ids, coalesce(
            doi, openalex)),
          .ids = NULL
        ) |>
        tidyr::unnest(link_col, keep_empty = TRUE) |>
        select("source_id", linked_id = any_of(link_col)) |>
        filter(!is.na(.data$linked_id))

      out[[link_type]] <- df
    }
  }

  # Citations are not in the works list, but we need to fetch them with an
  # ad-hoc query
  if ("citations" %in% links) {
    citations_ids <- works_df$.ids$openalex

    id_translator <- coalesce(works_df$.ids$doi, works_df$.ids$openalex) |>
      purrr::set_names(works_df$.ids$openalex)

    # Can only fetch 100 citations at a time
    batches <- split(citations_ids, ceiling(seq_along(citations_ids) / 100))

    msg_status("Fetching citations for {length(citations_ids)} IDs")

    results <- with(
      mirai::daemons(
        # OpenAlex allows only 10 operations per second
        min(get_parallel_process(), 10),
        .compute = "bibliobutler.openalex_linked"), {

          mirai::mirai_map(batches, \(batch) {

            citations <- bibliobutler::get_openalex_articles(
              filters = list(cites = paste(batch, collapse = "|")),
              fields = c("id", "referenced_works"))

            # Since the openalexR citation result lacks the source_id, we need
            # to infer it from the references of each citing ID
            oa_ids <- citations$referenced_works |>
              purrr::map_chr(\(x) {
                # See which of the ids is cited by the set of citing papers
                intersect(remove_url_from_id(x), citations_ids) |>
                  paste(collapse = ",")
              })

            data.frame(
              # The OpenAlex ID is then mapped to the relative DOI
              source_id = id_translator[oa_ids],
              linked_id = citations$.paperId
            )},
            id_translator = id_translator,
            citations_ids = citations_ids,
            .compute = "bibliobutler.openalex_linked"
          )[mirai::.progress]
        })

    out$citations <- bind_rows(results)
  }

  out
}

#' Prepare IDs for openalexR
#'
#' Given a character vector of IDs (DOI, PMID, or full OpenAlex IDs), return a
#' list with only the provided ID type.
#'
#' @param ids character vector
#'
#' @return A list with one element named after the ID type provided (`doi`,
#'   `pmid`, or `openalex`).
#'
oa_prepare_ids <- function(ids) {

  # Convert to character and remove NAs/empty strings
  valid_ids <- ids |>
    as.character() |>
    setdiff(NA_character_)

  # Return early if no valid IDs are passed
  if (length(valid_ids) == 0) {
    stop("All IDs are NA or empty.")
  }

  # Convert or classify IDs using case_match
  id_types <- dplyr::case_match(
    get_article_id_type(valid_ids),
    "doi" ~ "doi",
    "pmid" ~ "pmid",
    "openalex" ~ "openalex",
    NA ~ NA_character_,
    .default = "convert"
  )

  # Convert any IDs marked for conversion
  to_convert <- which(id_types == "convert")

  if (length(to_convert) > 0) {
    converted <- convert_article_id(valid_ids[to_convert], to = "doi")
    valid_ids[to_convert] <- converted
    id_types[to_convert] <- "doi"
  }

  # Remove any NAs or NFs (ie, could not be converted)
  valid_mask <- !is.na(id_types)
  valid_ids <- valid_ids[valid_mask]
  id_types <- id_types[valid_mask]

  if (length(valid_ids) == 0) {
    stop("No valid IDs after conversion.")
  }

  if (n_distinct(id_types) > 1) {
    stop("OpenAlex can accept only one ID type at once")
  }

  # Get the single ID type
  id_type <- unique(id_types)

  # Return list with single ID type
  list(valid_ids) |> purrr::set_names(id_type)
}

#' Attempt to find an OpenAlex ID URL for a given ID (which might be a DOI, PMID, etc.)
#' Return `NA` if we can't resolve it.
oa_find_id <- function(x) {
  # If it's already an OpenAlex URL, return it
  if (grepl("^https://openalex.org/", x)) {
    return(x)
  }

  # Otherwise look up
  tmp <- get_openalex_articles(ids = x, max_results = 1)
  if (nrow(tmp) == 0) {
    return(NA_character_)
  }
  # We can store the OpenAlex ID in the .ids sub-data-frame or
  # we can store it in the "paperId" if it wasn't a known doi.
  # By default, let's check .ids$openalex if available:
  row_ids <- tmp$.ids[[1]]
  if ("openalex" %in% names(row_ids)) {
    return(row_ids[["openalex"]])
  } else {
    # fallback
    return(NA_character_)
  }
}

#' Convert a user-supplied year range like "2016-2020" into from/to Publication Date
#'
#' If user says "2016-2020", we map to from_publication_date="2016-01-01",
#' to_publication_date="2020-12-31".
#' If user says "2010-", we map to from_publication_date="2010-01-01",
#' to_publication_date= Inf (represented as NULL or "9999-12-31" depending on your preference).
#' If user says "-2015", we map from=NULL, to="2015-12-31".
#'
oa_parse_year_filter <- function(x) {
  # remove whitespace
  x <- trimws(x)
  # possible forms: "YYYY", "YYYY-YYYY", "YYYY-", "-YYYY"
  if (grepl("^\\d{4}$", x)) {
    # single year
    from <- paste0(x, "-01-01")
    to   <- paste0(x, "-12-31")
  } else if (grepl("^\\d{4}-\\d{4}$", x)) {
    # year-year
    parts <- strsplit(x, "-")[[1]]
    from  <- paste0(parts[1], "-01-01")
    to    <- paste0(parts[2], "-12-31")
  } else if (grepl("^\\d{4}-$", x)) {
    # e.g. 2010-
    yr <- sub("-$", "", x)
    from <- paste0(yr, "-01-01")
    to   <- NULL
  } else if (grepl("^-\\d{4}$", x)) {
    # e.g. -2015
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

#' Process the openalexR result to a standard format
#'
#' Similar to \code{s2_process_response()}, we want columns like: paperId,
#' title, abstract, authors, year, journal, pubtype, .references (list),
#' .citations (empty by default), .related (list), .api = "openalex", .ids
#' (data.frame with openalex, doi, pmid, etc.)
#'
#' @param data The tibble/data.frame returned from
#'   \code{openalexR::oa_fetch(...)} with a set of sanitized columns at the
#'   beginning, identified by a dot prefix.
#'
#' @return A data frame with standardized columns.
#'
oa_process_response <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())  # empty
  }

  # Transform each row's authors
  authors_vec <- purrr::map(seq_len(nrow(data)), \(i) {
    names <- data[["authorships"]][[i]]$display_name
    if (is.null(names)) {
      return(data.frame())
    }
    names |> parse_authors()
  })

  # References are in "referenced_works", a character vector of OpenAlex IDs.
  # Related are in "related_works". We store them as a list-column
  refs_list <- data[["referenced_works"]] |>
    purrr::map(remove_url_from_id)
  if (rlang::is_empty(refs_list)) {
    refs_list <- rep(list(character()), nrow(data))
  }

  rel_list <- data[["related_works"]] |>
    purrr::map(remove_url_from_id)
  if (rlang::is_empty(rel_list)) {
    rel_list <- rep(list(character()), nrow(data))
  }

  # Build .ids for each row:
  # OpenalexR lumps multiple IDs in a list of named vectors
  # We want to store them as a data.frame in each row
  all_ids <- purrr::map(data[["ids"]], \(x) {
    if (is.null(x) || length(x) == 0) {
      return(data.frame())
    }

    as.list(x) |> as.data.frame()
  }) |> bind_rows() |>
    # Remove the URL prefixes and keep only the ID
    mutate(
      across(everything(), remove_url_from_id)
    )

  if (nrow(all_ids) == 0) all_ids <- NULL

  # Produce the final data frame
  data <- data |> mutate(
    .paperId     = col_get("id") |>
      remove_url_from_id(),
    .url         = col_get("pdf_url"),
    .title       = col_get("title"),
    .abstract    = col_get("abstract"),
    .authors     = authors_vec,
    .year        = col_get("publication_year"),
    .journal     = col_get("source_display_name"),
    .pubtype     = col_get("type"),
    .is_open_access = col_get("is_oa"),
    .references = refs_list,
    #.citations  = rep(list(NA), nrow(data)),  # empty by default
    .related    = rel_list,
    .api        = "openalex",
    .ids        = all_ids,
    .before = everything()
  )

  data |> dplyr::mutate(
    .record_name = generate_record_name(data),
    .before = everything()
  )
}
