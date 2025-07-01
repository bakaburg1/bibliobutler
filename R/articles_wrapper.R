#' Retrieve Articles from Multiple Bibliographic APIs
#'
#' This high-level helper wraps the package‐level functions
#' `get_crossref_articles()`, `get_openalex_articles()`,
#' `get_pubmed_articles()`, and `get_semanticscholar_articles()`.
#' It offers a unified interface that accepts the most common arguments and
#' returns a de-duplicated data frame combining the results from all selected
#' sources.
#'
#' @param ids Optional character vector of article identifiers (DOI, PMID,
#'   PMCID, OpenAlex ID, or Semantic Scholar ID). Only one of `ids` or `query`
#'   must be supplied.
#' @param query Optional character string with a free-text search query. Only
#'   one of `ids` or `query` must be supplied.
#' @param year_filter Publication year or range (e.g. `"2020"`,
#'   `"2018-2020"`, `"2019-"`, `"-2015"`). See the individual API helpers for
#'   additional details.
#' @param filters A named list of additional filters. Not every API supports
#'   custom filters; unsupported entries are ignored with a warning.
#' @param fields Character vector of fields to request. Unsupported fields for
#'   a given backend are silently ignored by that backend.
#' @param per_page Integer indicating the requested page size. The effective
#'   limit is capped internally by each backend (Crossref 1000, OpenAlex 200,
#'   PubMed 10000, Semantic Scholar 100).
#' @param max_results Maximum number of total results to retrieve per backend.
#' @param include_raw Logical. When `TRUE`, the raw XML returned by PubMed is
#'   kept in a `raw_xml` list column for records coming from that source.
#' @param sources Character vector specifying which back-ends to query.
#'   Any combination of `c("crossref", "openalex", "pubmed",
#'   "semanticscholar")` is allowed. Defaults to all.
#'
#' @return A data frame with the union of results returned by the selected
#'   back-ends. Duplicate records (identified primarily through the DOI, then
#'   PMID, then OpenAlex ID) are merged. The column `.source` stores, for each
#'   record, the vector of contributing back-ends.
#'
#' @details
#' When merging duplicate records, for each column the first non-missing and
#' non-empty value is retained. For list-columns the union of unique elements is
#' returned. If conflicting scalar values are present the value from the first
#' encountered backend (in the order supplied by `sources`) wins.
#'
#' Parallel execution is performed with `safe_mirai_map()`. If no `mirai`
#' daemons are running the function falls back to sequential evaluation.
#'
#' @examples
#' \dontrun{
#' # Fetch one article by DOI across all APIs
#' get_articles(ids = "10.1037/0003-066X.59.1.29")
#'
#' # Search by query string
#' get_articles(query = "machine learning", year_filter = "2020")
#'
#' # Limit to Crossref and OpenAlex only
#' get_articles(query = "deep learning", sources = c("crossref", "openalex"))
#' }
#'
#'
#' @export
get_articles <- function(
  ids = NULL,
  query = NULL,
  year_filter = NULL,
  filters = NULL,
  fields = NULL,
  per_page = 100,
  max_results = Inf,
  include_raw = FALSE,
  sources = c("crossref", "openalex", "pubmed", "semanticscholar")
) {
  # Validate input -----------------------------------------------------------
  if (!is.null(ids) && !is.null(query)) {
    stop("Use only one of `ids` or `query` as arguments.")
  }
  if (is.null(ids) && is.null(query)) {
    stop("Either `ids` or `query` must be provided.")
  }

  # Ensure canonical source names
  sources <- match.arg(
    tolower(sources),
    choices = c("crossref", "openalex", "pubmed", "semanticscholar"),
    several.ok = TRUE
  )

  # Build task list ---------------------------------------------------------
  api_tasks <- list()

  if ("crossref" %in% sources) {
    api_tasks$crossref <- function() {
      ids_cf <- convert_article_id(
        ids,
        to = "doi",
        keep_failed_conversions = FALSE
      )

      get_crossref_articles(
        ids = ids_cf,
        query = query,
        year_filter = year_filter,
        filters = filters,
        fields = fields,
        per_page = per_page,
        max_results = max_results
      ) |>
        dplyr::mutate(.source = rep(list("crossref"), dplyr::n()))
    }
  }

  if ("openalex" %in% sources) {
    api_tasks$openalex <- function() {
      get_openalex_articles(
        ids = ids,
        query = query,
        year_filter = year_filter,
        filters = filters,
        fields = fields,
        per_page = min(per_page, 200),
        max_results = max_results
      ) |>
        dplyr::mutate(.source = rep(list("openalex"), dplyr::n()))
    }
  }

  if ("pubmed" %in% sources) {
    api_tasks$pubmed <- function() {
      ids_pm <- convert_article_id(
        ids,
        to = "pmid",
        keep_failed_conversions = FALSE
      )

      get_pubmed_articles(
        ids = ids_pm,
        query = query,
        include_raw = include_raw,
        max_results = max_results,
        per_page = min(per_page, 10000)
      ) |>
        dplyr::mutate(.source = rep(list("pubmed"), dplyr::n()))
    }
  }

  if ("semanticscholar" %in% sources) {
    api_tasks$semanticscholar <- function() {
      get_semanticscholar_articles(
        ids = ids,
        query = query,
        year_filter = year_filter,
        filters = filters,
        fields = fields,
        per_page = min(per_page, 100),
        max_results = max_results
      ) |>
        dplyr::mutate(.source = rep(list("semanticscholar"), dplyr::n()))
    }
  }

  # Execute tasks (parallel if workers available) ---------------------------
  task_names <- names(api_tasks)

  results_list <- safe_mirai_map(
    task_names,
    \(api_name, ...) {
      api_tasks[[api_name]]()
    },
    api_tasks = api_tasks
  )

  # Collect asynchronous results when using mirai workers
  if (length(results_list) && inherits(results_list[[1]], "mirai")) {
    results_list <- results_list[mirai::.progress]
  }

  # Keep only successful data frames with at least one row and standardise types
  results_list <- purrr::keep(results_list, ~ is.data.frame(.x) && nrow(.x) > 0)

  if (length(results_list) == 0) {
    msg_warn("No results returned by the selected sources.")
    return(data.frame())
  }

  standardise_df <- function(df) {
    # Ensure .year integer
    if (".year" %in% names(df)) {
      df$.year <- suppressWarnings(as.integer(df$.year))
    }

    # Ensure .ids is a list-column where each element is a data.frame
    if (".ids" %in% names(df)) {
      if (!is.list(df$.ids) || is.data.frame(df$.ids)) {
        # if it's a data.frame column replicate rows into list-column
        if (is.data.frame(df$.ids)) {
          tmp <- df$.ids
          df$.ids <- vector("list", nrow(df))
          for (i in seq_len(nrow(df))) df$.ids[[i]] <- tmp[i, , drop = FALSE]
        } else {
          df$.ids <- lapply(
            df$.ids,
            function(x) if (is.null(x)) data.frame() else x
          )
        }
      }
    } else {
      df$.ids <- vector("list", nrow(df))
    }

    # Coerce .authors to character string
    if (".authors" %in% names(df)) {
      if (is.list(df$.authors)) {
        df$.authors <- purrr::map_chr(df$.authors, function(x) {
          if (is.null(x) || all(is.na(x))) return(NA_character_)
          paste0(as.character(x), collapse = "; ")
        })
      }
    }

    df
  }

  results_list <- purrr::map(results_list, standardise_df)

  combined_df <- dplyr::bind_rows(results_list)

  get_id_from_ids <- function(ids_list, id_name) {
    purrr::map_chr(
      ids_list,
      ~ purrr::pluck(.x, id_name, 1, .default = NA_character_)
    )
  }

  # Create deduplication key by prioritizing DOI (lowercase), then PMID,
  # then Semantic Scholar paper ID, then record name as fallback
  dedup_key <- dplyr::coalesce(
    tolower(get_id_from_ids(combined_df$.ids, "doi")),
    get_id_from_ids(combined_df$.ids, "pmid"),
    if (".paperId" %in% names(combined_df)) combined_df$.paperId else NULL,
    if (".record_name" %in% names(combined_df)) combined_df$.record_name else
      NULL
  )

  combined_df$.__key__ <- dedup_key

  # Group by key and summarise ---------------------------------------------
  summarised_df <- combined_df |>
    dplyr::summarise(
      # Consolidate .ids by unlisting twice, binding, and keeping distinct rows
      .ids = list(
        dplyr::bind_rows(.data$.ids) |>
          dplyr::distinct() |>
          dplyr::select(dplyr::where(~ any(!is.na(.x))))
      ),
      # Handle list-columns that need unlisting and uniquing
      dplyr::across(
        any_of(c(
          ".source",
          ".references",
          ".citations",
          ".related",
          ".pubtype",
          ".api"
        )),
        ~ list(unique(unlist(.x, use.names = FALSE))),
        .names = "{col}"
      ),
      # For all other columns (that are not list columns or the key), merge them
      # by taking the first non-NA value.
      dplyr::across(
        !any_of(c(
          ".source",
          ".references",
          ".citations",
          ".related",
          ".pubtype",
          ".api",
          ".ids",
          ".__key__",
          "pmid",
          "pmcid",
          "openalex",
          "semanticscholar"
        )),
        ~ dplyr::first(na.omit(.x)),
        .names = "{col}"
      ),
      .by = ".__key__"
    )

  summarised_df$.__key__ <- NULL

  summarised_df$.record_name <- generate_record_name(summarised_df)

  # Keep only dotted columns and columns shared across all back-ends.

  # Identify dotted columns (those starting with a dot)
  dotted_cols <- grep("^\\.", names(summarised_df), value = TRUE)

  # Identify non-dotted columns that are present in every individual back-end
  if (length(results_list) > 0) {
    get_non_dotted <- function(df) names(df)[!grepl("^\\.", names(df))]
    shared_cols <- Reduce(intersect, lapply(results_list, get_non_dotted))
  } else {
    shared_cols <- character(0)
  }

  # Columns to retain (unique)
  keep_cols <- intersect(
    unique(c(dotted_cols, shared_cols, "doi")),
    names(summarised_df)
  )
  summarised_df <- summarised_df[, keep_cols, drop = FALSE]

  # Rename dotted columns by stripping the leading dot
  new_names <- sub("^\\.", "", names(summarised_df))
  # If stripping the dot creates duplicate names, keep the first occurrence
  dup_idx <- duplicated(new_names)
  if (any(dup_idx)) {
    summarised_df <- summarised_df[, !dup_idx, drop = FALSE]
    new_names <- new_names[!dup_idx]
  }
  names(summarised_df) <- new_names

  # Define final column order and select them
  final_col_order <- c(
    "record_name",
    "paperId",
    "doi",
    "title",
    "abstract",
    "authors",
    "year",
    "journal",
    "pubtype",
    "url",
    "is_open_access",
    "references",
    "citations",
    "related",
    "ids",
    "source"
  )

  # Reorder columns and implicitly drop any not in the final list (like 'api')
  summarised_df |>
    dplyr::select(dplyr::any_of(final_col_order))
}
