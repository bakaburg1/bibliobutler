#' Collect references from a given source
#'
#' This function collects bibliographic references from a specified source.
#'
#' @param source A character string specifying the source of references.
#' @param query A character string containing the search query.
#' @param limit An integer specifying the maximum number of references to collect.
#'
#' @return A data frame containing collected references.
#' @export
#'
#' @examples
#' collect_references("pubmed", "artificial intelligence", 10)
collect_references <- function(source, query, limit = 100) {
  # Placeholder implementation
  message("Collecting references from ", source, " for query: ", query)
  message("Limit: ", limit, " references")

  # TODO: Implement actual reference collection logic

  data.frame(
    title = character(0),
    authors = character(0),
    year = integer(0),
    journal = character(0),
    doi = character(0)
  )
}


#' Convert between DOIs, PMIDs, PMCIDs, and OpenAlex IDs
#'
#' This function converts a list of DOIs, PMIDs, PMCIDs, or OpenAlex IDs to a
#' specified format using the OpenAlex API. It processes IDs in batches to
#' comply with API limitations and supports parallel processing.
#'
#' @param ids A character vector of DOIs, PMIDs, PMCIDs, or OpenAlex IDs to
#'   convert.
#' @param to A character string specifying the desired output format: "pmid",
#'   "doi", "pmcid", or "openalex". If NULL, the function will attempt to
#'   determine the conversion direction, defaulting to "doi" if no DOIs are
#'   present and "pmid" if some DOIs but no PMIDs are present.
#' @param keep_failed_conversions A logical value. If TRUE, the function will
#'   keep the original ID when conversion fails, instead of returning NA.
#'   Default is FALSE.
#'
#' @return A named character vector with the converted IDs. Names are the input
#'   IDs, and values are the corresponding converted IDs (or original IDs if
#'   keep_failed_conversions is TRUE and conversion fails).
#'
#' @examples
#' \dontrun{
#' # Convert mixed IDs to DOIs
#' ids <- c("10.1016/s0140-6736(06)68853-3", "16950365", "PMC5815332")
#' dois <- convert_article_id(ids, to = "doi")
#'
#' # Convert DOIs to PMIDs, keeping original if conversion fails
#' dois <- c("10.1016/s0140-6736(06)68853-3",
#' "10.1111/j.1469-0691.2007.01724.x")
#' pmids <- convert_article_id(dois,
#' to = "pmid", keep_failed_conversions = TRUE)
#' }
#'
#' @importFrom dplyr mutate group_split bind_rows n coalesce
#'
#' @export
convert_article_id <- function(
  ids,
  to = c("doi", "pmid", "pmcid", "openalex", "semanticscholar"),
  keep_failed_conversions = FALSE
) {
  to <- match.arg(to)

  # If all inputs are NA, return immediately
  if (all(is.na(ids))) {
    return(ids)
  }

  ids <- as.character(ids)
  names(ids) <- ids

  # Only process non-NA ids
  ids_no_missings <- ids[!is.na(ids)]

  # Determine the type of each ID
  id_types <- get_article_id_type(ids_no_missings)

  if (any(is.na(id_types))) {
    stop(
      "Some input IDs are not recognized as DOIs, PMIDs, PMCIDs, ",
      "Semantic Scholar, or OpenAlex IDs."
    )
  }

  # Create a data frame with all IDs, their types, and batch numbers
  all_ids_df <- data.frame(
    id = ids_no_missings,
    type = id_types
  ) |>
    dplyr::mutate(
      batch = ceiling(dplyr::n() / 50),
      .by = "type"
    ) |>
    dplyr::group_split(.data[["type"]], .data[["batch"]])

  # Process all IDs using safe_mirai_map() to leverage parallel workers when
  # they are available. The same conversion logic is applied inside the
  # anonymous function; the relevant internal helpers are passed explicitly so
  # that they are available to background workers spawned by mirai.
  results_list <- safe_mirai_map(
    all_ids_df,
    \(group, ...) {
      id_type <- unique(group$type)
      batch_ids <- group$id

      if (id_type == to) {
        return(data.frame(
          original_id = batch_ids,
          converted_id = batch_ids,
          stringsAsFactors = FALSE
        ))
      }

      # Choose the appropriate API based on identifier types
      if (id_type == "semanticscholar" || to == "semanticscholar") {
        api_result <- get_semanticscholar_articles(
          ids = batch_ids,
          fields = "externalIds"
        )
      } else {
        api_result <- get_openalex_articles(
          ids = batch_ids,
          fields = c("ids", "doi", "id")
        )
      }

      # If API returns no results for this batch, mark all as not found
      if (nrow(api_result) == 0) {
        return(data.frame(
          original_id = batch_ids,
          converted_id = "NF",
          stringsAsFactors = FALSE
        ))
      }

      id_col_name <- if (id_type == "doi") "doi" else id_type

      # Identifier extraction
      if (".ids" %in% names(api_result)) {
        classify_ids <- function(id_df) {
          vals <- trimws(as.character(unlist(id_df)))
          vals <- remove_url_from_id(vals)

          first_match <- function(pattern) {
            pattern <- stringr::regex(pattern, ignore_case = TRUE)
            hit <- vals[stringr::str_detect(vals, pattern)]
            if (length(hit)) hit[1] else NA_character_
          }

          tibble::tibble(
            doi = first_match("^10\\."),
            pmid = first_match("^\\d+$"),
            pmcid = first_match("^PMC\\d+$"),
            openalex = first_match("^W\\d+$")
          )
        }

        ids_wide <- purrr::map(api_result$.ids, classify_ids) |>
          dplyr::bind_rows()

        ids_wide <- ids_wide[,
          setdiff(names(ids_wide), names(api_result)),
          drop = FALSE
        ]

        api_result <- dplyr::bind_cols(
          dplyr::select(api_result, -".ids"),
          ids_wide
        )

        if ("openalex" %in% names(api_result)) {
          is_na_oa <- is.na(api_result$openalex) | api_result$openalex == ""
          api_result$openalex[is_na_oa] <- remove_url_from_id(
            api_result$.paperId[is_na_oa]
          )
        }

        if (id_type == "doi" && !"doi" %in% names(api_result)) {
          api_result$doi <- NA_character_
        }

        if (id_type == "doi") {
          api_result$doi <- dplyr::coalesce(
            tolower(api_result$doi),
            tolower(batch_ids)
          )
        }
      }

      # Case-insensitive matching for DOIs
      if (id_type == "doi") {
        batch_ids <- tolower(batch_ids)
        if (id_col_name %in% names(api_result)) {
          api_result[[id_col_name]] <- tolower(api_result[[id_col_name]])
        }
      }

      conversion_df <- dplyr::left_join(
        data.frame(source_id = batch_ids),
        dplyr::select(api_result, dplyr::any_of(c(to, id_col_name))),
        by = stats::setNames(id_col_name, "source_id")
      )

      converted_ids <- conversion_df[[to]]
      if (to == "doi") converted_ids <- tolower(converted_ids)

      # Fallback: DOI → PMID via PubMed when OpenAlex mapping is missing
      if (id_type == "doi" && to == "pmid") {
        missing_idx <- which(is.na(converted_ids) | converted_ids == "NF")
        if (length(missing_idx)) {
          doi_to_pmid <- function(doi) {
            search_params <- list(
              db = "pubmed",
              term = sprintf("%s[DOI]", doi),
              retmode = "json"
            )
            resp <- pm_make_request(
              "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
              search_params
            )
            cont <- try(httr2::resp_body_json(resp), silent = TRUE)
            if (
              !inherits(cont, "try-error") &&
                !is.null(cont$esearchresult$idlist) &&
                length(cont$esearchresult$idlist) > 0
            ) {
              return(cont$esearchresult$idlist[[1]])
            }
            "NF"
          }
          converted_ids[missing_idx] <- purrr::map_chr(
            batch_ids[missing_idx],
            doi_to_pmid
          )
        }
      }

      converted_ids <- dplyr::coalesce(converted_ids, "NF")
      data.frame(
        original_id = batch_ids,
        converted_id = converted_ids,
        stringsAsFactors = FALSE
      )
    },
    get_semanticscholar_articles = get_semanticscholar_articles,
    get_openalex_articles = get_openalex_articles,
    pm_make_request = pm_make_request,
    remove_url_from_id = remove_url_from_id
  )

  # Collect asynchronous results when running with parallel workers
  if (length(results_list) && inherits(results_list[[1]], "mirai")) {
    results_list <- purrr::map(results_list, `[]`)
  }

  results <- dplyr::bind_rows(results_list)

  # Create the final result as a named vector, preserving original NAs and order
  result <- ids

  # Get non-NA ids
  valid_ids <- ids[!is.na(ids)]

  # Match original ids to results (case-insensitive for DOIs)
  id_matches <- match(
    tolower(valid_ids),
    tolower(results$original_id)
  )

  # Update result vector with converted ids
  result[!is.na(ids)] <- results$converted_id[id_matches]
  names(result) <- ids # Ensure names match input exactly

  # Replace non-NA values with original ids if keep_failed_conversions is TRUE
  if (keep_failed_conversions) {
    result <- dplyr::coalesce(result, ids)
  }

  return(result)
}
