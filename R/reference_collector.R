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
#' dois <- c("10.1016/s0140-6736(06)68853-3", "10.1111/j.1469-0691.2007.01724.x")
#' pmids <- convert_article_id(dois, to = "pmid", keep_failed_conversions = TRUE)
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
    mutate(
      batch = ceiling(seq_len(n()) / 50),
      .by = "type"
    ) |>
    group_split(.data[["type"]], .data[["batch"]])

  # Process all IDs in a single future_map call
  results <- furrr::future_map(
    all_ids_df, \(group) {
      id_type <- unique(group$type)
      batch_ids <- group$id

      # Return IDs that are already in the target format
      if (id_type == to) {
        return(data.frame(original_id = batch_ids, converted_id = batch_ids))
      }

      # Get the ids using the appropriate API
      if (id_type == "semanticscholar" || to == "semanticscholar") {

        api_result <- get_semanticscholar_articles(
          ids = batch_ids, fields = "externalIds")

      } else { # Default to OpenAlex API for all apart semanticscholar IDs

        # Construct the OpenAlex API filter parameter
        filter_param <- paste0(id_type, ":", paste(
          if (id_type == "pmcid") {
            stringr::str_remove(batch_ids, "^PMC")
          } else batch_ids,
          collapse = "|"
        ))

        api_result <- get_openalex_articles(
          ids = batch_ids, fields = "ids"
        )
      }

      conversion_df <- left_join(
        data.frame(source_id = batch_ids),
        api_result$.ids |> select(any_of(c(to, id_type))),
        by = c("source_id" = id_type))

      converted_ids <- conversion_df[[to]]

      if (to == "doi") converted_ids <- tolower(converted_ids)

      converted_ids <- coalesce(converted_ids, "NF")
      original_ids <- conversion_df$source_id

      data.frame(original_id = original_ids, converted_id = converted_ids)
    }) |> bind_rows()

  # Create the final result as a named vector, preserving original NAs and order
  result <- ids

  # Get non-NA ids
  valid_ids <- ids[!is.na(ids)]

  # Match original ids to results
  id_matches <- match(valid_ids, results$original_id)

  # Update result vector with converted ids
  result[!is.na(ids)] <- results$converted_id[id_matches]
  names(result) <- ids  # Ensure names match input exactly

  # Replace non-NA values with original ids if keep_failed_conversions is TRUE
  if (keep_failed_conversions) {
    result <- dplyr::coalesce(result, ids)
  }

  return(result)
}
