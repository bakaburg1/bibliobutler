get_semanticscholar_article <- function(
    ids = NULL,
    query = NULL,
    filter_query = NULL,
    fields = NULL,
    per_page = 100,
    max_results = Inf
) {
  # Input validation
  inputs <- c(!is.null(ids), !is.null(query), !is.null(filter_query))
  if (sum(inputs) != 1) {
    stop("Use only one between `ids`, `query`, or `filter_query` as arguments")
  }
  if (sum(inputs) == 0) {
    stop("One `ids`, `query`, or `filter_query` must be provided.")
  }

  # Prepare the query data
  query_data <- list(
    fields = if (!is.null(fields)) paste(fields, collapse = ",") else NULL
  )

  # Function to make a single API call
  make_api_call <- function(endpoint, method, body = NULL, query = NULL) {
    response <- httr::RETRY(
      method,
      endpoint,
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

  # Function to process the response
  process_response <- function(data) {
    if ("data" %in% names(data)) data <- data$data
    
    data |> dplyr::mutate(
      .api = "semanticscholar",
      paperId = data$paperId,
      doi = coalesce(data$externalIds$DOI, NA),
      title = coalesce(data$title, NA),
      abstract = coalesce(data$abstract, NA),
      across(any_of("authors"), ~ .x |> purrr::map_chr(\(.y) {
        paste(.y$name, collapse = ", ") |> 
          stringr::str_remove_all("\\.")
      })),
      year = coalesce(data$year, NA),
      journal = coalesce(data$venue, NA),
      pubtype = coalesce(data$publicationTypes, NA),
      .references = list(data$references$paperId),
      .citations = list(data$citations$paperId),
      .keep = "none"
    )
  }

  # Process batch of ids
  process_batch <- function(batch_ids) {
    endpoint <- "https://api.semanticscholar.org/graph/v1/paper/batch"
    body <- list(ids = as.list(unname(batch_ids)))
    
    results <- make_api_call(endpoint, "POST", body = body, query = query_data)
    process_response(results)
  }

  # Initialize results
  all_results <- list()

  if (!is.null(ids)) {
    # Convert ids to DOI, then to PMID if DOI conversion fails
    converted_ids <- convert_article_id(ids, to = "doi")
    pmid_mask <- converted_ids %in% c(NA, "NF")
    if (any(pmid_mask)) {
      converted_ids[pmid_mask] <- convert_article_id(ids[pmid_mask], to = "pmid")
    }
    
    # Prefix DOIs with "DOI:" and PMIDs with "PMID:"
    prefixed_ids <- ifelse(
      !is.na(converted_ids) & converted_ids != "NF",
      ifelse(
        !pmid_mask,
        paste0("DOI:", converted_ids),
        paste0("PMID:", converted_ids)
      ),
      NA
    )
    
    # Remove any remaining NAs or NFs
    valid_ids <- prefixed_ids[!is.na(prefixed_ids) & prefixed_ids != "NF"]
    
    if (length(valid_ids) == 0) {
      stop("No valid IDs after conversion.")
    }
    
    # Process ids in batches of 500
    id_batches <- split(valid_ids, ceiling(seq_along(valid_ids) / 500))
    
    all_results <- furrr::future_map(id_batches, process_batch, .progress = TRUE) %>%
      purrr::flatten()
    
  } else {
    # Use paper/search/bulk for query or filter_query
    endpoint <- "https://api.semanticscholar.org/graph/v1/paper/search/bulk"
    query_data$limit <- per_page
    
    if (!is.null(query)) {
      query_data$query <- query
    } else {
      query_data$filter <- filter_query
    }

    total_results <- 0
    token <- NULL

    repeat {
      if (!is.null(token)) {
        query_data$token <- token
      }
      
      batch_results <- make_api_call(endpoint, "GET", query = query_data)
      
      all_results <- c(all_results, list(process_response(batch_results)))
      total_results <- total_results + nrow(all_results[[length(all_results)]])
      token <- batch_results$token
      
      if (is.null(token) || total_results >= max_results) {
        break
      }
    }
  }

  # Combine all results
  results <- dplyr::bind_rows(all_results) %>%
    dplyr::mutate(
      RecordName = generate_record_name(pick(everything())),
      .after = .api
    )

  if (nrow(results) == 0) {
    warning("No results found for the given query.", call. = FALSE, immediate. = TRUE)
  }

  return(results)
}