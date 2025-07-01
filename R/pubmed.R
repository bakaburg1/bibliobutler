#' Retrieve Articles from PubMed via NCBI E-utilities API
#'
#' This function fetches article data from the PubMed database using the NCBI
#' E-utilities API. Articles can be retrieved by PMIDs, DOIs, titles, or custom
#' search queries. The function handles the conversion of different ID types and
#' returns a standardized data format.
#'
#' @param ids Character vector of PMIDs or DOIs. Default is NULL.
#' @param title Character string to search for in article titles. Default is
#'   NULL.
#' @param query Character string for a custom PubMed search query using PubMed's
#'   search syntax. Default is NULL.
#' @param include_raw Logical, whether to include raw XML data in the results.
#'   Default is FALSE.
#' @param max_results Integer, maximum number of results to return. Default is
#'   Inf (all available results).
#' @param per_page Integer, number of results to retrieve in each batch request.
#'   Default is 1000. Large values provide better API efficiency but may
#'   increase memory usage.
#' @param concurrent Logical, whether to send HTTP requests concurrently.
#'   Default is TRUE. Set to FALSE for sequential HTTP requests.
#'
#' @return A data frame containing article metadata from PubMed with
#' standardized columns:
#' \itemize{
#'   \item .api: Source API identifier (always "pubmed")
#'   \item pmid: PubMed ID
#'   \item doi: Digital Object Identifier
#'   \item title: Article title
#'   \item abstract: Article abstract
#'   \item authors: Author names (in format "LastName FirstName, LastName
#'    FirstName, ...")
#'   \item year: Publication year
#'   \item journal: Journal name (preferably the ISO abbreviation)
#'   \item pubtype: Publication type(s)
#' }
#'
#' @details This function requires at least one of the three main parameters
#'   (ids, title, or query) to be provided. It uses the NCBI E-utilities API
#'   (ESearch and EFetch) to retrieve article data. For ID-based searches, it
#'   uses EFetch directly. For title or query searches, it uses ESearch followed
#'   by EFetch. The function automatically handles API rate limits and
#'   authentication, and will fetch all available results (up to max_results)
#'   using pagination.
#'
#'   PubMed responses can be large, with each batch of 1000 articles being
#'   approximately 25MB in size. The function uses concurrent HTTP requests to
#'   speed up retrieval of multiple batches. This behavior can be disabled by
#'   setting concurrent=FALSE.
#'
#'   Note that while PubMed theoretically supports batch sizes up to 10000,
#'   large batch sizes can cause API timeouts or slow responses, so a default
#'   of 1000 is used. For very large datasets, consider using a smaller per_page
#'   value if you experience issues.
#'
#' @examples
#' \dontrun{
#' # Get article by PMID
#' article <- get_pubmed_articles(ids = "33400058")
#'
#' # Search by title
#' results <- get_pubmed_articles(title = "Machine learning in bioinformatics")
#'
#' # Use a complex query
#' query_results <- get_pubmed_articles(
#'   query = "cancer AND immunotherapy AND (2020[PDAT]:2023[PDAT])"
#' )
#'
#' # Get all articles about COVID-19 vaccines (potentially thousands)
#' all_results <- get_pubmed_articles(
#'   query = "covid-19 vaccine effectiveness"
#' )
#'
#' # Limit to 500 results for a broad query
#' limited_results <- get_pubmed_articles(
#'   query = "cancer treatment",
#'   max_results = 500
#' )
#'
#' # Use a different batch size and disable parallelization
#' results <- get_pubmed_articles(
#'   query = "machine learning",
#'   per_page = 500,
#'   concurrent = FALSE
#' )
#' }
#'
#' @export
get_pubmed_articles <- function(
  ids = NULL,
  title = NULL,
  query = NULL,
  include_raw = FALSE,
  max_results = Inf,
  per_page = 1000,
  concurrent = TRUE
) {
  debug_mode <- isTRUE(getOption("bibliobutler.dev_mode", FALSE))
  if (debug_mode) func_start <- Sys.time()
  on.exit(
    {
      if (debug_mode) {
        elapsed <- round(as.numeric(Sys.time() - func_start, units = "secs"), 2)
        msg_status("DEBUG: Total get_pubmed_articles() time: {elapsed} s")
      }
    },
    add = TRUE
  )

  msg_info("Starting PubMed article retrieval...")

  # Input validation
  inputs <- c(!is.null(ids), !is.null(title), !is.null(query))

  if (sum(inputs) == 0) {
    msg_error("At least one of ids, title, or query must be provided.")
  }

  # Ensure per_page is valid (within PubMed limits)
  per_page <- min(max(1, per_page), 10000) # Clamped between 1 and 10000

  # Warn if per_page is very large
  if (per_page > 2000) {
    msg_warn(
      "Very large batch sizes (per_page > 2000) may result in slow responses or timeouts from PubMed API."
    )
  }

  msg_status("Using batch size of {per_page} articles per request")

  if (concurrent) {
    msg_status("Using concurrent HTTP requests")
  } else {
    msg_status("Using sequential HTTP requests")
  }

  # Base URL for E-utilities
  esearch_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  efetch_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"

  # If IDs are provided, ensure they're PMIDs
  if (!is.null(ids)) {
    msg_status("Converting input IDs to PMIDs...")
    ids <- convert_article_id(ids, to = "pmid")
    ids <- setdiff(ids, c(NA, "NF"))

    if (length(ids) == 0) {
      msg_warn("No valid PMIDs were found after conversion.")
      return(data.frame())
    }

    # If searching by IDs directly
    total_ids <- length(ids)
    msg_info("Fetching {total_ids} articles by ID")

    # Determine how many batches we need
    num_batches <- ceiling(total_ids / per_page)

    # Create request objects for each batch
    requests <- vector("list", num_batches)
    for (i in seq_len(num_batches)) {
      start_idx <- (i - 1) * per_page + 1
      end_idx <- min(i * per_page, length(ids))
      batch_ids <- ids[start_idx:end_idx]

      # Create request for this batch
      params <- list(
        db = "pubmed",
        id = paste(batch_ids, collapse = ","),
        retmode = "xml",
        tool = "bibliobutler",
        email = getOption("bibliobutler.ncbi_email", NULL),
        api_key = getOption("bibliobutler.ncbi_key", NULL)
      )

      requests[[i]] <- create_pm_request(efetch_url, params, concurrent)
    }

    # Perform the requests (either in parallel or sequentially)

    if (concurrent && num_batches > 1) {
      # Use httr2's built-in concurrent request functionality
      responses <- httr2::req_perform_parallel(
        requests,
        max_active = num_batches,
        on_error = "continue" # Use "continue" to process errors but keep going
      )
    } else {
      responses <- httr2::req_perform_sequential(requests)
    }

    # Process the responses
    msg_status("Processing responses...")

    # Process each response and combine into a single data frame
    all_results_list <- lapply(responses, function(resp) {
      process_batch_response(resp, include_raw)
    })

    # Filter out empty data frames and combine results
    all_results_list <- all_results_list[sapply(all_results_list, nrow) > 0]
    all_results <- dplyr::bind_rows(all_results_list)

    msg_success("Retrieved {nrow(all_results)} articles")

    return(all_results)
  }

  # Otherwise, use ESearch followed by EFetch
  # Construct search query
  if (!is.null(title)) {
    search_term <- paste0('"', title, '"[Title]')
  } else {
    search_term <- query
  }

  msg_status("Constructed search term: {search_term}")

  # First search to get total count and WebEnv
  esearch_params <- list(
    db = "pubmed",
    term = search_term,
    retmax = 0, # We just want the count first
    usehistory = "y",
    retmode = "json",
    tool = "bibliobutler",
    email = getOption("bibliobutler.ncbi_email", NULL),
    api_key = getOption("bibliobutler.ncbi_key", NULL)
  )

  msg_status("Executing PubMed search query...")
  esearch_request <- create_pm_request(esearch_url, esearch_params, concurrent)
  esearch_response <- httr2::req_perform(esearch_request)

  # Parse the response
  search_content <- httr2::resp_body_json(esearch_response)

  # Check if we have results
  if (
    is.null(search_content$esearchresult$count) ||
      as.numeric(search_content$esearchresult$count) == 0
  ) {
    msg_warn("No results found in PubMed for the given query.")
    return(data.frame())
  }

  # Get total count and limit to max_results if specified
  total_count <- as.numeric(search_content$esearchresult$count)

  if (!is.infinite(max_results) && max_results < total_count) {
    display_count <- max_results
  } else {
    display_count <- total_count
  }

  msg_info("Total results: {total_count} for query")
  if (display_count < total_count) {
    msg_info("(retrieving first {display_count} results)")
  }

  # Get WebEnv and QueryKey for batch retrieval
  web_env <- search_content$esearchresult$webenv
  query_key <- search_content$esearchresult$querykey

  msg_status("Using WebEnv: {web_env}, QueryKey: {query_key}")

  # Create request objects for each batch
  num_batches <- ceiling(min(display_count, total_count) / per_page)

  # Create request objects for each batch
  requests <- vector("list", num_batches)
  for (i in seq_len(num_batches)) {
    start <- (i - 1) * per_page
    current_batch_size <- min(per_page, display_count - start)

    # Skip if we've already reached the limit
    if (current_batch_size <= 0) next

    # Create request for this batch
    params <- list(
      db = "pubmed",
      WebEnv = web_env,
      query_key = query_key,
      retstart = start,
      retmax = current_batch_size,
      retmode = "xml",
      tool = "bibliobutler",
      email = getOption("bibliobutler.ncbi_email", NULL),
      api_key = getOption("bibliobutler.ncbi_key", NULL)
    )

    requests[[i]] <- create_pm_request(efetch_url, params, concurrent)
  }

  # Remove any NULL entries (in case we had early termination due to max_results)
  requests <- requests[!sapply(requests, is.null)]

  # Perform the requests (either in parallel or sequentially)
  msg_status("Performing {length(requests)} batch requests...")

  if (concurrent && length(requests) > 1) {
    # Use httr2's built-in concurrent request functionality
    responses <- httr2::req_perform_parallel(
      requests,
      max_active = length(requests),
      on_error = "continue" # Use "continue" to process errors but keep going
    )
  } else {
    responses <- httr2::req_perform_sequential(requests)
  }

  # Process the responses
  msg_status("Processing responses...")

  # Process each response and combine into a single data frame
  all_results_list <- lapply(responses, function(resp) {
    process_batch_response(resp, include_raw)
  })

  # Filter out empty data frames and combine results
  all_results_list <- all_results_list[sapply(all_results_list, nrow) > 0]
  all_results <- dplyr::bind_rows(all_results_list)

  msg_success("Fetched {nrow(all_results)} results from PubMed")

  return(all_results)
}

#' Retrieve Linked Articles from PubMed
#'
#' This function fetches citations, references, or related articles for a given
#' set of PubMed IDs using the NCBI E-utilities API. It automatically converts
#' other ID types (DOIs, PMCIDs) to PMIDs and handles the API requests to
#' retrieve all linked articles.
#'
#' @param ids Character vector of article identifiers (PMIDs, DOIs, etc.).
#' @param links Character vector specifying the types of links to fetch:
#'   "citations", "references", or "related". Default is all three types.
#'
#' @return A list of data frames, one for each requested link type, each with
#'   columns:
#'   \itemize{
#'     \item linked_id: The linked PMID
#'     \item source_id: List column containing the source PMIDs that link to
#'        the linked_id
#'   }
#'
#' @details This function uses the NCBI ELink API to retrieve connected
#'   articles. Input IDs are automatically converted to PMIDs using the
#'   convert_article_id function. For citations, it fetches articles that cite
#'   the input article(s). For references, it fetches articles that are cited by
#'   the input article(s). For related articles, it fetches articles that are
#'   conceptually related according to PubMed's algorithm.
#'
#' @examples
#' \dontrun{
#' # Get all link types for a PMID
#' links <- get_pubmed_linked("33400058")
#'
#' # Get only citations for a DOI
#' citations <- get_pubmed_linked(
#'  "10.1038/s41586-020-2012-7", links = "citations")
#'
#' # Get references for multiple articles
#' refs <- get_pubmed_linked(c("33400058", "34320281"), links = "references")
#' }
#'
#' @export
get_pubmed_linked <- function(
  ids,
  links = c("citations", "references", "related")
) {
  debug_mode <- isTRUE(getOption("bibliobutler.dev_mode", FALSE))
  if (debug_mode) func_start <- Sys.time()
  on.exit(
    {
      if (debug_mode) {
        elapsed <- round(as.numeric(Sys.time() - func_start, units = "secs"), 2)
        msg_status("DEBUG: Total get_pubmed_linked() time: {elapsed} s")
      }
    },
    add = TRUE
  )

  # Validate and select the link types
  links <- match.arg(links, several.ok = TRUE)

  # Convert IDs to PMIDs
  ids <- convert_article_id(ids, to = "pmid")
  ids <- setdiff(ids, c(NA, "NF"))

  # Prepare the results list
  empty_df <- data.frame(linked_id = character(), source_id = character())

  results <- list()
  for (l in links) {
    results[[l]] <- empty_df
  }

  if (length(ids) == 0) {
    msg_warn(
      "No valid PMIDs were found. No results will be returned."
    )

    return(results)
  }

  # Define linkname mapping
  link_mapping <- list(
    citations = "pubmed_pubmed_citedin",
    references = "pubmed_pubmed_refs",
    related = c(
      "pubmed_pubmed",
      "pubmed_pubmed_alsoviewed",
      "pubmed_pubmed_reviews",
      "pubmed_pubmed_combined"
    )
  )

  # Base URL for E-link
  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi"

  # Build the query parameters
  params <- list(
    dbfrom = "pubmed",
    db = "pubmed",
    id = paste(ids, collapse = ","),
    retmode = "json",
    tool = "bibliobutler",
    email = getOption("bibliobutler.ncbi_email", NULL),
    api_key = getOption("bibliobutler.ncbi_key", NULL),
    retmax = 1000
  )

  # Edit the query based on the links parameter
  if (length(links) == 1 && links %in% c("citations", "references")) {
    params$linkname <- link_mapping[[links]]
  } else {
    params$cmd <- "neighbor"
  }

  # Make the API request
  response <- pm_make_request(url, params)

  content <- tryCatch(
    {
      httr2::resp_body_json(response)
    },
    error = function(e) {
      msg_warn("Could not parse JSON from PubMed ELink API: {e$message}")
      NULL # Return NULL to indicate parsing failure
    }
  )

  if (is.null(content)) {
    msg_warn(
      "No parseable content received from PubMed ELink API. Returning empty results."
    )
    return(results)
  }

  # Process the results - for each input ID...
  for (linkset in content$linksets) {
    source_id <- linkset$ids[[1]]

    # Skip if the linkset has no linksetdbs
    if (is.null(linkset$linksetdbs) || length(linkset$linksetdbs) == 0) {
      next
    }

    # Assign names to the linksetdbs which contain the linked ids
    linksetdb <- linkset$linksetdbs |>
      stats::setNames(
        purrr::map_chr(linkset$linksetdbs, \(db) db$linkname)
      )

    # For each link type...
    for (l in links) {
      # Get the linked ids and remove duplicates
      linked_ids <- purrr::map(
        link_mapping[[l]],
        \(ln) {
          if (!is.null(linksetdb[[ln]])) linksetdb[[ln]]$links else NULL
        }
      ) |>
        unlist() |>
        unique()

      # Add the new links to the existing ones
      if (length(linked_ids) > 0) {
        new_links <- data.frame(linked_id = linked_ids, source_id = source_id)
        results[[l]] <- bind_rows(results[[l]], new_links)
      }
    }
  }

  # Collapse source_ids for unique linked_ids
  for (l in links) {
    if (nrow(results[[l]]) > 0) {
      results[[l]] <- results[[l]] |>
        summarise(
          source_id = list(unique(source_id)),
          .by = linked_id
        )
    }
  }

  return(results)
}


#' Format PubMed API results
#'
#' Internal function to process and normalize data from PubMed API responses.
#'
#' @param response The response object from an API call.
#' @param include_raw Logical indicating whether to include raw XML in the
#'   result.
#'
#' @return A data frame with standardized article data.
#'
#' @keywords internal
pm_format_results <- function(response, include_raw = FALSE) {
  # Get content from httr2 response
  content <- httr2::resp_body_string(response)

  # Parse XML
  parsed <- try(xml2::read_xml(content), silent = TRUE)

  if (inherits(parsed, "try-error")) {
    msg_error(
      "Failed to parse XML: {attr(parsed, 'condition')$message}",
      stop = FALSE
    )
    return(data.frame())
  }

  # Extract articles from XML
  articles <- xml2::xml_find_all(parsed, ".//PubmedArticle")

  if (length(articles) == 0) {
    msg_warn("No articles found in response")
    return(data.frame())
  }

  msg_status("Processing {length(articles)} articles...")

  # Process all articles at once instead of batching
  results <- purrr::map_dfr(articles, function(article) {
    # Extract basic article info
    pmid <- xml2::xml_text(xml2::xml_find_first(article, ".//PMID"))

    # Journal info
    journal_node <- xml2::xml_find_first(article, ".//Journal")
    journal <- if (!is.na(journal_node)) {
      xml2::xml_text(
        xml2::xml_find_first(journal_node, ".//ISOAbbreviation") %||%
          xml2::xml_find_first(journal_node, ".//Title")
      )
    } else NA_character_

    # Article title
    title_node <- xml2::xml_find_first(article, ".//ArticleTitle")
    title <- if (!is.na(title_node)) xml2::xml_text(title_node) else
      NA_character_

    # Abstract
    abstract_nodes <- xml2::xml_find_all(article, ".//AbstractText")
    abstract <- if (length(abstract_nodes) > 0) {
      paste(purrr::map_chr(abstract_nodes, xml2::xml_text), collapse = " ")
    } else NA_character_

    # Publication date
    year_node <- xml2::xml_find_first(article, ".//PubDate/Year")
    year <- if (!is.na(year_node)) as.integer(xml2::xml_text(year_node)) else
      NA_integer_

    # Author list
    author_nodes <- xml2::xml_find_all(article, ".//Author")
    authors <- if (length(author_nodes) > 0) {
      author_names <- purrr::map_chr(author_nodes, function(author) {
        last_name <- xml2::xml_text(
          xml2::xml_find_first(author, ".//LastName") %||%
            xml2::xml_find_first(author, ".//CollectiveName")
        )
        first_name <- xml2::xml_find_first(author, ".//ForeName")
        first_name <- if (!is.na(first_name)) xml2::xml_text(first_name) else ""

        if (first_name != "") {
          paste(last_name, first_name)
        } else {
          last_name
        }
      })
      paste(author_names, collapse = ", ")
    } else NA_character_

    # DOI
    doi_node <- xml2::xml_find_first(article, ".//ELocationID[@EIdType='doi']")
    doi <- if (!is.na(doi_node)) xml2::xml_text(doi_node) else NA_character_

    # Publication types
    pubtype_nodes <- xml2::xml_find_all(article, ".//PublicationType")
    pubtypes <- if (length(pubtype_nodes) > 0) {
      paste(purrr::map_chr(pubtype_nodes, xml2::xml_text), collapse = "; ")
    } else NA_character_

    # Assemble result
    result <- data.frame(
      .record_name = NA_character_,
      .paperId = pmid,
      .api = "pubmed",
      pmid = pmid,
      doi = doi,
      .title = title,
      .abstract = abstract,
      .authors = parse_authors(authors, to_string = TRUE),
      .year = year,
      .journal = journal,
      .pubtype = pubtypes,
      .url = NA_character_,
      .is_open_access = NA,
      .ids = I(list(data.frame(pmid = pmid, doi = doi))),
      .references = I(list(NA_character_)),
      .citations = I(list(NA_character_)),
      .related = I(list(NA_character_)),
      stringsAsFactors = FALSE
    )

    result$.record_name <- generate_record_name(result)

    if (include_raw) {
      result$raw_xml <- list(xml2::as_xml_document(article))
    }

    result
  })

  return(results)
}

#' Create a PubMed API request
#'
#' Internal function to configure and execute API requests to PubMed.
#'
#' @param endpoint The API endpoint URL.
#' @param params List of parameters for the request.
#' @param method HTTP method to use ("GET" or "POST").
#'
#' @return An httr2 response object.
#'
#' @keywords internal
pm_make_request <- function(endpoint, params, method = "GET") {
  # Add API credentials
  params <- c(
    params,
    list(
      tool = "bibliobutler",
      email = getOption("bibliobutler.ncbi_email", NULL),
      api_key = getOption("bibliobutler.ncbi_key", NULL)
    )
  )

  # Remove NULL parameters
  params <- params[!sapply(params, is.null)]

  # Build the request using httr2
  req <- httr2::request(endpoint) |>
    # Set the method
    httr2::req_method(method)

  # Add parameters as query or body based on method
  if (method == "GET") {
    req <- req |> httr2::req_url_query(!!!params)
  } else {
    req <- req |> httr2::req_body_form(!!!params)
  }

  # Add retry with exponential backoff for rate limits and server errors
  req <- req |>
    httr2::req_retry(
      max_tries = 5,
      backoff = \(i) max(rpois(1, 2 * 2^i), 1),
      is_transient = \(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 503, 504)
      }
    )

  if (getOption("bibliobutler.dev_mode", TRUE)) {
    msg_status("Requesting url: {req$url}")
  }

  # Perform the request
  response <- req |> httr2::req_perform()

  return(response)
}

#' Create a PubMed request with proper retry and throttling
#'
#' Internal function to create a request with appropriate retry settings for
#' PubMed API calls. The function includes exponential backoff for rate limits
#' and server errors.
#'
#' @param url The URL to request.
#' @param params List of parameters for the request.
#' @param concurrent Whether the request will be part of a concurrent batch.
#'
#' @return An httr2 request object.
#'
#' @keywords internal
create_pm_request <- function(url, params, concurrent = FALSE) {
  # Configure more conservative retry parameters for concurrent requests
  max_tries <- if (concurrent) 8 else 5

  # Initial backoff in seconds (minimum wait time between retries)
  backoff_min <- if (concurrent) 1.0 else 0.5

  # Configure backoff strategy - exponential with concurrent-aware settings
  backoff_fn <- function(attempt) {
    # Base value (seconds) doubles with each retry attempt
    base <- 2 * 2^attempt

    # Add randomization to avoid thundering herd when doing concurrent requests
    jitter <- if (concurrent) runif(1, min = 0, max = 2) else 0

    # For concurrent requests, we want a longer backoff
    multiplier <- if (concurrent) 1.5 else 1.0

    # Calculate final backoff with a minimum value
    max(backoff_min, multiplier * base + jitter)
  }

  httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(
      max_tries = max_tries,
      backoff = backoff_fn,
      is_transient = function(resp) {
        # Consider more status codes as transient for better handling
        # 400 Bad Request is often returned for rate limiting with PubMed
        httr2::resp_status(resp) %in% c(400, 429, 500, 502, 503, 504)
      }
    )
}

#' Process a PubMed batch response into a data frame
#'
#' Internal function to process a response from the PubMed API.
#'
#' @param response The response object from an API call.
#' @param include_raw Logical indicating whether to include raw XML in the
#'   result.
#'
#' @return A data frame of processed article data.
#'
#' @keywords internal
process_batch_response <- function(response, include_raw = FALSE) {
  # Check if the response is an error object (httr2_perform_parallel returns
  # errors as objects)
  if (inherits(response, "error") || inherits(response, "httr2_http_")) {
    msg_error("API error: {conditionMessage(response)}", stop = FALSE)
    return(data.frame())
  }

  if (httr2::resp_status(response) >= 400) {
    msg_error("API error: HTTP {httr2::resp_status(response)}", stop = FALSE)
    return(data.frame())
  }

  batch_results <- try(pm_format_results(response, include_raw), silent = TRUE)

  if (inherits(batch_results, "try-error")) {
    msg_error(
      "Failed to process batch: {attr(batch_results, 'condition')$message}",
      stop = FALSE
    )
    return(data.frame())
  }

  batch_results
}
