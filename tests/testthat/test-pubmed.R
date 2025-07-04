test_that("get_pubmed_articles retrieves correct article data compared to direct API call", {
  # Test article with known PMID
  test_pmid <- "33400058"

  # Get article data using our implementation
  result <- get_pubmed_articles(ids = test_pmid)

  # Make a direct API call to PubMed
  api_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",
    test_pmid,
    "&retmode=xml"
  )

  direct_response <- httr::GET(api_url)
  direct_content <- httr::content(direct_response, "text", encoding = "UTF-8")
  direct_xml <- xml2::read_xml(direct_content)

  # Extract key fields from direct API call
  direct_pmid <- xml2::xml_text(xml2::xml_find_first(direct_xml, "//PMID"))
  direct_title <- xml2::xml_text(xml2::xml_find_first(
    direct_xml,
    "//ArticleTitle"
  ))
  direct_doi_node <- xml2::xml_find_first(
    direct_xml,
    "//ELocationID[@EIdType='doi']"
  )
  direct_doi <- if (!is.na(direct_doi_node))
    xml2::xml_text(direct_doi_node) else NA_character_

  # Extract journal info
  journal_node <- xml2::xml_find_first(direct_xml, "//Journal")
  direct_journal <- if (!is.na(journal_node)) {
    iso_abbrev_node <- xml2::xml_find_first(journal_node, ".//ISOAbbreviation")
    if (!is.na(iso_abbrev_node)) {
      xml2::xml_text(iso_abbrev_node)
    } else {
      title_node <- xml2::xml_find_first(journal_node, ".//Title")
      if (!is.na(title_node)) xml2::xml_text(title_node) else NA_character_
    }
  } else NA_character_

  # Extract publication year
  year_node <- xml2::xml_find_first(direct_xml, "//PubDate/Year")
  direct_year <- if (!is.na(year_node))
    as.integer(xml2::xml_text(year_node)) else NA_integer_

  # Extract authors
  author_nodes <- xml2::xml_find_all(direct_xml, "//Author")
  direct_authors <- if (length(author_nodes) > 0) {
    author_names <- sapply(author_nodes, function(author) {
      last_name_node <- xml2::xml_find_first(author, ".//LastName")
      first_name_node <- xml2::xml_find_first(author, ".//ForeName")

      last_name <- if (!is.na(last_name_node)) {
        xml2::xml_text(last_name_node)
      } else {
        collective_name_node <- xml2::xml_find_first(
          author,
          ".//CollectiveName"
        )
        if (!is.na(collective_name_node))
          xml2::xml_text(collective_name_node) else ""
      }

      first_name <- if (!is.na(first_name_node))
        xml2::xml_text(first_name_node) else ""

      if (first_name != "") {
        paste(last_name, first_name)
      } else {
        last_name
      }
    })
    paste(author_names, collapse = ", ")
  } else NA_character_

  # Extract publication types
  pubtype_nodes <- xml2::xml_find_all(direct_xml, "//PublicationType")
  direct_pubtypes <- if (length(pubtype_nodes) > 0) {
    paste(sapply(pubtype_nodes, xml2::xml_text), collapse = "; ")
  } else NA_character_

  # Test that our implementation returns the correct data
  expect_equal(result$pmid[1], direct_pmid, ignore_attr = TRUE)
  expect_equal(result$doi[1], direct_doi, ignore_attr = TRUE)
  expect_equal(result$.title[1], direct_title, ignore_attr = TRUE)
  expect_equal(result$.journal[1], direct_journal, ignore_attr = TRUE)
  expect_equal(result$.year[1], direct_year, ignore_attr = TRUE)
  expect_equal(
    result$.authors[1],
    parse_authors(direct_authors, to_string = TRUE),
    ignore_attr = TRUE
  )
  expect_equal(result$.pubtype[1], direct_pubtypes, ignore_attr = TRUE)

  # Test that the result has all expected columns
  expected_columns <- c(
    ".api",
    "pmid",
    "doi",
    ".title",
    ".abstract",
    ".authors",
    ".year",
    ".journal",
    ".pubtype",
    ".record_name",
    ".paperId",
    ".url",
    ".is_open_access",
    ".ids",
    ".references",
    ".citations",
    ".related"
  )
  expect_named(result, expected_columns, ignore.order = TRUE)

  # Test that the API field is correctly set
  expect_equal(result$.api[1], "pubmed")
})

test_that("get_pubmed_articles handles DOI input correctly with direct API comparison", {
  # Test with a DOI instead of PMID
  test_doi <- "10.1208/s12248-020-00532-2"
  expected_pmid <- "33400058"

  # Get article data using our implementation with DOI
  result <- get_pubmed_articles(ids = test_doi)

  # Make a direct API call to PubMed using the expected PMID
  api_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",
    expected_pmid,
    "&retmode=xml"
  )

  direct_response <- httr::GET(api_url)
  direct_content <- httr::content(direct_response, "text", encoding = "UTF-8")
  direct_xml <- xml2::read_xml(direct_content)

  # Extract key fields from direct API call
  direct_pmid <- xml2::xml_text(xml2::xml_find_first(direct_xml, "//PMID"))
  direct_title <- xml2::xml_text(xml2::xml_find_first(
    direct_xml,
    "//ArticleTitle"
  ))
  direct_doi_node <- xml2::xml_find_first(
    direct_xml,
    "//ELocationID[@EIdType='doi']"
  )
  direct_doi <- if (!is.na(direct_doi_node))
    xml2::xml_text(direct_doi_node) else NA_character_

  # Test that the DOI was correctly converted to PMID and article was retrieved
  expect_equal(result$pmid[1], direct_pmid)
  expect_equal(result$.title[1], direct_title)
  expect_equal(result$doi[1], direct_doi)
})

test_that("get_pubmed_articles handles invalid IDs correctly", {
  # Test with an invalid ID
  expect_error(
    get_pubmed_articles(ids = "invalid_id"),
    "Some input IDs are not recognized"
  )

  # Test with a non-existent but valid-format PMID
  # The function might not throw a warning for non-existent PMIDs
  # as they might be valid format but just not exist in the database
  non_existent_pmid <- "99999999999"
  result <- get_pubmed_articles(ids = non_existent_pmid)
  expect_equal(nrow(result), 0)
})

test_that("get_pubmed_linked retrieves correct linked articles compared to direct API call", {
  # Test article with known PMID
  test_pmid <- "33400058"

  # Get linked articles using our implementation
  linked_results <- get_pubmed_linked(
    test_pmid,
    links = c("citations", "references")
  )

  # Make a direct API call to PubMed for linked articles
  api_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&db=pubmed",
    "&id=",
    test_pmid,
    "&cmd=neighbor&retmode=json"
  )

  direct_response <- httr2::request(api_url) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  # Try to parse JSON, handle potential non-JSON error responses
  direct_content <- tryCatch(
    {
      httr2::resp_body_json(direct_response)
    },
    error = function(e) {
      message(
        "Warning: Could not parse JSON from PubMed ELink API: ",
        e$message
      )
      NULL # Return NULL or an empty list if parsing fails
    }
  )

  # Ensure content is not NULL before proceeding
  if (is.null(direct_content) || is.null(direct_content$linksets)) {
    message(
      "Direct API call for get_pubmed_linked returned no parseable JSON or data. Expecting empty results."
    )
    expect_equal(nrow(linked_results$citations), 0)
    expect_equal(nrow(linked_results$references), 0)
    return(NULL) # Exit the test prematurely as the direct call failed
  }

  # Extract citations and references from direct API call
  direct_linksets <- direct_content$linksets[[1]]$linksetdbs

  # Find citation and reference linksets
  direct_citations <- NULL
  direct_references <- NULL

  for (linkset in direct_linksets) {
    if (linkset$linkname == "pubmed_pubmed_citedin") {
      direct_citations <- linkset$links
    } else if (linkset$linkname == "pubmed_pubmed_refs") {
      direct_references <- linkset$links
    }
  }

  # Test that our implementation returns the correct data
  if (!is.null(direct_citations) && length(direct_citations) > 0) {
    # Ensure direct_citations is a character vector
    if (is.list(direct_citations)) {
      direct_citations <- unlist(direct_citations)
    }

    # Sort both lists to ensure consistent comparison
    sorted_direct_citations <- sort(as.character(direct_citations))
    sorted_result_citations <- sort(as.character(
      linked_results$citations$linked_id
    ))

    # Check if all direct citations are in our results
    expect_setequal(sorted_result_citations, sorted_direct_citations)
    expect_equal(length(direct_citations), nrow(linked_results$citations))
  }

  if (!is.null(direct_references) && length(direct_references) > 0) {
    # Ensure direct_references is a character vector
    if (is.list(direct_references)) {
      direct_references <- unlist(direct_references)
    }

    # Sort both lists to ensure consistent comparison
    sorted_direct_references <- sort(as.character(direct_references))
    sorted_result_references <- sort(as.character(
      linked_results$references$linked_id
    ))

    # Check if all direct references are in our results
    expect_setequal(sorted_result_references, sorted_direct_references)
    expect_equal(length(direct_references), nrow(linked_results$references))
  }

  # Test that the result has the expected structure
  expect_named(linked_results$citations, c("linked_id", "source_id"))
  expect_named(linked_results$references, c("linked_id", "source_id"))

  # Test that source_id is correctly set
  if (nrow(linked_results$citations) > 0) {
    expect_true(all(sapply(
      linked_results$citations$source_id,
      function(x) test_pmid %in% x
    )))
  }
  if (nrow(linked_results$references) > 0) {
    expect_true(all(sapply(
      linked_results$references$source_id,
      function(x) test_pmid %in% x
    )))
  }
})

test_that("get_pubmed_linked handles different link types correctly with direct API comparison", {
  # Test article with known PMID
  test_pmid <- "33400058"

  # Make a direct API call to PubMed for all link types
  api_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&db=pubmed",
    "&id=",
    test_pmid,
    "&cmd=neighbor&retmode=json"
  )

  direct_response <- httr2::request(api_url) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  # Try to parse JSON, handle potential non-JSON error responses
  direct_content <- tryCatch(
    {
      httr2::resp_body_json(direct_response)
    },
    error = function(e) {
      message(
        "Warning: Could not parse JSON from PubMed ELink API: ",
        e$message
      )
      NULL # Return NULL or an empty list if parsing fails
    }
  )

  # Ensure content is not NULL before proceeding
  if (is.null(direct_content) || is.null(direct_content$linksets)) {
    message(
      "Direct API call for get_pubmed_linked returned no parseable JSON or data. Expecting empty results."
    )
    expect_equal(nrow(citations_only), 0)
    expect_equal(nrow(references_only), 0)
    expect_equal(nrow(related_only), 0)
    expect_equal(nrow(all_links$citations), 0)
    expect_equal(nrow(all_links$references), 0)
    expect_equal(nrow(all_links$related), 0)
    return(NULL) # Exit the test prematurely as the direct call failed
  }

  # Extract linknames from direct API call
  direct_linknames <- sapply(
    direct_content$linksets[[1]]$linksetdbs,
    function(db) db$linkname
  )

  # Test with only citations
  citations_only <- get_pubmed_linked(test_pmid, links = "citations")
  expect_named(citations_only, "citations")
  expect_false("references" %in% names(citations_only))
  expect_false("related" %in% names(citations_only))
  expect_in("pubmed_pubmed_citedin", direct_linknames)

  # Test with only references
  references_only <- get_pubmed_linked(test_pmid, links = "references")
  expect_false("citations" %in% names(references_only))
  expect_named(references_only, "references", ignore.order = TRUE)
  expect_false("related" %in% names(references_only))
  expect_in("pubmed_pubmed_refs", direct_linknames)

  # Test with only related
  related_only <- get_pubmed_linked(test_pmid, links = "related")
  expect_false("citations" %in% names(related_only))
  expect_false("references" %in% names(related_only))
  expect_named(related_only, "related")

  # Test with all link types
  all_links <- get_pubmed_linked(test_pmid)
  expect_named(all_links, c("citations", "references", "related"))
})

test_that("get_article_id_type correctly identifies PMIDs", {
  # Valid PMIDs
  expect_equal(get_article_id_type("12345678"), "pmid")
  expect_equal(get_article_id_type("33400058"), "pmid")

  # Non-PMIDs
  expect_equal(get_article_id_type("PMC12345"), "pmcid")
  expect_equal(get_article_id_type("10.1038/nature"), "doi")
  expect_equal(get_article_id_type("not_a_pmid"), NA_character_)
  expect_equal(get_article_id_type(""), NA_character_)
})

test_that("pm_format_results correctly formats PubMed API responses compared to raw XML", {
  # Direct API call to get raw XML for testing pm_format_results
  pmid_test <- "33400058"
  api_url_direct <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",
    pmid_test,
    "&retmode=xml"
  )
  direct_response_obj <- httr2::request(api_url_direct) |>
    httr2::req_perform()

  # Expected values (from direct XML parsing to avoid dependency on pm_format_results bugs)
  direct_title <- xml2::xml_text(xml2::xml_find_first(
    xml2::read_xml(httr2::resp_body_string(direct_response_obj)),
    ".//ArticleTitle"
  ))

  # Test pm_format_results
  result <- pm_format_results(direct_response_obj)

  # Verify core fields
  expect_equal(result$.title[1], direct_title, ignore_attr = TRUE)

  # Test with include_raw = TRUE
  result_with_raw <- pm_format_results(direct_response_obj, include_raw = TRUE)
  expect_true("raw_xml" %in% names(result_with_raw))
  expect_s3_class(result_with_raw$raw_xml[[1]], "xml_document")

  # Test that the result has all expected columns
  expected_columns <- c(
    ".api",
    "pmid",
    "doi",
    ".title",
    ".abstract",
    ".authors",
    ".year",
    ".journal",
    ".pubtype",
    ".record_name",
    ".paperId",
    ".url",
    ".is_open_access",
    ".ids",
    ".references",
    ".citations",
    ".related"
  )
  expect_named(result, expected_columns, ignore.order = TRUE)

  expected_columns_raw <- c(expected_columns, "raw_xml")
  expect_named(result_with_raw, expected_columns_raw, ignore.order = TRUE)
})

test_that("pm_make_request correctly handles API requests with proper parameters", {
  # Test with a simple request
  endpoint <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
  params <- list(
    db = "pubmed",
    id = "33400058",
    retmode = "xml"
  )

  # Make the request using our function
  response <- pm_make_request(endpoint, params)

  # Make a direct request for comparison using httr2
  direct_params <- c(
    params,
    list(
      tool = "bibliobutler",
      email = getOption("bibliobutler.ncbi_email", NULL),
      api_key = getOption("bibliobutler.ncbi_key", NULL)
    )
  )
  # Remove NULL parameters
  direct_params <- direct_params[!sapply(direct_params, is.null)]

  direct_response <- httr2::request(endpoint) |>
    httr2::req_url_query(!!!direct_params) |>
    httr2::req_perform()

  # Check that both responses have the same status code using httr2 functions
  expect_equal(
    httr2::resp_status(response),
    httr2::resp_status(direct_response)
  )
  expect_equal(httr2::resp_status(response), 200)

  # Check that both responses have similar content
  response_content <- httr2::resp_body_string(response)
  direct_content <- httr2::resp_body_string(direct_response)

  # Parse XML to compare key elements
  response_xml <- xml2::read_xml(response_content)
  direct_xml <- xml2::read_xml(direct_content)

  response_pmid <- xml2::xml_text(xml2::xml_find_first(response_xml, "//PMID"))
  direct_pmid <- xml2::xml_text(xml2::xml_find_first(direct_xml, "//PMID"))

  expect_equal(response_pmid, direct_pmid)
  expect_equal(response_pmid, "33400058")

  # Test with POST method
  response_post <- pm_make_request(endpoint, params, method = "POST")
  expect_s3_class(response_post, "httr2_response")
  expect_equal(httr2::resp_status(response_post), 200)
})

test_that("get_pubmed_articles returns expected structure for a query", {
  # Test with a query
  test_query <- "machine learning"
  max_results_test <- 5
  results <- get_pubmed_articles(
    query = test_query,
    max_results = max_results_test
  )

  # Check that results are returned and count is correct
  expect_true(nrow(results) > 0)
  expect_lte(nrow(results), max_results_test)

  # Check column names
  expected_columns <- c(
    ".api",
    "pmid",
    "doi",
    ".title",
    ".abstract",
    ".authors",
    ".year",
    ".journal",
    ".pubtype",
    ".record_name",
    ".paperId",
    ".url",
    ".is_open_access",
    ".ids",
    ".references",
    ".citations",
    ".related"
  )
  expect_named(results, expected_columns, ignore.order = TRUE)

  # Check data types for key columns
  expect_type(results$.title, "character")
  expect_type(results$.abstract, "character")
  expect_type(results$.authors, "character")
  expect_type(results$.year, "integer")
  expect_type(results$.journal, "character")
  expect_type(results$.pubtype, "character")
  expect_type(results$.record_name, "character")
  expect_type(results$.paperId, "character")
  expect_type(results$.url, "character")
  expect_type(results$.is_open_access, "logical")
  expect_type(results$.ids, "list")
  expect_type(results$.references, "list")
  expect_type(results$.citations, "list")
  expect_type(results$.related, "list")
})

# Add a test for PubMed API-specific date format parsing in case `year_filter` is used
test_that("cr_parse_year_filter works correctly for year ranges", {
  # Example from Crossref documentation: "from-pub-date=2010-01-01,until-pub-date=2015-12-31"
  filter_str <- "2010-2015"
  parsed <- cr_parse_year_filter(filter_str)

  expect_equal(parsed$from, "2010-01-01")
  expect_equal(parsed$to, "2015-12-31")
})

test_that("cr_parse_year_filter works correctly for single year", {
  filter_str <- "2020"
  parsed <- cr_parse_year_filter(filter_str)

  expect_equal(parsed$from, "2020-01-01")
  expect_equal(parsed$to, "2020-12-31")
})
