# test-openalex.R

test_that(
  "get_openalex_articles() returns expected structure for a query", {

  skip_on_cran()

  # Call the function with sample parameters
  results <- get_openalex_articles(
    query = "machine learning",
    max_results = 100,
    year_filter = "2025"
  )

  # Check that results is a data frame
  expect_s3_class(results, "data.frame")

  # Check it has some rows (<= max_results is tested below)
  expect_gt(nrow(results), 0)
  expect_lte(nrow(results), 100)

  # Get only processed fields for testing
  processed_results <- results |> 
    dplyr::select(dplyr::starts_with("."))

  # Check it has at least the columns we expect
  expect_setequal(names(processed_results), c(
    ".record_name", ".paperId", ".title", ".abstract", ".year",
    ".authors", ".journal", ".pubtype", ".api", ".is_open_access",
    ".url", ".references", ".related", ".ids"
  ))

  # Check if the publication year is 2025
  expect_true(all(processed_results$.year == 2025))

  # Check .ids is a data frame column
  expect_s3_class(processed_results$.ids, "data.frame")

  # Check if openalex and doi are present in .ids
  expect_contains(names(processed_results$.ids), c("doi", "openalex"))
})

test_that("get_openalex_articles() can fetch articles by ID", {
  skip_on_cran()

  # Test OpenAlex ID
  openalex_id <- "W2741809807"  # OpenAlex ID without URL prefix
  result_oa <- get_openalex_articles(ids = openalex_id)

  expect_s3_class(result_oa, "data.frame")
  expect_equal(nrow(result_oa), 1)
  expect_true(all(result_oa$.paperId != ""))

  # Test DOI
  doi_id <- "10.1371/journal.pone.0266781"
  result_doi <- get_openalex_articles(ids = doi_id)

  expect_s3_class(result_doi, "data.frame")
  expect_equal(nrow(result_doi), 1)
  expect_true(all(result_doi$.paperId != ""))

  # Check columns in both results
  for (results in list(result_oa, result_doi)) {
    processed_results <- results |> 
      dplyr::select(dplyr::starts_with("."))

    expect_setequal(names(processed_results), c(
      ".record_name", ".paperId", ".title", ".abstract", ".year",
      ".authors", ".journal", ".pubtype", ".api", ".is_open_access",
      ".url", ".references", ".related", ".ids"
    ))

    expect_contains(names(processed_results$.ids), c("doi", "openalex"))
  }
})

test_that("get_openalex_linked() returns expected structure", {
  skip_on_cran()

  linked <- get_openalex_linked(ids = "10.1371/journal.pone.0266781")

  # Check that `linked` is a list of length 3
  expect_type(linked, "list")
  expect_setequal(names(linked), c("citations", "references", "related"))

  # Ensure each element is a data frame (possibly empty)
  expect_s3_class(linked$citations, "data.frame")
  expect_s3_class(linked$references, "data.frame")
  expect_s3_class(linked$related, "data.frame")

  # Check that the citations data frame has the expected columns
  # (source_id, linked_id) if it's not empty
  if (nrow(linked$citations) > 0) {
    expect_setequal(names(linked$citations), c("source_id", "linked_id"))
  }

  # Check that the references data frame has the expected columns
  if (nrow(linked$references) > 0) {
    expect_setequal(names(linked$references), c("source_id", "linked_id"))
  }

  # Check that the related data frame has the expected columns
  if (nrow(linked$related) > 0) {
    expect_setequal(names(linked$related), c("source_id", "linked_id"))
  }
})

test_that(
  "get_openalex_linked() handles invalid or empty ID gracefully", {

  # Provide an invalid ID
  bad_id <- "notadoi"

  # Expect error messages about invalid IDs
  expect_error(
    get_openalex_linked(ids = bad_id),
    "No valid IDs after conversion"
  )

  # Test with empty input
  expect_error(
    get_openalex_linked(character(0)),
    "All IDs are NA or empty"
  )
})

test_that("get_openalex_linked() handles single ID type", {
  skip_on_cran()

  # Test with just OpenAlex ID
  linked_oa <- get_openalex_linked(ids = "W2741809807")
  expect_type(linked_oa, "list")
  expect_named(linked_oa, c("citations", "references", "related"))
  purrr::walk(linked_oa, ~ expect_s3_class(.x, "data.frame"))

  # Test with just DOI
  linked_doi <- get_openalex_linked(ids = "10.1371/journal.pone.0266781")
  expect_type(linked_doi, "list")
  expect_named(linked_doi, c("citations", "references", "related"))
  purrr::walk(linked_doi, ~ expect_s3_class(.x, "data.frame"))
})

test_that("Two different versions of the same ID return the same article", {
  skip_on_cran()

  openalex_id <- "W2741809807"  # Without URL prefix
  doi_id      <- "10.1371/journal.pone.0266781"

  result_oa  <- get_openalex_articles(ids = openalex_id)
  result_doi <- get_openalex_articles(ids = doi_id)

  # Basic checks: both should return exactly 1 row
  expect_equal(nrow(result_oa), 1)
  expect_equal(nrow(result_doi), 1)

  # Check if some key fields match
  expect_equal(result_oa$.paperId, result_doi$.paperId)
  expect_equal(result_oa$.title, result_doi$.title)
  expect_equal(result_oa$.year, result_doi$.year)
})

test_that("oa_parse_year_filter handles various formats correctly", {
  # Test single year
  expect_equal(
    oa_parse_year_filter("2020"),
    list(from = "2020-01-01", to = "2020-12-31")
  )

  # Test year range
  expect_equal(
    oa_parse_year_filter("2015-2020"),
    list(from = "2015-01-01", to = "2020-12-31")
  )

  # Test open-ended ranges
  expect_equal(
    oa_parse_year_filter("2010-"),
    list(from = "2010-01-01", to = NULL)
  )
  expect_equal(
    oa_parse_year_filter("-2015"),
    list(from = NULL, to = "2015-12-31")
  )

  # Test invalid formats
  expect_warning(
    result <- oa_parse_year_filter("invalid"),
    "Could not parse year_filter"
  )
  expect_equal(result, list(from = NULL, to = NULL))
})

test_that("oa_prepare_ids handles various ID formats", {
  # Test OpenAlex ID
  result <- oa_prepare_ids("W2741809807")

  expect_equal(
    result,
    list(openalex = "W2741809807")
  )

  # Test DOI
  expect_equal(
    oa_prepare_ids("10.1371/journal.pone.0266781"),
    list(doi = "10.1371/journal.pone.0266781")
  )

  # Test invalid ID
  expect_error(
    oa_prepare_ids("invalid"),
    "No valid IDs after conversion"
  )

  # Test mixed ID types
  expect_error(
    oa_prepare_ids(c(
      "W2741809807",
      "10.1371/journal.pone.0266781"
    )),
    "OpenAlex can accept only one ID type at once"
  )
}) 