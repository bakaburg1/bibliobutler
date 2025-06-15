# test-crossref.R

test_that("get_crossref_articles() returns expected structure for a query", {
  skip_on_cran()

  # Call the function with sample parameters - only request 5 results for faster tests
  results <- get_crossref_articles(
    query = "machine learning",
    max_results = 5, # reduced from 100
    year_filter = "2020",
    filters = list(type = "journal-article") # restrict to journal articles for more consistent results
  )

  # Check that results is a data frame
  expect_s3_class(results, "data.frame")

  # Check it has some rows (<= max_results is tested below)
  expect_gt(nrow(results), 0)
  expect_lte(nrow(results), 5) # reduced from 100

  # Get only processed fields for testing
  processed_results <- results |>
    dplyr::select(dplyr::starts_with("."))

  # Check it has at least the columns we expect
  expect_setequal(
    names(processed_results),
    c(
      ".record_name",
      ".paperId",
      ".title",
      ".abstract",
      ".year",
      ".authors",
      ".journal",
      ".pubtype",
      ".api",
      ".is_open_access",
      ".url",
      ".references",
      ".citations",
      ".related",
      ".ids"
    )
  )

  # Check if the publication year is 2020 for non-NA values
  # Rather than checking actual values, we'll just check if the year column
  # exists
  expect_true(any(names(processed_results) == ".year"))
  non_na_years <- processed_results$.year[!is.na(processed_results$.year)]

  # Only test years if there are non-NA values
  if (length(non_na_years) > 0) {
    expect_equal(non_na_years, rep("2020", length(non_na_years)))
  }

  # Check .ids is a list column with data frames
  expect_type(processed_results$.ids, "list")

  # Get first item to check structure
  first_ids <- processed_results$.ids[[1]]
  expect_s3_class(first_ids, "data.frame")

  # Check the names of columns in first ids data frame
  expect_named(first_ids, "DOI", ignore.order = TRUE)
})

test_that("get_crossref_articles() can fetch articles by ID", {
  skip_on_cran()

  # Test DOI - use a known reliable DOI for testing
  doi_id <- "10.1371/journal.pone.0266781"
  result_doi <- get_crossref_articles(ids = doi_id)

  expect_s3_class(result_doi, "data.frame")
  expect_equal(nrow(result_doi), 1)
  expect_equal(result_doi$.paperId[1], doi_id)

  # Check columns in the result
  processed_results <- result_doi |>
    dplyr::select(dplyr::starts_with("."))

  expect_setequal(
    names(processed_results),
    c(
      ".record_name",
      ".paperId",
      ".title",
      ".abstract",
      ".year",
      ".authors",
      ".journal",
      ".pubtype",
      ".api",
      ".is_open_access",
      ".url",
      ".references",
      ".citations",
      ".related",
      ".ids"
    )
  )

  # Check .ids is a list column with data frames
  expect_type(processed_results$.ids, "list")

  # Get first item to check structure
  first_ids <- processed_results$.ids[[1]]
  expect_s3_class(first_ids, "data.frame")

  # Check the names of columns in first ids data frame
  expect_named(first_ids, "DOI", ignore.order = TRUE)
})

test_that("get_crossref_linked() returns expected structure", {
  skip_on_cran()

  linked <- get_crossref_linked(ids = "10.1371/journal.pone.0266781")

  # Check that `linked` is a list with references
  expect_type(linked, "list")
  expect_named(linked, "references")

  # Ensure the element is a data frame (possibly empty)
  expect_s3_class(linked$references, "data.frame")

  # Check that the references data frame has the expected columns
  if (nrow(linked$references) > 0) {
    expect_setequal(names(linked$references), c("source_id", "linked_id"))
  }
})

test_that("get_crossref_linked() handles invalid or empty ID gracefully", {
  # Provide an invalid ID
  bad_id <- "notadoi"

  # Expect error messages about invalid IDs - the exact message may include details
  expect_error(get_crossref_linked(ids = bad_id))

  # Test with empty input
  expect_error(get_crossref_linked(character(0)))
})

test_that("get_crossref_linked() works with multiple DOIs", {
  skip_on_cran()

  # Test with multiple DOIs
  dois <- c("10.1371/journal.pone.0266781", "10.1037/0003-066X.59.1.29")
  linked_dois <- get_crossref_linked(ids = dois)
  expect_type(linked_dois, "list")
  expect_named(linked_dois, "references")
  expect_s3_class(linked_dois$references, "data.frame")

  # If there are results, check if both source IDs appear
  if (nrow(linked_dois$references) > 0) {
    # Use expect_in instead of expect_true(all(...))
    expect_in(
      tolower(linked_dois$references$source_id),
      tolower(dois)
    )
  }
})

test_that("cr_parse_year_filter handles various formats correctly", {
  # Test single year
  expect_equal(
    cr_parse_year_filter("2020"),
    list(from = "2020-01-01", to = "2020-12-31")
  )

  # Test year range
  expect_equal(
    cr_parse_year_filter("2015-2020"),
    list(from = "2015-01-01", to = "2020-12-31")
  )

  # Test open-ended ranges
  expect_equal(
    cr_parse_year_filter("2010-"),
    list(from = "2010-01-01", to = NULL)
  )
  expect_equal(
    cr_parse_year_filter("-2015"),
    list(from = NULL, to = "2015-12-31")
  )

  # Test invalid formats
  expect_warning(
    result <- cr_parse_year_filter("invalid"),
    "Could not parse year_filter"
  )
  expect_equal(result, list(from = NULL, to = NULL))
})

test_that("cr_prepare_ids handles various ID formats", {
  # Test DOI
  expect_equal(
    cr_prepare_ids("10.1371/journal.pone.0266781"),
    "10.1371/journal.pone.0266781"
  )

  # Test multiple DOIs
  expect_equal(
    cr_prepare_ids(c(
      "10.1371/journal.pone.0266781",
      "10.1037/0003-066X.59.1.29"
    )),
    c("10.1371/journal.pone.0266781", "10.1037/0003-066X.59.1.29")
  )

  # Test with NA values
  expect_equal(
    cr_prepare_ids(c("10.1371/journal.pone.0266781", NA)),
    "10.1371/journal.pone.0266781"
  )

  # Test empty input
  expect_error(
    cr_prepare_ids(character(0))
  )
})
