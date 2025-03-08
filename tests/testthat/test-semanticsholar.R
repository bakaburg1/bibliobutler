# test-semanticscholar.R

test_that(
  "get_semanticscholar_articles() returns expected structure for a query", {

  skip_on_cran()

  # Call the function with sample parameters
  results <- get_semanticscholar_articles(
    query = "machine learning",
    max_results = 100,
    year_filter = 2025
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
    ".url", ".ids"
  ))

  # Check if the publication year is 2025
  expect_true(all(processed_results$.year == 2025))

  # Check .ids is a data frame column
  expect_s3_class(processed_results$.ids, "data.frame")

  # Check if doi and semantic scholar id are present in .ids
  expect_contains(names(processed_results$.ids), c("doi", "semanticscholar"))
})

test_that("get_semanticscholar_articles() can fetch articles by ID", {
  skip_on_cran()

  # Provide known IDs (some real or mocked):
  sample_ids <- c(
    "649def34f8be52c8b66281af98ae884c09aef38b",
    "10.1145/3197026.3197040"
  )

  results <- get_semanticscholar_articles(
    ids = sample_ids,
    fields = c("citations", "references", "authors")  # Added authors field
  )

  expect_s3_class(results, "data.frame")
  # Might be fewer if some fail to fetch
  expect_equal(nrow(results), length(sample_ids))
  expect_true(all(results$.paperId != ""))          # Expect some non-empty IDs

  # Get only processed fields for testing
  processed_results <- results |>
    dplyr::select(dplyr::starts_with("."))

  # Check we have at least the same columns as before:
  expect_setequal(names(processed_results), c(
    ".record_name", ".paperId", ".title", ".abstract", ".year",
    ".authors", ".journal", ".pubtype", ".api", ".is_open_access",
    ".url", ".references", ".citations", ".ids"
  ))

  # Check if doi and semantic scholar id are present in .ids
  expect_contains(names(processed_results$.ids), c("doi", "semanticscholar"))
})

test_that("get_semanticscholar_linked() returns expected structure", {
  skip_on_cran()

  linked <- get_semanticscholar_linked(ids = "10.1145/3701201")

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
  "get_semanticscholar_linked() handles invalid or empty ID gracefully", {
  # skip_on_cran()

  # Provide an invalid ID
  bad_id <- "notadoi"

  # Expect warning messages about invalid IDs
  expect_message(
    linked_bad <- get_semanticscholar_linked(ids = bad_id),
    "Some IDs are not valid:"
  )
  expect_message(
    linked_bad <- get_semanticscholar_linked(ids = bad_id),
    "notadoi"
  )
  expect_message(
    linked_bad <- get_semanticscholar_linked(ids = bad_id),
    "No valid IDs passed"
  )

  # Expect it returns a list (though empty data frames)
  expect_type(linked_bad, "list")

  # Expect 0-row data frames since ID was invalid
  expect_equal(nrow(linked_bad$citations), 0)
  expect_equal(nrow(linked_bad$references), 0)
  expect_equal(nrow(linked_bad$related), 0)
})

test_that("get_semanticscholar_linked() works with IDs of different formats", {
  skip_on_cran()

  ids <- c(
    "0002dafbba9b1dfd434dcb00d6f7c8468ccdd89e",  # Semantic Scholar ID
    "10.62486/agsalud2025197"                    # DOI
  )

  linked <- get_semanticscholar_linked(ids = ids)

  # We expect a list with elements citations, references, related (even if 0
  # rows)
  expect_type(linked, "list")
  expect_named(linked, c("citations", "references", "related"))

  # Check each is a data frame (though it could be empty)
  purrr::walk(linked, ~ expect_s3_class(.x, "data.frame"))
})

test_that("Two different versions of the same ID return the same article", {
  skip_on_cran()

  s2_id   <- "0002dafbba9b1dfd434dcb00d6f7c8468ccdd89e"
  doi_id  <- "10.62486/agsalud2025197"

  result_s2  <- get_semanticscholar_articles(ids = s2_id)
  result_doi <- get_semanticscholar_articles(ids = doi_id)

  # Basic checks: both should return exactly 1 row
  expect_equal(nrow(result_s2), 1)
  expect_equal(nrow(result_doi), 1)

  # Check if some key fields match
  expect_equal(result_s2$.paperId, result_doi$.paperId)
  expect_equal(result_s2$.title, result_doi$.title)
  expect_equal(result_s2$.year, result_doi$.year)

})
