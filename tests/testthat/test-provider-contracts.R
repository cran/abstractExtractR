# Frozen, representative provider JSON schemas; not claimed to be verbatim live responses.
fixture <- function(name) {
  path <- testthat::test_path("..", "fixtures", paste0(name, ".json"))
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

test_that("Crossref contract preserves DOI, title, authors and dates", {
  payload <- fixture("crossref-works")
  articles <- process_crossref_response(payload)
  expect_length(articles, 2L)
  expect_s3_class(articles[[1L]], "Article")
  expect_identical(articles[[1L]]$title, "A study of Gaussian processes")
  expect_identical(articles[[1L]]$doi, "10.1000/example")
  expect_identical(articles[[1L]]$authors, c("Ada Lovelace", "Grace Hopper"))
  expect_identical(articles[[1L]]$publication_date, "2024-04-12")
  expect_identical(articles[[1L]]$abstract, "<jats:p>Measured results</jats:p>")
  expect_null(articles[[2L]]$abstract)
  expect_identical(articles[[2L]]$publication_date, "2023")
})

test_that("OpenAlex contract reconstructs inverted abstracts and optional fields", {
  articles <- process_openalex_response(fixture("openalex-works"))
  expect_length(articles, 2L)
  expect_identical(articles[[1L]]$doi, "10.1000/example")
  expect_identical(articles[[1L]]$abstract, "Measured results")
  expect_identical(articles[[1L]]$authors, c("Ada Lovelace", "Grace Hopper"))
  expect_identical(articles[[2L]]$title, "Work without DOI")
  expect_null(articles[[2L]]$doi)
  expect_null(articles[[2L]]$abstract)
})

test_that("Scopus contract preserves raw author sequence and optional metadata", {
  articles <- process_scopus_response(fixture("scopus-search"))
  expect_length(articles, 2L)
  expect_identical(articles[[1L]]$authors,
                   c("Lovelace, Ada", "Hopper, Grace"))
  expect_identical(articles[[1L]]$doi, "10.1000/example")
  expect_null(articles[[2L]]$doi)
  expect_null(articles[[2L]]$abstract)
})

test_that("Crossref and OpenAlex fixtures reconcile conservatively", {
  crossref <- process_crossref_response(fixture("crossref-works"))
  openalex <- process_openalex_response(fixture("openalex-works"))
  result <- deduplicate_articles(c(crossref, openalex), report = TRUE)
  expect_equal(length(result$articles), 3L)
  expect_equal(result$mapping$output_index[[3L]], 1L)
  expect_identical(result$articles[[1L]]$abstract,
                   "<jats:p>Measured results</jats:p>")
  expect_true(any(result$conflicts$field == "abstract"))
})
