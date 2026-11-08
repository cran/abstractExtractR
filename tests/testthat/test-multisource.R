test_that("unified search reconciles DOI and preserves source evidence", {
  mock <- function(url, headers, params, timeout) {
    if (grepl("crossref.org", url, fixed = TRUE)) {
      return(list(message = list(items = list(
        list(title = list("From Crossref"), DOI = "10.1000/One",
             published = list("date-parts" = list(list(2024))))
      ))))
    }
    list(results = list(
      list(doi = "https://doi.org/10.1000/one",
           abstract_inverted_index = list(Abstract = list(0L)),
           publication_date = "2024-06-10")
    ), meta = list())
  }
  result <- search_articles(
    "example", requester = mock, min_interval = 0
  )
  expect_length(result$articles, 1L)
  expect_identical(result$articles[[1L]]$title, "From Crossref")
  expect_identical(result$articles[[1L]]$abstract, "Abstract")
  expect_identical(result$articles[[1L]]$publication_date, "2024-06-10")
  expect_identical(result$provenance$source, c("crossref", "openalex"))
  expect_identical(result$provenance$output_index, c(1L, 1L))
  expect_identical(result$sources$retrieved, c(1L, 1L))
  expect_identical(result$sources$contributed, c(1L, 0L))
  expect_equal(nrow(result$conflicts), 0L)
})

test_that("failed source can be skipped without losing valid evidence", {
  mock <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) stop("temporary failure")
    list(results = list(list(title = "OpenAlex paper")), meta = list())
  }
  output <- search_articles("topic", requester = mock, min_interval = 0,
                            on_error = "skip")
  expect_length(output$articles, 1L)
  expect_identical(output$sources$status, c("failed", "ok"))
  expect_identical(output$errors$source, "crossref")
  expect_error(search_articles(
    "topic", requester = mock, min_interval = 0,
    on_error = "stop"
  ), "crossref search failed")
})

test_that("Scopus has explicit bounded cursor retrieval", {
  calls <- list()
  mock <- function(url, headers, params, timeout) {
    calls[[length(calls) + 1L]] <<- params
    if (length(calls) == 1L) {
      return(list("search-results" = list(
        entry = list(list("dc:title" = "One"),
                     list("dc:title" = "Two")),
        cursor = list("@next" = "next")
      )))
    }
    list("search-results" = list(
      entry = list(list("dc:title" = "Three")), cursor = list()
    ))
  }
  res <- search_articles(
    "topic", sources = "scopus", scopus_api_key = "my-key",
    max_results_per_source = 3L, page_size = 2L,
    requester = mock, min_interval = 0
  )
  expect_length(res$articles, 3L)
  expect_identical(calls[[1L]]$count, 2L)
  expect_identical(calls[[2L]]$count, 1L)
  expect_identical(calls[[2L]]$cursor, "next")
})

test_that("conflicts retain source provenance", {
  mock <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) {
      return(list(message = list(items = list(
        list(title = list("First"), DOI = "10.1000/A")
      ))))
    }
    list(results = list(list(title = "Second", doi = "10.1000/a")),
         meta = list())
  }
  res <- search_articles("topic", requester = mock, min_interval = 0)
  expect_equal(nrow(res$conflicts), 1L)
  expect_identical(res$conflicts$source, "openalex")
  expect_identical(res$conflicts$field, "title")
})

test_that("invalid configurations fail without HTTP", {
  never <- function(url, headers, params, timeout) fail("unexpected HTTP")
  expect_error(search_articles("", requester = never), "query")
  expect_error(search_articles("x", sources = c("crossref", "crossref"),
                               requester = never), "unique")
  expect_error(search_articles("x", page_size = 26L,
                               requester = never), "page_size")
  expect_error(search_articles("x", sources = "scopus",
                               requester = never), "scopus_api_key")
  expect_error(search_articles("x", sources = "openalex",
      max_results_per_source = 50L, page_size = 25L, requester = never),
      "api_key")
})
