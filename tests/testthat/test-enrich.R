test_that("enrichment fetches missing abstracts and preserves metadata", {
  calls <- character()
  callback <- function(url, headers, params, timeout) {
    calls <<- c(calls, url)
    expect_equal(timeout, 7)
    list(message = list(abstract = "<jats:p>Crossref result</jats:p>"))
  }
  original <- Article(
    title = "Scopus title", authors = c("Ada", "Grace"),
    publication_date = "2025-01-01", doi = "10.1000/paper"
  )
  articles <- list(first = original, second = Article(
    title = "Already complete", doi = "10.1000/different", abstract = "Keep me"
  ))
  enriched <- enrich_articles_with_crossref(
    articles, requester = callback, timeout = 7
  )
  expect_identical(names(enriched), names(articles))
  expect_equal(enriched$first$abstract, "<jats:p>Crossref result</jats:p>")
  expect_equal(enriched$first$title, "Scopus title")
  expect_identical(enriched$first$authors, c("Ada", "Grace"))
  expect_equal(enriched$second$abstract, "Keep me")
  expect_null(original$abstract)
  expect_length(calls, 1L)
})

test_that("duplicate DOIs use one request and missing responses are cached", {
  calls <- 0L
  callback <- function(url, headers, params, timeout) {
    calls <<- calls + 1L
    list(message = list())
  }
  articles <- list(
    Article(doi = " 10.1000/Duplicate "),
    Article(doi = "10.1000/duplicate"),
    Article(doi = "10.1000/duplicate", abstract = "  ")
  )
  enriched <- enrich_articles_with_crossref(articles, requester = callback)
  expect_equal(calls, 1L)
  expect_null(enriched[[1L]]$abstract)
  expect_equal(enriched[[3L]]$abstract, "  ")
})

test_that("overwrite never destroys text when Crossref has no abstract", {
  callback <- function(url, headers, params, timeout) list(message = list())
  article <- Article(doi = "10.1000/example", abstract = "Scopus original")
  enriched <- enrich_articles_with_crossref(
    list(article), overwrite = TRUE, requester = callback
  )
  expect_identical(enriched[[1L]]$abstract, "Scopus original")
})

test_that("overwrite can explicitly replace existing text", {
  callback <- function(url, headers, params, timeout) {
    list(message = list(abstract = "Crossref replacement"))
  }
  article <- Article(doi = "10.1000/example", abstract = "Scopus original")
  expect_equal(enrich_articles_with_crossref(
    list(article), overwrite = TRUE, requester = callback
  )[[1L]]$abstract, "Crossref replacement")
})

test_that("missing and blank DOIs never generate requests", {
  callback <- function(url, headers, params, timeout) {
    fail("Unexpected network call")
  }
  articles <- list(Article(), Article(doi = "   "))
  expect_identical(
    enrich_articles_with_crossref(articles, requester = callback), articles
  )
  expect_identical(
    enrich_articles_with_crossref(list(), requester = callback), list()
  )
})

test_that("error policies stop warn or skip with original metadata intact", {
  callback <- function(url, headers, params, timeout) stop("offline")
  article <- Article(title = "Unchanged", doi = "10.1000/failure")
  expect_error(enrich_articles_with_crossref(
    list(article), requester = callback
  ), "Failed to retrieve")
  expect_warning(
    warning_result <- enrich_articles_with_crossref(
      list(article), on_error = "warn", requester = callback
    ),
    "Crossref lookup failed"
  )
  expect_identical(warning_result[[1L]], article)
  expect_silent(
    skip_result <- enrich_articles_with_crossref(
      list(article), on_error = "skip", requester = callback
    )
  )
  expect_identical(skip_result[[1L]], article)
})

test_that("input validation does not depend on a network connection", {
  callback <- function(url, headers, params, timeout) list(message = list())
  expect_error(enrich_articles_with_crossref(list(list()), requester = callback),
               "Article")
  expect_error(enrich_articles_with_crossref("invalid", requester = callback),
               "list")
  expect_error(enrich_articles_with_crossref(list(), overwrite = NA,
                                             requester = callback), "overwrite")
  expect_error(enrich_articles_with_crossref(list(), timeout = 0,
                                             requester = callback), "timeout")
  expect_error(enrich_articles_with_crossref(list(), requester = "not callable"),
               "requester")
  expect_error(enrich_articles_with_crossref(list(), on_error = "unknown",
                                             requester = callback), "arg")
})
