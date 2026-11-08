test_that("large Crossref pagination respects exact result bounds", {
  calls <- 0L
  mock <- function(url, headers, params, timeout) {
    calls <<- calls + 1L
    rows <- params$rows
    start <- (calls - 1L) * 25L + 1L
    works <- lapply(seq_len(rows), function(i) {
      list(title = list(paste("Work", start + i - 1L)),
           DOI = sprintf("10.1000/work-%d", start + i - 1L))
    })
    list(message = list(items = works,
                        "next-cursor" = paste0("page-", calls + 1L)))
  }
  works <- search_crossref("topic", max_results = 103L, page_size = 25L,
                           requester = mock, min_interval = 0)
  expect_length(works, 103L)
  expect_equal(calls, 5L)
  expect_identical(works[[103L]]$doi, "10.1000/work-103")
})

test_that("OpenAlex bounded cursor workload preserves exact record count", {
  calls <- 0L
  mock <- function(url, headers, params, timeout) {
    calls <<- calls + 1L
    rows <- params[["per-page"]]
    works <- lapply(seq_len(rows), function(i) {
      list(title = paste("Work", calls, i))
    })
    list(results = works, meta = list(next_cursor = paste0("cursor-", calls)))
  }
  works <- search_openalex("topic", api_key = "test-key",
                           max_results = 78L, page_size = 20L,
                           requester = mock, min_interval = 0)
  expect_length(works, 78L)
  expect_equal(calls, 4L)
})

test_that("cursor pages without usable entries fail instead of looping", {
  bad_crossref <- function(url, headers, params, timeout) {
    list(message = list(items = list("invalid"),
                        "next-cursor" = "next"))
  }
  expect_error(search_crossref(
    "topic", max_results = 5L, page_size = 1L,
    requester = bad_crossref, min_interval = 0
  ), "no usable works")

  bad_openalex <- function(url, headers, params, timeout) {
    list(results = list("invalid"), meta = list(next_cursor = "next"))
  }
  expect_error(search_openalex(
    "topic", api_key = "test", max_results = 5L, page_size = 1L,
    requester = bad_openalex, min_interval = 0
  ), "no usable works")

  bad_scopus <- function(url, headers, params, timeout) {
    list("search-results" = list(
      entry = list("invalid"), cursor = list("@next" = "next")
    ))
  }
  expect_error(search_articles(
    "topic", sources = "scopus", scopus_api_key = "test-key",
    max_results_per_source = 5L, page_size = 1L,
    requester = bad_scopus, min_interval = 0
  ), "no usable entries")
})

test_that("two distinct empty Scopus cursors fail with no progress", {
  calls <- 0L
  mock <- function(url, headers, params, timeout) {
    calls <<- calls + 1L
    list("search-results" = list(entry = list(),
      cursor = list("@next" = paste0("next-", calls))))
  }
  expect_error(fetch_all_from_scopus("topic", "test-key", requester = mock),
               "no progress")
  expect_equal(calls, 2L)
})

test_that("partial provider failures preserve successful sources", {
  mock <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) {
      stop("crossref outage contact=private@example.org")
    }
    list(results = list(list(title = "OpenAlex survives",
                             doi = "10.1000/survives")), meta = list())
  }
  result <- search_articles(
    "topic", sources = c("crossref", "openalex"),
    on_error = "skip", crossref_mailto = "private@example.org",
    requester = mock, min_interval = 0
  )
  expect_identical(result$sources$status, c("failed", "ok"))
  expect_length(result$articles, 1L)
  expect_equal(nrow(result$errors), 1L)
  expect_false(grepl("private@example.org",
                     result$errors$message[[1L]], fixed = TRUE))
  # The standard HTTP adapter intentionally provides a generic public
  # error message. No replacement marker is necessary if a secret was never
  # present in that message.
  expect_match(result$errors$message[[1L]],
               "Failed to retrieve or decode data", fixed = TRUE)
})

test_that("explicit provider errors have credential values redacted", {
  # An already-classified request condition is propagated by .call_json;
  # this exercises the redaction path when its message contains a secret.
  provider_condition <- structure(
    list(
      message = "request failed for private@example.org",
      call = NULL
    ),
    class = c("abstract_extract_request_error", "error", "condition")
  )
  mock <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) {
      stop(provider_condition)
    }
    list(results = list(), meta = list())
  }
  result <- search_articles(
    "topic", sources = c("crossref", "openalex"),
    on_error = "skip", crossref_mailto = "private@example.org",
    requester = mock, min_interval = 0
  )
  expect_identical(result$sources$status, c("failed", "ok"))
  expect_match(result$errors$message[[1L]],
               "[redacted]", fixed = TRUE)
  expect_false(grepl("private@example.org",
                     result$errors$message[[1L]], fixed = TRUE))
})
