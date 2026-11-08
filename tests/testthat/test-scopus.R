test_that("single-page Scopus search uses expected request", {
  callback <- function(url, headers, params, timeout) {
    expect_equal(url, "https://api.elsevier.com/content/search/scopus")
    expect_equal(headers[["X-ELS-APIKey"]], "secret")
    expect_equal(params$query, "forecasting")
    expect_equal(params$count, 10L)
    expect_equal(timeout, 5)
    list("search-results" = list(entry = list()))
  }
  expect_type(fetch_from_scopus(" forecasting ", " secret ",
                                max_results = 10L, timeout = 5,
                                requester = callback), "list")
  expect_error(fetch_from_scopus("", "key"), "query")
  expect_error(fetch_from_scopus("topic", ""), "api_key")
  expect_error(fetch_from_scopus("topic", "key", max_results = 26), "max_results")
  expect_error(fetch_from_scopus("topic", "key", timeout = 0), "timeout")
})

test_that("cursor pagination collects entries with author and date filters", {
  calls <- list()
  callback <- function(url, headers, params, timeout) {
    calls[[length(calls) + 1L]] <<- params
    if (length(calls) == 1L) {
      return(list("search-results" = list(
        entry = list(list("dc:title" = "First")),
        cursor = list("@next" = "page-2")
      )))
    }
    list("search-results" = list(
      entry = list(list("dc:title" = "Second")), cursor = list()
    ))
  }
  entries <- fetch_all_from_scopus(
    "topic", "key", author = "Ada", start_date = "2020-01-01",
    end_date = "2026-01-01", requester = callback
  )
  expect_length(entries, 2L)
  expect_equal(calls[[1L]]$query, "topic AND AUTHOR(Ada)")
  expect_equal(calls[[1L]]$date, "2020-01-01 to 2026-01-01")
  expect_equal(calls[[2L]]$cursor, "page-2")
})

test_that("malformed pages and repeated cursors fail visibly", {
  expect_error(fetch_all_from_scopus("topic", "key", start_date = "2020-01-01"),
               "provided together")
  bad <- function(url, headers, params, timeout) list(invalid = TRUE)
  expect_error(fetch_all_from_scopus("topic", "key", requester = bad),
               "search-results")
  repeat_page <- function(url, headers, params, timeout) {
    list("search-results" = list(entry = list(), cursor = list("@next" = "same")))
  }
  expect_error(fetch_all_from_scopus("topic", "key", requester = repeat_page),
               "repeated pagination")
})

test_that("Scopus entries normalize into articles", {
  input <- list(list(
    "dc:title" = "Example",
    "dc:description" = "An abstract",
    "prism:coverDate" = "2026-02-03",
    "prism:doi" = "10.1000/paper",
    author = list(list(authname = "Alice"), list(authname = "Bob"),
                  list(other = "ignored"), "invalid")
  ))
  articles <- process_scopus_entries(input)
  expect_s3_class(articles[[1L]], "Article")
  expect_equal(articles[[1L]]$authors, c("Alice", "Bob"))
  expect_equal(articles[[1L]]$doi, "10.1000/paper")
  expect_length(process_scopus_response(
    list("search-results" = list(entry = list()))), 0L)
  expect_error(process_scopus_response(list("search-results" = list(
    entry = list(invalid = 1)))), "entry")
})

test_that("request errors retain endpoint but never expose API keys", {
  callback <- function(url, headers, params, timeout) stop("network unavailable")
  err <- tryCatch(fetch_from_scopus("topic", "secret", requester = callback),
                  error = identity)
  expect_s3_class(err, "abstract_extract_request_error")
  expect_s3_class(err, "error")
  expect_null(conditionCall(err))
  expect_s3_class(err$parent, "simpleError")
  expect_identical(conditionMessage(err$parent), "network unavailable")
  expect_equal(err$source, "https://api.elsevier.com/content/search/scopus")
  expect_false(grepl("secret", conditionMessage(err), fixed = TRUE))
})
