test_that("Crossref search sends supported parameters", {
  callback <- function(url, headers, params, timeout) {
    expect_identical(url, "https://api.crossref.org/works")
    expect_equal(params[["query.bibliographic"]], "gaussian processes")
    expect_equal(params$rows, 2L)
    expect_equal(params$filter,
                 "type:journal-article,from-pub-date:2020-01-01,until-pub-date:2026-10-01")
    expect_equal(params$mailto, "research@example.org")
    expect_equal(params$cursor, "*")
    list(message = list(items = list()))
  }
  response <- fetch_from_crossref(
    "gaussian processes", rows = 2L, start_date = "2020-01-01",
    end_date = "2026-10-01", work_type = "journal-article",
    mailto = "research@example.org", cursor = "*", requester = callback
  )
  expect_identical(response$message$items, list())
})

test_that("crossref entries preserve incomplete dates and author order", {
  entries <- list(
    list(title = list("A study"), DOI = "10.1000/a",
         abstract = "<jats:p>Content</jats:p>",
         author = list(list(given = "Ada", family = "Lovelace"),
                       list(name = "Research Collective"),
                       list(given = "Grace", family = "Hopper")),
         published = list("date-parts" = list(list(2024, 5)))),
    list(title = list("Second"),
         published = list("date-parts" = list(list(2020)))),
    "invalid entry"
  )
  result <- process_crossref_entries(entries)
  expect_length(result, 2L)
  expect_s3_class(result[[1L]], "Article")
  expect_identical(result[[1L]]$authors,
                   c("Ada Lovelace", "Research Collective", "Grace Hopper"))
  expect_identical(result[[1L]]$publication_date, "2024-05")
  expect_identical(result[[2L]]$publication_date, "2020")
  expect_identical(result[[1L]]$abstract, "<jats:p>Content</jats:p>")
})

test_that("cursor search respects result limit and progresses safely", {
  requests <- list()
  callback <- function(url, headers, params, timeout) {
    requests[[length(requests) + 1L]] <<- params
    if (length(requests) == 1L) {
      return(list(message = list(
        items = list(list(title = list("One")),
                     list(title = list("Two"))),
        "next-cursor" = "second"
      )))
    }
    list(message = list(items = list(list(title = list("Three"))),
                        "next-cursor" = "third"))
  }
  papers <- search_crossref(
    "a topic", max_results = 3L, page_size = 2L,
    requester = callback, min_interval = 0
  )
  expect_length(papers, 3L)
  expect_identical(vapply(papers, function(p) p$title, character(1)),
                   c("One", "Two", "Three"))
  expect_equal(requests[[1L]]$rows, 2L)
  expect_equal(requests[[2L]]$rows, 1L)
  expect_equal(requests[[2L]]$cursor, "second")
})

test_that("short pages stop searching and bad cursors are rejected", {
  short <- function(url, headers, params, timeout) {
    list(message = list(items = list(list(title = list("Only"))),
                        "next-cursor" = "ignored"))
  }
  expect_length(search_crossref(
    "topic", max_results = 5L, page_size = 2L,
    requester = short, min_interval = 0), 1L)
  repeated <- function(url, headers, params, timeout) {
    list(message = list(items = list(list(title = list("A"))),
                        "next-cursor" = "*"))
  }
  expect_error(search_crossref(
    "topic", max_results = 2L, page_size = 1L,
    requester = repeated, min_interval = 0), "repeated")
})

test_that("invalid parameters fail without network calls", {
  never <- function(url, headers, params, timeout) {
    fail("Unexpected HTTP call")
  }
  expect_error(fetch_from_crossref("", requester = never), "query")
  expect_error(fetch_from_crossref("x", rows = 0, requester = never), "rows")
  expect_error(fetch_from_crossref("x", start_date = "2026-02-30",
                                   requester = never), "date")
  expect_error(fetch_from_crossref("x", start_date = "2026-10-01",
                                   end_date = "2020-01-01",
                                   requester = never), "after")
  expect_error(fetch_from_crossref("x", work_type = "Journal Article",
                                   requester = never), "work_type")
  expect_error(search_crossref("x", page_size = 1001L, requester = never),
               "page_size")
  expect_error(search_crossref("x", max_tries = 11L, requester = never),
               "max_tries")
  expect_error(process_crossref_response(list()), "message")
  expect_error(process_crossref_response(list(message = list())), "items")
})
