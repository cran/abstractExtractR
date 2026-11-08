test_that("history tracks DOI additions, removals and metadata drift", {
  plan <- new_search_plan("topic", sources = "crossref")
  make <- function(works, when) {
    mock <- function(url, headers, params, timeout) {
      list(message = list(items = works))
    }
    run_search_plan(plan, requester = mock, clock = function() {
      as.POSIXct(when, tz = "UTC")
    })
  }
  a <- make(list(
    list(title = list("Old"), DOI = "10.1000/a"),
    list(title = list("Removed"), DOI = "10.1000/b"),
    list(title = list("No DOI"))
  ), "2026-10-01 10:00:00")
  b <- make(list(
    list(title = list("Changed"), DOI = "10.1000/a"),
    list(title = list("Added"), DOI = "10.1000/c")
  ), "2026-10-02 10:00:00")
  history <- compare_search_history(list(early = a, later = b))
  expect_s3_class(history, "SearchHistory")
  expect_equal(history$transitions$added, 1L)
  expect_equal(history$transitions$removed, 1L)
  expect_equal(history$transitions$modified_articles, 1L)
  expect_equal(history$transitions$unmatched_before, 1L)
  expect_true(history$transitions$comparable)
  expect_setequal(history$events$change_type, c("added", "removed", "modified"))
  expect_identical(history$runs$label, c("early", "later"))
})

test_that("empty searches produce stable schemas and safe reports", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  snap <- run_search_plan(plan, requester = mock)
  history <- compare_search_history(list(snap, snap))
  expect_equal(nrow(history$events), 0L)
  expect_equal(nrow(history$unmatched), 0L)
  expect_true(is.na(history$runs$mean_completeness[[1L]]))
  directory <- tempfile("history-test-")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  write_search_history_report(history, directory)
  expect_true(file.exists(file.path(directory, "transitions.csv")))
  expect_true(file.exists(file.path(directory, "README.md")))
  expect_error(write_search_history_report(history, directory), "already exists")
})

test_that("reports omit detailed values unless explicitly requested", {
  plan <- new_search_plan("topic", sources = "crossref")
  make <- function(title) {
    mock <- function(url, headers, params, timeout) {
      list(message = list(items = list(
        list(title = list(title), DOI = "10.1000/a")
      )))
    }
    run_search_plan(plan, requester = mock)
  }
  history <- compare_search_history(list(make("=unsafe"), make("Updated")))
  safe_dir <- tempfile("safe-report-")
  full_dir <- tempfile("full-report-")
  on.exit(unlink(c(safe_dir, full_dir), recursive = TRUE), add = TRUE)
  write_search_history_report(history, safe_dir)
  safe <- read.csv(file.path(safe_dir, "events.csv"))
  expect_true(is.na(safe$before[[1L]]) || !nzchar(safe$before[[1L]]))
  write_search_history_report(history, full_dir, include_values = TRUE)
  full <- read.csv(file.path(full_dir, "events.csv"))
  expect_identical(full$before[[1L]], "'=unsafe")
})

test_that("history detects incomparable plans and invalid ordering", {
  make <- function(query, when) {
    plan <- new_search_plan(query, sources = "crossref")
    mock <- function(url, headers, params, timeout) {
      list(message = list(items = list()))
    }
    run_search_plan(plan, requester = mock, clock = function() {
      as.POSIXct(when, tz = "UTC")
    })
  }
  a <- make("first", "2026-10-01 00:00:00")
  b <- make("second", "2026-10-02 00:00:00")
  h <- compare_search_history(list(a, b))
  expect_false(h$transitions$comparable)
  expect_error(compare_search_history(list(b, a)), "nondecreasing")
  expect_error(compare_search_history(list(a)), "at least two")
  expect_error(compare_search_history(list(a, b), labels = c("x", "x")),
               "unique")
})
