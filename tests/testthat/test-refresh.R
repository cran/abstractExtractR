test_that("refresh reruns the plan and publishes a complete evidence bundle", {
  plan <- new_search_plan("topic", sources = "crossref")
  make_mock <- function(works) {
    function(url, headers, params, timeout) {
      list(message = list(items = works))
    }
  }
  previous <- run_search_plan(
    plan, requester = make_mock(list(
      list(title = list("Old"), DOI = "10.1000/a")
    )),
    clock = function() as.POSIXct("2026-10-01 10:00:00", tz = "UTC")
  )
  destination <- tempfile("refresh-evidence-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  refreshed <- refresh_search_snapshot(
    previous, destination,
    requester = make_mock(list(
      list(title = list("Updated"), DOI = "10.1000/a"),
      list(title = list("Added"), DOI = "10.1000/b")
    )),
    clock = function() as.POSIXct("2026-10-17 10:00:00", tz = "UTC"),
    sleep = function(...) NULL
  )
  expect_s3_class(refreshed, "SearchRefresh")
  expect_identical(refreshed$history$transitions$added, 1L)
  expected <- c(
    "before.rds", "after.rds", "before-manifest.json",
    "after-manifest.json", "report", "README.md"
  )
  expect_true(all(file.exists(file.path(destination, expected))))
  expect_identical(read_search_snapshot(file.path(destination, "before.rds")),
                   previous)
  expect_identical(read_search_snapshot(file.path(destination, "after.rds")),
                   refreshed$snapshot)
  expect_error(refresh_search_snapshot(previous, destination,
                                       requester = make_mock(list())),
               "already exists")
})

test_that("failed refresh never publishes an incomplete directory", {
  plan <- new_search_plan("topic", sources = "crossref")
  empty <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  previous <- run_search_plan(plan, requester = empty)
  destination <- tempfile("refresh-failure-")
  failure <- function(url, headers, params, timeout) {
    stop("network unavailable")
  }
  expect_error(refresh_search_snapshot(previous, destination,
                                       requester = failure), "search failed")
  expect_false(dir.exists(destination))
})

test_that("partial provider failures remain explicit in refresh history", {
  plan <- new_search_plan("topic", sources = c("crossref", "openalex"),
                          on_error = "skip")
  empty <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) {
      return(list(message = list(items = list())))
    }
    list(results = list(), meta = list())
  }
  previous <- run_search_plan(
    plan, requester = empty,
    clock = function() as.POSIXct("2026-10-01", tz = "UTC")
  )
  partial <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) stop("unavailable")
    list(results = list(), meta = list())
  }
  destination <- tempfile("refresh-partial-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  current <- refresh_search_snapshot(
    previous, destination, requester = partial,
    clock = function() as.POSIXct("2026-10-02", tz = "UTC")
  )
  expect_false(current$snapshot$context$complete)
  expect_false(current$history$transitions$comparable)
  expect_identical(current$snapshot$result$sources$status,
                   c("failed", "ok"))
})

test_that("invalid destination and options fail before HTTP", {
  plan <- new_search_plan("topic", sources = "crossref")
  empty <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  previous <- run_search_plan(plan, requester = empty)
  never <- function(url, headers, params, timeout) {
    fail("network should not be called")
  }
  existing <- tempfile("refresh-existing-")
  dir.create(existing)
  on.exit(unlink(existing, recursive = TRUE), add = TRUE)
  expect_error(refresh_search_snapshot(previous, existing,
                                       requester = never), "already exists")
  expect_error(refresh_search_snapshot(previous, tempfile(),
                                       include_values = NA,
                                       requester = never), "include_values")
})
