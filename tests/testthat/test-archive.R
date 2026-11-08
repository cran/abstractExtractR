test_that("archive preserves runs and chronological history", {
  plan <- new_search_plan("topic", sources = "crossref")
  make <- function(title, when) {
    mock <- function(url, headers, params, timeout) {
      list(message = list(items = list(
        list(title = list(title), DOI = "10.1000/a")
      )))
    }
    run_search_plan(plan, requester = mock,
                    clock = function() as.POSIXct(when, tz = "UTC"))
  }
  before <- make("Old", "2026-10-01")
  after <- make("New", "2026-10-02")
  path <- tempfile("archive-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(before, path)
  append_search_archive(after, path)
  archive <- read_search_archive(path)
  expect_identical(archive$inventory$run, c("run_000001", "run_000002"))
  expect_equal(archive$history$transitions$modified_articles, 1L)
  expect_identical(archive$snapshots[[1L]], before)
  expect_identical(archive$snapshots[[2L]], after)
  expect_error(create_search_archive(before, path), "already exists")
  expect_error(append_search_archive(make("Old", "2026-09-30"), path),
               "before the last")
})

test_that("archive refresh appends an observation on demand", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  path <- tempfile("archive-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  initial <- run_search_plan(
    plan, requester = mock,
    clock = function() as.POSIXct("2026-10-01", tz = "UTC")
  )
  create_search_archive(initial, path)
  refreshed <- refresh_search_archive(
    path, requester = mock,
    clock = function() as.POSIXct("2026-10-02", tz = "UTC")
  )
  expect_equal(nrow(refreshed$inventory), 2L)
  failure <- function(url, headers, params, timeout) stop("offline")
  expect_error(refresh_search_archive(path, requester = failure),
               "search failed")
  expect_equal(nrow(read_search_archive(path)$inventory), 2L)
})

test_that("archive validates consecutive folders and query plans", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  snap <- run_search_plan(plan, requester = mock)
  path <- tempfile("archive-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(snap, path)
  dir.create(file.path(path, "runs", "run_000003"))
  expect_error(read_search_archive(path), "consecutive")
  unlink(file.path(path, "runs", "run_000003"), recursive = TRUE)
  other <- new_search_plan("different", sources = "crossref")
  expect_error(append_search_archive(run_search_plan(other, requester = mock),
                                     path), "different search plan")
})
