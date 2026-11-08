test_that("selective comparisons match existing DOI comparison", {
  plan <- new_search_plan("topic", sources = "crossref")
  make <- function(title, time) {
    mock <- function(url, headers, params, timeout) {
      list(message = list(items = list(
        list(title = list(title), DOI = "10.1000/a")
      )))
    }
    run_search_plan(plan, requester = mock,
      clock = function() as.POSIXct(time, tz = "UTC"))
  }
  first <- make("Old", "2026-10-01")
  second <- make("New", "2026-10-02")
  path <- tempfile("archive-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(first, path)
  append_search_archive(second, path)
  expected <- compare_search_snapshots(first, second)
  expect_identical(compare_search_archive_runs(path, 1L, "latest"), expected)
  expect_equal(compare_recent_archive_runs(path, 2L)$transitions$modified_articles, 1L)
  expect_error(compare_search_archive_runs(path, 2L, 1L), "earlier")
  expect_error(compare_recent_archive_runs(path, 3L), "last_n")
})

test_that("selected comparisons do not deserialize unrelated corrupt RDS", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  snap <- run_search_plan(plan, requester = mock)
  path <- tempfile("archive-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(snap, path)
  append_search_archive(snap, path)
  append_search_archive(snap, path)
  cat("corruption", file = file.path(path, "runs", "run_000001",
                                  "snapshot.rds"), append = TRUE)
  expect_equal(nrow(compare_recent_archive_runs(path, 2L)$transitions), 1L)
  expect_error(read_search_archive(path), "checksum mismatch")
})
