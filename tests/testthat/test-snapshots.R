test_that("plans validate a stable credential-free query contract", {
  plan <- new_search_plan("  causal inference  ", sources = "crossref",
                          page_size = 5, max_results_per_source = 10)
  expect_s3_class(plan, "SearchPlan")
  expect_identical(plan$query, "causal inference")
  expect_false(any(grepl("key|mailto|password", names(plan))))
  expect_error(new_search_plan("", sources = "crossref"), "query")
  expect_error(new_search_plan("test", sources = "unknown"), "sources")
  expect_error(new_search_plan("test", sources = "scopus",
                               start_date = "2024-01-01"), "both")
  expect_error(new_search_plan("test", page_size = 26), "page_size")
  expect_error(new_search_plan("test", timeout = 0), "timeout")
})

test_that("snapshot round trip preserves evidence and never stores secrets", {
  plan <- new_search_plan("forecasting", sources = c("crossref", "openalex"),
                          page_size = 2)
  requests <- 0L
  mock <- function(url, headers, params, timeout) {
    requests <<- requests + 1L
    if (grepl("crossref", url, fixed = TRUE)) {
      return(list(message = list(items = list(
        list(title = list("Forecast"), DOI = "10.1000/A")
      ))))
    }
    list(results = list(list(title = "Forecast",
                             doi = "https://doi.org/10.1000/a")),
         meta = list())
  }
  times <- as.POSIXct(c("2026-10-16 20:00:00", "2026-10-16 20:00:03"),
                      tz = "UTC")
  clock <- local({i <- 0L; function() {i <<- i + 1L; times[[i]]}})
  snapshot <- run_search_plan(
    plan, openalex_api_key = "private-key", crossref_mailto = "me@example.org",
    requester = mock, sleep = function(...) NULL, clock = clock
  )
  expect_s3_class(snapshot, "SearchSnapshot")
  expect_identical(snapshot$started_at, "2026-10-16T20:00:00Z")
  expect_identical(snapshot$finished_at, "2026-10-16T20:00:03Z")
  expect_identical(snapshot$context$openalex_key_provided, TRUE)
  expect_identical(snapshot$context$complete, TRUE)
  expect_length(snapshot$result$articles, 1L)
  expect_equal(nrow(snapshot$result$provenance), 2L)
  expect_equal(requests, 2L)
  expect_false(grepl("private-key|me@example.org",
                     paste(capture.output(str(snapshot)), collapse = "\n")))

  filename <- tempfile(fileext = ".rds")
  on.exit(unlink(filename), add = TRUE)
  expect_identical(save_search_snapshot(snapshot, filename), filename)
  expect_error(save_search_snapshot(snapshot, filename), "exists")
  restored <- read_search_snapshot(filename)
  expect_identical(restored, snapshot)
  expect_false(grepl("private-key|me@example.org",
                     paste(capture.output(str(restored)), collapse = "\n")))
})

test_that("failed provider snapshots retain partial status", {
  plan <- new_search_plan("topic", sources = c("crossref", "openalex"),
                          on_error = "skip")
  mock <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) stop("failure")
    list(results = list(), meta = list())
  }
  snapshot <- run_search_plan(plan, requester = mock)
  expect_false(snapshot$context$complete)
  expect_identical(snapshot$result$sources$status, c("failed", "ok"))
  expect_equal(nrow(snapshot$result$errors), 1L)
})

test_that("empty searches round-trip and corrupt snapshots fail", {
  plan <- new_search_plan("test", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  snapshot <- run_search_plan(plan, requester = mock)
  expect_length(snapshot$result$articles, 0L)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  save_search_snapshot(snapshot, path)
  expect_identical(read_search_snapshot(path), snapshot)
  invalid <- snapshot
  invalid$schema_version <- 999L
  expect_error(save_search_snapshot(invalid, tempfile()), "schema_version")
  saveRDS(invalid, path)
  expect_error(read_search_snapshot(path), "schema_version")
})
