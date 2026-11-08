test_that("longitudinal DOI observations report changes and gaps", {
  plan <- new_search_plan("topic", sources = "crossref")
  make <- function(works, at) {
    mock <- function(url, headers, params, timeout) {
      list(message = list(items = works))
    }
    run_search_plan(plan, requester = mock,
      clock = function() as.POSIXct(at, tz = "UTC"))
  }
  path <- tempfile("doi-timeline-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(make(list(list(
    title = list("First"), DOI = "10.1000/a", abstract = "Original"
  )), "2026-10-01"), path)
  append_search_archive(make(list(), "2026-10-02"), path)
  append_search_archive(make(list(list(
    title = list("Revised"), DOI = "10.1000/a", abstract = "Updated"
  )), "2026-10-03"), path)
  result <- article_metadata_timeline(path, "doi:10.1000/A")
  expect_identical(result$doi, "10.1000/a")
  expect_equal(result$summary$observed_runs, 2L)
  expect_equal(result$summary$unobserved_runs, 1L)
  expect_identical(result$gaps$run, "run_000002")
  expect_setequal(result$changes$field, c("title", "abstract_sha256"))
  expect_false("abstract" %in% names(result$observations))
  expect_false(identical(result$observations$abstract_sha256[[1]],
                         result$observations$abstract_sha256[[2]]))
  full <- article_metadata_timeline(path, "10.1000/a",
                                    include_abstract = TRUE)
  expect_identical(full$observations$abstract, c("Original", "Updated"))
})

test_that("missing DOI yields an empty timeline without inventing evidence", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  path <- tempfile("doi-timeline-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(run_search_plan(plan, requester = mock), path)
  result <- article_metadata_timeline(path, "10.1000/absent")
  expect_equal(nrow(result$observations), 0L)
  expect_equal(nrow(result$changes), 0L)
  expect_equal(nrow(result$gaps), 1L)
  expect_true(is.na(result$summary$first_observed_at))
  expect_error(article_metadata_timeline(path, "invalid"), "valid DOI")
  expect_error(article_metadata_timeline(path, "10.1000/absent",
                                         include_abstract = NA), "include_abstract")
})
