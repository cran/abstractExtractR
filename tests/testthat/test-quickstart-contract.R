test_that("recommended entry points remain exported", {
  entry_points <- c(
    "new_search_plan", "run_search_plan", "assess_article_quality",
    "save_search_snapshot", "read_search_snapshot",
    "create_search_archive", "refresh_search_archive",
    "compare_recent_archive_runs", "write_search_history_report",
    "export_articles", "search_articles"
  )
  exports <- getNamespaceExports("abstractExtractR")
  expect_true(all(entry_points %in% exports))
})

test_that("documented small offline workflow preserves core return shapes", {
  mock <- function(url, headers, params, timeout) {
    if (grepl("crossref.org", url, fixed = TRUE)) {
      return(list(message = list(items = list(
        list(title = list("Example study"), DOI = "10.1000/demo")
      ))))
    }
    if (grepl("openalex.org", url, fixed = TRUE)) {
      return(list(results = list(list(
        title = "Example study",
        doi = "https://doi.org/10.1000/DEMO",
        abstract_inverted_index = list(
          Recovered = list(0L), abstract = list(1L)
        )
      )), meta = list()))
    }
    stop("Unexpected provider")
  }

  plan <- new_search_plan(
    "example", sources = c("crossref", "openalex"),
    max_results_per_source = 2L, page_size = 2L,
    min_interval = 0
  )
  snapshot <- run_search_plan(
    plan, requester = mock,
    clock = function() as.POSIXct("2026-10-18", tz = "UTC")
  )
  expect_s3_class(snapshot, "SearchSnapshot")
  expect_length(snapshot$result$articles, 1L)
  expect_identical(snapshot$result$articles[[1L]]$doi, "10.1000/demo")
  expect_identical(snapshot$result$articles[[1L]]$abstract,
                   "Recovered abstract")
  expect_equal(nrow(snapshot$result$provenance), 2L)
  expect_true(snapshot$context$complete)

  audit <- assess_article_quality(snapshot$result$articles)
  expect_equal(nrow(audit$records), 1L)
  expect_equal(nrow(audit$summary), 5L)

  root <- tempfile("getting-started-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  saved <- file.path(root, "baseline.rds")
  save_search_snapshot(snapshot, saved)
  expect_identical(read_search_snapshot(saved), snapshot)

  archive <- file.path(root, "archive")
  create_search_archive(snapshot, archive)
  updated <- refresh_search_archive(
    archive, requester = mock, return_mode = "recent",
    clock = function() as.POSIXct("2026-10-19", tz = "UTC")
  )
  expect_s3_class(updated, "SearchArchiveRefresh")
  expect_equal(updated$comparison$summary$modified_articles, 0L)
  expect_equal(nrow(updated$inventory), 2L)

  history <- compare_recent_archive_runs(archive, last_n = 2L)
  report <- file.path(root, "report")
  write_search_history_report(history, report)
  expect_true(file.exists(file.path(report, "transitions.csv")))
  citations <- file.path(root, "papers.bib")
  export_articles(updated$snapshot$result$articles, citations,
                  format = "bibtex")
  expect_true(file.exists(citations))
})
