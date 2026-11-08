test_that("complete research workflow preserves reproducible DOI evidence", {
  plan <- new_search_plan(
    "causal inference", sources = c("crossref", "openalex"),
    max_results_per_source = 3L, page_size = 3L,
    on_error = "stop", on_conflict = "keep_first"
  )
  first <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) {
      return(list(message = list(items = list(
        list(title = list("Shared paper"), DOI = "10.1000/shared",
             published = list("date-parts" = list(list(2024)))),
        list(title = list("Disappears"), DOI = "10.1000/old")
      ))))
    }
    list(results = list(
      list(title = "Shared paper",
           doi = "https://doi.org/10.1000/SHARED",
           abstract_inverted_index = list(Recovered = list(0L),
                                          evidence = list(1L))),
      list(title = "Unkeyed article")
    ), meta = list())
  }
  start <- run_search_plan(
    plan, requester = first, sleep = function(...) NULL,
    clock = function() as.POSIXct("2026-10-01", tz = "UTC")
  )
  expect_length(start$result$articles, 3L)
  expect_equal(nrow(start$result$provenance), 4L)
  expect_identical(start$result$articles[[1L]]$abstract,
                   "Recovered evidence")
  expect_true(start$context$complete)
  quality <- assess_article_quality(start$result$articles)
  expect_equal(nrow(quality$records), 3L)
  expect_equal(nrow(quality$summary), 5L)

  root <- tempfile("complete-research-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  saved <- file.path(root, "baseline.rds")
  save_search_snapshot(start, saved)
  expect_identical(read_search_snapshot(saved), start)

  archive <- file.path(root, "archive")
  create_search_archive(start, archive)
  next_response <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) {
      return(list(message = list(items = list(
        list(title = list("Shared paper revised"), DOI = "10.1000/shared"),
        list(title = list("New article"), DOI = "10.1000/new")
      ))))
    }
    list(results = list(
      list(title = "Shared paper revised",
           doi = "https://doi.org/10.1000/shared",
           abstract_inverted_index = list(Updated = list(0L))),
      list(title = "Unkeyed article")
    ), meta = list())
  }
  updated <- refresh_search_archive(
    archive, requester = next_response, sleep = function(...) NULL,
    clock = function() as.POSIXct("2026-10-02", tz = "UTC"),
    return_mode = "recent"
  )
  expect_s3_class(updated, "SearchArchiveRefresh")
  expect_equal(updated$comparison$summary$added, 1L)
  expect_equal(updated$comparison$summary$removed, 1L)
  expect_true(updated$comparison$summary$modified_articles >= 1L)
  expect_equal(updated$comparison$summary$unmatched_before, 1L)
  expect_equal(updated$comparison$summary$unmatched_after, 1L)
  expect_equal(nrow(updated$inventory), 2L)
  expect_identical(verify_search_archive(archive)$runs$integrity,
                   rep("verified_sha256", 2L))

  trace <- trace_archive_dois(archive, "10.1000/shared")
  expect_identical(trace$summary$observed_runs, 2L)
  timeline <- article_metadata_timeline(archive, "10.1000/shared")
  expect_true("title" %in% timeline$changes$field)
  expect_true("abstract_sha256" %in% timeline$changes$field)

  history <- compare_recent_archive_runs(archive, last_n = 2L)
  report_path <- file.path(root, "report")
  write_search_history_report(history, report_path)
  expect_true(all(file.exists(file.path(
    report_path, c("runs.csv", "providers.csv", "transitions.csv",
                   "events.csv", "unmatched.csv", "README.md")
  ))))
  expect_equal(nrow(utils::read.csv(
    file.path(report_path, "transitions.csv")
  )), 1L)
  citations <- file.path(root, "articles.bib")
  export_articles(updated$snapshot$result$articles, citations,
                  format = "bibtex")
  expect_true(file.exists(citations))
  expect_gt(file.info(citations)$size, 0)
})

test_that("incomplete provider coverage is not treated as comparable", {
  plan <- new_search_plan(
    "causal inference", sources = c("crossref", "openalex"),
    max_results_per_source = 2L, page_size = 2L, on_error = "skip"
  )
  baseline <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) {
      return(list(message = list(items = list(
        list(title = list("Evidence"), DOI = "10.1000/evidence")
      ))))
    }
    list(results = list(), meta = list())
  }
  first <- run_search_plan(
    plan, requester = baseline,
    clock = function() as.POSIXct("2026-10-01", tz = "UTC")
  )
  root <- tempfile("partial-workflow-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  archive <- file.path(root, "archive")
  create_search_archive(first, archive)
  failure <- function(url, headers, params, timeout) {
    if (grepl("crossref", url, fixed = TRUE)) stop("provider outage")
    list(results = list(), meta = list())
  }
  refresh <- refresh_search_archive(
    archive, requester = failure, return_mode = "recent",
    clock = function() as.POSIXct("2026-10-02", tz = "UTC")
  )
  expect_false(refresh$snapshot$context$complete)
  expect_false(refresh$comparison$summary$complete_sources)
  expect_false(refresh$comparison$comparability$complete_sources)
  expect_identical(refresh$snapshot$result$sources$status,
                   c("failed", "ok"))
  expect_identical(refresh$snapshot$result$errors$source, "crossref")
})
