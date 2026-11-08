test_that("DOI tracing finds observations without loading unrelated runs", {
  plan <- new_search_plan("topic", sources = "crossref")
  make <- function(dois, at) {
    mock <- function(url, headers, params, timeout) {
      list(message = list(items = lapply(dois, function(doi) {
        list(title = list("Study"), DOI = doi)
      })))
    }
    run_search_plan(plan, requester = mock,
      clock = function() as.POSIXct(at, tz = "UTC"))
  }
  archive <- tempfile("doi-archive-")
  on.exit(unlink(archive, recursive = TRUE), add = TRUE)
  create_search_archive(make(c("10.1000/a"), "2026-10-01"), archive)
  append_search_archive(make(c("10.1000/a", "10.1000/b"),
                             "2026-10-02"), archive)
  trace <- trace_archive_dois(
    archive, c("DOI:10.1000/A", "10.1000/b", "10.1000/missing"))
  expect_identical(trace$summary$observed_runs, c(2L, 1L, 0L))
  expect_identical(trace$summary$first_run[[1L]], "run_000001")
  expect_equal(length(trace$legacy_runs), 0L)
  expect_true(trace$integrity_checked)
  expect_identical(nrow(trace$occurrences), 3L)
})

test_that("older manifests fall back to trusted RDS and indexes are checked", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list(
      list(title = list("Study"), DOI = "10.1000/a")
    )))
  }
  archive <- tempfile("doi-archive-")
  on.exit(unlink(archive, recursive = TRUE), add = TRUE)
  snap <- run_search_plan(plan, requester = mock)
  create_search_archive(snap, archive)
  path <- file.path(archive, "runs", "run_000001")
  manifest_path <- file.path(path, "manifest.json")
  data <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  data$doi_index <- NULL
  jsonlite::write_json(data, manifest_path, auto_unbox = TRUE, pretty = TRUE)
  record <- file.path(path, "integrity.json")
  integ <- jsonlite::fromJSON(record, simplifyVector = FALSE)
  # Close the binary stream deterministically to avoid warnings during
  # repeated testthat runs on CI.
  connection <- file(manifest_path, open = "rb")
  digest <- tryCatch(
    openssl::sha256(connection),
    finally = close(connection)
  )
  integ$manifest_sha256 <- paste(sprintf("%02x", as.integer(digest)), collapse = "")
  jsonlite::write_json(integ, record, auto_unbox = TRUE, pretty = TRUE)
  expect_error(trace_archive_dois(archive, "10.1000/a",
                                  on_unindexed = "error"), "no DOI index")
  traced <- trace_archive_dois(archive, "10.1000/a")
  expect_identical(traced$legacy_runs, "run_000001")
  expect_identical(traced$summary$observed_runs, 1L)
  expect_error(trace_archive_dois(archive, "not-a-doi"), "valid DOI")
})
