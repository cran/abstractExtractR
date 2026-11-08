test_that("manifest is credential-free and exports JSON", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list(list(title = list("Paper"),
                                         DOI = "10.1000/a"))))
  }
  snap <- run_search_plan(plan, requester = mock,
                          crossref_mailto = "private@example.org")
  manifest <- search_snapshot_manifest(snap)
  expect_equal(manifest$counts$normalized_articles, 1L)
  file <- tempfile(fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  write_search_manifest(snap, file)
  text <- paste(readLines(file), collapse = "\n")
  expect_false(grepl("private@example.org", text, fixed = TRUE))
  decoded <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  expect_identical(decoded$plan$sources, list("crossref"))
  expect_identical(decoded$counts$normalized_articles, 1L)
  expect_error(write_search_manifest(snap, file), "exists")
})

test_that("comparison classifies DOI evidence and metadata differences", {
  plan <- new_search_plan("topic", sources = "crossref")
  make <- function(records) {
    mock <- function(url, headers, params, timeout) {
      list(message = list(items = records))
    }
    run_search_plan(plan, requester = mock)
  }
  before <- make(list(list(title = list("Old"), DOI = "10.1000/a"),
                      list(title = list("Removed"), DOI = "10.1000/b"),
                      list(title = list("No DOI"))))
  after <- make(list(list(title = list("Changed"), DOI = "10.1000/a"),
                     list(title = list("Added"), DOI = "10.1000/c")))
  diff <- compare_search_snapshots(before, after)
  expect_equal(diff$summary$added, 1L)
  expect_equal(diff$summary$removed, 1L)
  expect_equal(diff$summary$modified_articles, 1L)
  expect_equal(diff$summary$unmatched_before, 1L)
  expect_identical(diff$added$doi, "10.1000/c")
  expect_identical(diff$removed$doi, "10.1000/b")
  expect_identical(diff$modified$field, "title")
  expect_true(diff$comparability$same_plan)
})

test_that("multi-source manifests keep an ordered JSON array", {
  plan <- new_search_plan("topic", sources = c("openalex", "crossref"))
  mock <- function(url, headers, params, timeout) {
    if (grepl("openalex", url, fixed = TRUE)) {
      return(list(results = list(), meta = list()))
    }
    list(message = list(items = list()))
  }
  snap <- run_search_plan(plan, requester = mock, sleep = function(...) NULL)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_search_manifest(snap, path)
  json <- paste(readLines(path, warn = FALSE), collapse = "\n")
  decoded <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_identical(decoded$plan$sources, list("openalex", "crossref"))
  expect_identical(decoded$counts$normalized_articles, 0L)
})
