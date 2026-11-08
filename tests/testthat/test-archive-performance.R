test_that("recent refresh returns bounded evidence and keeps full behavior", {
  plan <- new_search_plan("topic", sources = "crossref")
  make <- function(label, at) {
    mock <- function(url, headers, params, timeout) {
      list(message = list(items = list(list(
        title = list(label), DOI = "10.1000/a"
      ))))
    }
    run_search_plan(plan, requester = mock,
                    clock = function() as.POSIXct(at, tz = "UTC"))
  }
  path <- tempfile("archive-fast-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(make("A", "2026-10-01"), path)
  append_search_archive(make("B", "2026-10-02"), path)
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list(list(
      title = list("C"), DOI = "10.1000/a"
    ))))
  }
  fast <- refresh_search_archive(
    path, requester = mock, return_mode = "recent",
    clock = function() as.POSIXct("2026-10-03", tz = "UTC")
  )
  expect_s3_class(fast, "SearchArchiveRefresh")
  expect_identical(fast$previous$result$articles[[1L]]$title, "B")
  expect_identical(fast$snapshot$result$articles[[1L]]$title, "C")
  expect_equal(fast$comparison$summary$modified_articles, 1L)
  expect_equal(nrow(fast$inventory), 3L)
  full <- refresh_search_archive(
    path, requester = mock,
    clock = function() as.POSIXct("2026-10-04", tz = "UTC")
  )
  expect_s3_class(full, "SearchArchive")
  expect_equal(length(full$snapshots), 4L)
  expect_equal(nrow(full$history$transitions), 3L)
  expect_error(refresh_search_archive(
    path, return_mode = "wrong", requester = mock
  ), "arg")
})

test_that("append does not need to deserialize unrelated earlier runs", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  snap <- run_search_plan(plan, requester = mock)
  path <- tempfile("archive-fast-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(snap, path)
  append_search_archive(snap, path)
  # Make an old RDS unreadable yet checksum-consistent to demonstrate
  # that append validates historical bytes without deserializing old data.
  folder <- file.path(path, "runs", "run_000001")
  writeBin(charToRaw("invalid-RDS"), file.path(folder, "snapshot.rds"))
  integrity_path <- file.path(folder, "integrity.json")
  integrity <- jsonlite::fromJSON(integrity_path)
  integrity$snapshot_sha256 <- paste(
    sprintf("%02x", as.integer(openssl::sha256(charToRaw("invalid-RDS")))),
    collapse = ""
  )
  jsonlite::write_json(integrity, integrity_path, auto_unbox = TRUE)
  append_search_archive(snap, path)
  expect_equal(nrow(list_search_archive_runs(path)), 3L)
  expect_error(read_search_archive(path), "Could not read search snapshot")
})
