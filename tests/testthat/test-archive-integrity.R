test_that("new archives include checksums and detect corruption", {
  plan <- new_search_plan("test", sources = "crossref")
  mock <- function(url, headers, params, timeout) list(message = list(items = list()))
  snapshot <- run_search_plan(plan, requester = mock)
  path <- tempfile("archive-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(snapshot, path)
  expect_identical(verify_search_archive(path)$runs$integrity, "verified_sha256")
  manifest <- file.path(path, "runs", "run_000001", "manifest.json")
  cat(" ", file = manifest, append = TRUE)
  expect_identical(verify_search_archive(path)$runs$integrity, "invalid")
  expect_error(read_search_archive(path), "checksum mismatch")
})

test_that("legacy archives remain readable with unverified status", {
  plan <- new_search_plan("test", sources = "crossref")
  mock <- function(url, headers, params, timeout) list(message = list(items = list()))
  path <- tempfile("archive-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(run_search_plan(plan, requester = mock), path)
  unlink(file.path(path, "runs", "run_000001", "integrity.json"))
  unlink(file.path(path, "archive-layout.json"))
  expect_identical(verify_search_archive(path)$runs$integrity, "legacy_unverified")
  expect_equal(nrow(read_search_archive(path)$inventory), 1L)
})

test_that("writer lock and interrupted stages require deliberate recovery", {
  plan <- new_search_plan("test", sources = "crossref")
  mock <- function(url, headers, params, timeout) list(message = list(items = list()))
  snap <- run_search_plan(plan, requester = mock)
  path <- tempfile("archive-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(snap, path)
  lock <- file.path(path, ".abstractExtractR.lock")
  dir.create(lock)
  expect_error(append_search_archive(snap, path), "locked")
  expect_true(recover_search_archive(path)$locked)
  expect_error(recover_search_archive(path, dry_run = FALSE), "locked")
  recover_search_archive(path, dry_run = FALSE, force_unlock = TRUE)
  expect_false(dir.exists(lock))
  stage <- file.path(path, "runs", ".abstractExtractR-run-interrupted")
  dir.create(stage)
  expect_length(verify_search_archive(path)$pending, 1L)
  expect_error(append_search_archive(snap, path), "interrupted staging")
  report <- recover_search_archive(path, dry_run = FALSE)
  expect_length(report$removed, 1L)
  expect_false(dir.exists(stage))
  append_search_archive(snap, path)
  expect_equal(nrow(read_search_archive(path)$inventory), 2L)
})

test_that("protected runs reject removed checksums", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  path <- tempfile("archive-sha256-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(run_search_plan(plan, requester = mock), path)
  unlink(file.path(path, "runs", "run_000001", "integrity.json"))
  expect_identical(verify_search_archive(path)$runs$integrity, "invalid")
  expect_error(read_search_archive(path), "integrity record is missing")
})

test_that("legacy MD5 archives are readable and next run uses SHA-256", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  snap <- run_search_plan(plan, requester = mock)
  path <- tempfile("archive-md5-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(snap, path)
  folder <- file.path(path, "runs", "run_000001")
  md5 <- list(schema_version = 1L, algorithm = "md5",
              snapshot_md5 = unname(tools::md5sum(file.path(folder, "snapshot.rds"))),
              manifest_md5 = unname(tools::md5sum(file.path(folder, "manifest.json"))))
  jsonlite::write_json(md5, file.path(folder, "integrity.json"),
                       auto_unbox = TRUE)
  unlink(file.path(path, "archive-layout.json"))
  expect_identical(verify_search_archive(path)$runs$integrity, "verified_md5")
  append_search_archive(snap, path)
  expect_identical(verify_search_archive(path)$runs$integrity,
                   c("verified_md5", "verified_sha256"))
  expect_equal(nrow(read_search_archive(path)$inventory), 2L)
})

test_that("interrupted writes preserve evidence and require inspection", {
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  snap <- run_search_plan(plan, requester = mock)
  path <- tempfile("archive-failure-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_search_archive(snap, path)
  stage <- file.path(path, "runs", ".abstractExtractR-run-crashed")
  dir.create(stage)
  writeLines("incomplete", file.path(stage, "snapshot.rds"))
  expect_equal(nrow(read_search_archive(path)$inventory), 1L)
  expect_error(append_search_archive(snap, path), "interrupted staging")
  expect_length(recover_search_archive(path)$pending, 1L)
  recovered <- recover_search_archive(path, dry_run = FALSE)
  expect_length(recovered$removed, 1L)
  expect_equal(nrow(read_search_archive(path)$inventory), 1L)
})
