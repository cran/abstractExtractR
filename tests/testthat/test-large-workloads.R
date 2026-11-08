test_that("larger reconciliation preserves ordering and audit mapping", {
  n <- 1200L
  papers <- lapply(seq_len(n), function(i) {
    Article(title = paste("Paper", i), doi = sprintf("10.1000/p%d", i))
  })
  duplicates <- lapply(seq_len(200L), function(i) {
    Article(abstract = paste("Abstract", i),
            doi = sprintf("https://doi.org/10.1000/p%d", i))
  })
  result <- deduplicate_articles(c(papers, duplicates), report = TRUE)
  expect_length(result$articles, n)
  expect_identical(result$mapping$output_index[(n + 1L):(n + 200L)],
                   seq_len(200L))
  expect_identical(result$articles[[1L]]$abstract, "Abstract 1")
  expect_identical(result$articles[[n]]$title, paste("Paper", n))
  expect_equal(nrow(result$conflicts), 0L)
})

test_that("metadata issue ordering stays stable at higher volume", {
  papers <- lapply(seq_len(750L), function(i) {
    Article(title = if (i %% 2L == 0L) "" else paste("Study", i),
            doi = if (i %% 3L == 0L) "bad" else sprintf("10.1000/p%d", i))
  })
  report <- assess_article_quality(papers)
  expect_equal(nrow(report$records), 750L)
  expect_identical(names(report$issues), c("input_index", "field", "issue"))
  expect_true(all(diff(report$issues$input_index) >= 0L))
  expect_true(any(report$issues$issue == "invalid"))
  expect_equal(nrow(report$summary), 5L)
})

test_that("large DOI manifest retains indexed positions", {
  articles <- lapply(seq_len(300L), function(i) {
    Article(title = paste("Study", i), doi = sprintf("10.1000/d%d", i))
  })
  plan <- new_search_plan("topic", sources = "crossref")
  mock <- function(url, headers, params, timeout) {
    list(message = list(items = list()))
  }
  snapshot <- run_search_plan(plan, requester = mock)
  snapshot$result$articles <- articles
  # A prebuilt synthetic snapshot must have provenance aligned to articles.
  snapshot$result$provenance <- data.frame(
    source = rep("crossref", length(articles)),
    output_index = seq_along(articles),
    stringsAsFactors = FALSE
  )
  manifest <- search_snapshot_manifest(snapshot)
  expect_length(manifest$doi_index, 300L)
  expect_identical(manifest$doi_index[[300L]]$article_index, 300L)
  expect_identical(manifest$doi_index[[300L]]$doi, "10.1000/d300")
})

test_that("preallocation preserves names only on retained records", {
  records <- list(
    lead = Article(title = "First", doi = "10.1000/a"),
    unkeyed = Article(title = "No DOI"),
    duplicate = Article(abstract = "Extra", doi = "doi:10.1000/A"),
    latest = Article(title = "Second", doi = "10.1000/b")
  )
  result <- deduplicate_articles(records, report = TRUE)
  expect_identical(names(result$articles), c("lead", "unkeyed", "latest"))
  expect_identical(result$articles[[1L]]$abstract, "Extra")
  expect_identical(result$mapping$output_index, c(1L, 2L, 1L, 3L))

  partially_named <- records
  names(partially_named) <- c("lead", "", "duplicate", "latest")
  mixed <- deduplicate_articles(partially_named)
  expect_identical(names(mixed), c("lead", "", "latest"))

  unnamed <- unname(records)
  expect_null(names(deduplicate_articles(unnamed)))
  expect_null(names(deduplicate_articles(list())))
})
