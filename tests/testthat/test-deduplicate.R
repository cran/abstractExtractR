test_that("DOI normalization recognizes resolver variants and rejects malformed IDs", {
  x <- c(" DOI:10.1000/ABC ", "https://doi.org/10.1000/abc",
         "https://dx.doi.org/10.1000%2FABC?source=one",
         "urn:doi:10.1000/ABC", "other", "", NA_character_)
  expect_identical(
    normalize_doi(x),
    c(rep("10.1000/abc", 4), rep(NA_character_, 3))
  )
  expect_error(normalize_doi(3), "character")
  expect_identical(normalize_doi(character()), character())
})

test_that("duplicate DOI records fill gaps without changing input", {
  a <- Article(title = "A study", doi = "10.1000/EXAMPLE",
               publication_date = "2024")
  b <- Article(doi = "https://doi.org/10.1000/example",
               abstract = "Recovered", publication_date = "2024-06-03",
               authors = c("Ada", "Grace"))
  input <- list(scopus = a, crossref = b)
  result <- deduplicate_articles(input, report = TRUE)
  expect_length(result$articles, 1L)
  expect_identical(names(result$articles), "scopus")
  expect_identical(result$articles[[1]]$title, "A study")
  expect_identical(result$articles[[1]]$abstract, "Recovered")
  expect_identical(result$articles[[1]]$authors, c("Ada", "Grace"))
  expect_identical(result$articles[[1]]$publication_date, "2024-06-03")
  expect_identical(result$articles[[1]]$doi, "10.1000/example")
  expect_identical(input$scopus$doi, "10.1000/EXAMPLE")
  expect_identical(result$mapping$output_index, c(1L, 1L))
  expect_identical(result$mapping$status, c("unique", "duplicate"))
  expect_equal(nrow(result$conflicts), 0L)
})

test_that("DOI-free records remain independent even with same title", {
  x <- list(Article(title = "Same"), Article(title = "Same"),
            Article(title = "Same", doi = "not a DOI"))
  result <- deduplicate_articles(x, report = TRUE)
  expect_length(result$articles, 3L)
  expect_identical(result$mapping$status, rep("unkeyed", 3L))
})

test_that("conflicts retain first value and are auditable", {
  x <- list(Article(title = "First", authors = "Ada",
                    publication_date = "2024-03-02", doi = "10.1000/A"),
            Article(title = "Second", authors = "Bob",
                    publication_date = "2025-03-02", doi = "10.1000/a"))
  out <- deduplicate_articles(x, report = TRUE)
  expect_identical(out$articles[[1L]]$title, "First")
  expect_setequal(out$conflicts$field,
                  c("title", "authors", "publication_date"))
  expect_warning(deduplicate_articles(x, on_conflict = "warn"), "conflicting")
  expect_error(deduplicate_articles(x, on_conflict = "error"), "Conflicting")
})

test_that("empty input and argument checks are deterministic", {
  empty <- deduplicate_articles(list(), report = TRUE)
  expect_identical(empty$articles, list())
  expect_equal(nrow(empty$mapping), 0L)
  expect_equal(nrow(empty$conflicts), 0L)
  expect_error(deduplicate_articles(list(list())), "Article")
  expect_error(deduplicate_articles(list(), report = NA), "report")
})
