test_that("quality assessment separates presence and validity", {
  papers <- list(
    Article(title = "A", abstract = "Text", publication_date = "2024-02-29",
            authors = c("Ada", "Bob"), doi = "doi:10.1000/A"),
    Article(title = " ", publication_date = "2024-02-30",
            authors = c("Ada", " "), doi = "invalid"),
    Article(title = "B", doi = "https://doi.org/10.1000/a")
  )
  result <- assess_article_quality(papers)
  expect_equal(result$records$completeness, c(1, 3/5, 2/5))
  expect_equal(result$records$validity, c(1, 0, 2/5))
  expect_identical(result$records$duplicate_doi, c(FALSE, FALSE, TRUE))
  expect_equal(result$summary$present[result$summary$field == "doi"], 3L)
  expect_equal(result$summary$invalid[result$summary$field == "doi"], 1L)
  expect_true(any(result$issues$issue == "invalid"))
  expect_true(any(result$issues$issue == "duplicate"))
})

test_that("quality metrics handle empty inputs", {
  result <- assess_article_quality(list())
  expect_equal(nrow(result$records), 0L)
  expect_equal(nrow(result$summary), 5L)
  expect_true(all(is.na(result$summary$presence_rate)))
  expect_equal(nrow(result$issues), 0L)
})

test_that("weights are normalized and validated", {
  weights <- c(title = 2, abstract = 0, publication_date = 0,
               authors = 0, doi = 1)
  result <- assess_article_quality(list(Article(title = "A")), weights)
  expect_equal(result$records$completeness, 2/3)
  expect_equal(result$weights[["title"]], 2/3)
  expect_error(assess_article_quality(list(), weights = rep(1, 5)), "weights")
  expect_error(assess_article_quality(list(), weights = setNames(rep(0, 5),
              c("title", "abstract", "publication_date", "authors", "doi"))),
              "weights")
  expect_error(assess_article_quality(list(list())), "Article")
})
