test_that("tabular conversion handles empty and incomplete records", {
  empty <- articles_to_data_frame(list())
  expect_s3_class(empty, "data.frame")
  expect_identical(names(empty),
                   c("title", "abstract", "publication_date", "authors", "doi"))
  expect_equal(nrow(empty), 0L)
  expect_true(all(vapply(empty, is.character, logical(1L))))
  articles <- list(
    Article(title = "Paper", authors = c("Alice", "Bob"), doi = "10.1000/x"),
    Article(abstract = "<jats:p>Content</jats:p>")
  )
  result <- articles_to_data_frame(articles)
  expect_equal(result$authors[[1L]], "Alice; Bob")
  expect_true(is.na(result$authors[[2L]]))
  expect_true(is.na(result$title[[2L]]))
  expect_equal(result$abstract[[2L]], "<jats:p>Content</jats:p>")
  expect_equal(articles_to_data_frame(articles, author_separator = " | ")$authors[[1]],
               "Alice | Bob")
})

test_that("BibTeX citation keys are stable and unique", {
  x <- list(
    Article(title = "A & B", doi = "10.1000/a", authors = c("Ada", "Bob")),
    Article(title = "Second", doi = "10.1000/a"),
    Article(title = "Third")
  )
  result <- articles_to_bibtex(x)
  expect_match(result, "@article\\{doi_10_1000_a,", perl = TRUE)
  expect_match(result, "@article\\{doi_10_1000_a_2,", perl = TRUE)
  expect_match(result, "@article\\{ref_3,", perl = TRUE)
  expect_true(grepl("A \\& B", result, fixed = TRUE))
  expect_match(result, "Ada and Bob", fixed = TRUE)
  expect_error(articles_to_bibtex(x, keys = c("x", "x", "y")), "unique")
  expect_error(articles_to_bibtex(x, keys = "one"), "keys")
  expect_equal(articles_to_bibtex(list()), "")
})

test_that("RIS exports repeat authors, preserve unicode, and use CRLF", {
  x <- list(Article(title = "Métodos", authors = c("Álvaro", "Betty"),
                    publication_date = "2026-10-10", doi = "10.1000/p"))
  result <- articles_to_ris(x)
  expect_true(grepl("TY  - JOUR\r\n", result, fixed = TRUE))
  expect_true(grepl("AU  - Álvaro\r\nAU  - Betty\r\n", result, fixed = TRUE))
  expect_true(grepl("DA  - 2026/10/10", result, fixed = TRUE))
  expect_true(endsWith(result, "ER  - \r\n"))
  expect_equal(articles_to_ris(list()), "")
})

test_that("untrusted text cannot inject RIS lines", {
  x <- list(Article(title = "Title\nER  - \nTY  - ELEC",
                    abstract = "Some\nOther"))
  result <- articles_to_ris(x)
  expect_true(grepl("TI  - Title ER  -  TY  - ELEC", result, fixed = TRUE))
  expect_equal(sum(startsWith(strsplit(result, "\r\n", fixed = TRUE)[[1]], "TY  - ")), 1L)
  expect_true(grepl("AB  - Some Other", result, fixed = TRUE))
})

test_that("CSV defaults to formula-safe output", {
  target <- tempfile(fileext = ".csv")
  on.exit(unlink(target), add = TRUE)
  x <- list(Article(title = "=HYPERLINK(1)", authors = "+danger",
                    doi = "10.1000/csv"))
  expect_identical(export_articles(x, target, format = "csv"), invisible(target))
  result <- utils::read.csv(target, check.names = FALSE,
                            stringsAsFactors = FALSE)
  expect_identical(result$title[[1]], "'=HYPERLINK(1)")
  expect_identical(result$authors[[1]], "'+danger")
  export_articles(x, target, format = "csv", sanitize_csv = FALSE)
  raw <- utils::read.csv(target, check.names = FALSE,
                         stringsAsFactors = FALSE)
  expect_equal(raw$title[[1]], "=HYPERLINK(1)")
})

test_that("BibTeX and RIS write paths contain exact renderings", {
  for (format in c("bibtex", "ris")) {
    path <- tempfile()
    x <- list(Article(title = "Case", doi = "10.1000/case"))
    expected <- if (format == "bibtex") articles_to_bibtex(x) else articles_to_ris(x)
    export_articles(x, path, format = format)
    con <- file(path, "rb")
    bytes <- readBin(con, "raw", n = file.info(path)$size)
    close(con)
    expect_identical(rawToChar(bytes), enc2utf8(expected))
    unlink(path)
  }
})

test_that("export input validation is explicit", {
  expect_error(articles_to_data_frame(list(list())), "Article")
  expect_error(articles_to_data_frame(list(), author_separator = NA_character_),
               "author_separator")
  expect_error(export_articles(list(), ""), "file")
  expect_error(export_articles(list(), tempfile(), sanitize_csv = NA),
               "sanitize_csv")
  expect_error(export_articles(list(), tempfile(), format = "json"), "arg")
})
