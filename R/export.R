# Deterministic export of the package's normalized Article records.

.check_export_articles <- function(articles) {
  if (!is.list(articles) ||
      !all(vapply(articles, inherits, logical(1L), what = "Article"))) {
    stop("articles must be a list of Article objects", call. = FALSE)
  }
  invisible(NULL)
}

.export_field <- function(article, name) {
  value <- article[[name]]
  if (is.null(value)) {
    return(NA_character_)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(sprintf("article %s must be a scalar string or NULL", name),
         call. = FALSE)
  }
  value
}

.export_authors <- function(article) {
  authors <- article[["authors"]]
  if (!is.character(authors) || anyNA(authors)) {
    stop("article authors must be a character vector without NA",
         call. = FALSE)
  }
  authors[nzchar(trimws(authors))]
}

#' Convert article records to a data frame
#'
#' Converts normalized records from Scopus or Crossref into a rectangular
#' representation. Missing scalar fields and missing authors become NA, and
#' authors are joined in source order with a configurable separator.
#' Embedded XML markup in abstracts is preserved.
#'
#' @param articles List of [Article()] records.
#' @param author_separator Scalar character separator for the authors column.
#'
#' @return A base R data.frame containing title, abstract, publication_date,
#'   authors, and doi, in that order. Empty input produces zero rows with
#'   those same character columns.
#' @export
#'
#' @examples
#' x <- list(Article(title = "Paper", authors = c("Ada", "Grace"),
#'                   doi = "10.1000/example"))
#' articles_to_data_frame(x)
articles_to_data_frame <- function(articles, author_separator = "; ") {
  .check_export_articles(articles)
  if (!is.character(author_separator) || length(author_separator) != 1L ||
      is.na(author_separator)) {
    stop("author_separator must be a scalar string", call. = FALSE)
  }

  get_field <- function(name) {
    vapply(articles, .export_field, character(1L), name = name,
           USE.NAMES = FALSE)
  }
  authors <- vapply(articles, function(article) {
    values <- .export_authors(article)
    if (length(values) == 0L) NA_character_ else {
      paste(values, collapse = author_separator)
    }
  }, character(1L), USE.NAMES = FALSE)

  data.frame(
    title = get_field("title"),
    abstract = get_field("abstract"),
    publication_date = get_field("publication_date"),
    authors = authors,
    doi = get_field("doi"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Quote special LaTeX characters without altering the meaning of the text.
# Build replacements one character at a time so inserted macros are not
# accidentally re-escaped. Accented Unicode characters remain UTF-8.
.bibtex_escape <- function(value) {
  value <- gsub("[\r\n\t]+", " ", value)
  chars <- strsplit(value, "", fixed = TRUE)[[1L]]
  replacements <- c(
    "\\" = "\\textbackslash{}",
    "{" = "\\{",
    "}" = "\\}",
    "%" = "\\%",
    "&" = "\\&",
    "$" = "\\$",
    "#" = "\\#",
    "_" = "\\_",
    "~" = "\\textasciitilde{}",
    "^" = "\\textasciicircum{}"
  )
  escaped <- vapply(chars, function(ch) {
    replacement <- replacements[ch]
    if (is.na(replacement)) ch else unname(replacement)
  }, character(1L), USE.NAMES = FALSE)
  paste0(escaped, collapse = "")
}

# Generate stable BibTeX keys using DOI when possible and the row otherwise.
# Duplicates are resolved deterministically without altering article fields.
.bibtex_keys <- function(articles, keys) {
  n <- length(articles)
  if (!is.null(keys)) {
    if (!is.character(keys) || length(keys) != n || anyNA(keys) ||
        any(!grepl("^[A-Za-z][A-Za-z0-9_:-]*$", keys)) ||
        anyDuplicated(keys)) {
      stop("keys must contain one unique, BibTeX-safe identifier per article",
           call. = FALSE)
    }
    return(keys)
  }

  result <- character(n)
  for (i in seq_len(n)) {
    doi <- .export_field(articles[[i]], "doi")
    if (is.na(doi) || !nzchar(trimws(doi))) {
      base <- paste0("ref_", i)
    } else {
      suffix <- gsub("[^a-z0-9]+", "_", tolower(trimws(doi)))
      suffix <- sub("_+$", "", suffix)
      base <- paste0("doi_", suffix)
    }

    candidate <- base
    number <- 2L
    while (candidate %in% result) {
      candidate <- paste0(base, "_", number)
      number <- number + 1L
    }
    result[[i]] <- candidate
  }
  result
}

#' Render article records as BibTeX
#'
#' Returns UTF-8 BibTeX using one article entry per record. Titles, author
#' names, DOIs and abstracts are escaped for LaTeX; missing fields are omitted.
#' Citation keys derive from DOIs where possible and are made unique.
#' Classical BibTeX may need LaTeX accent macros for non-ASCII characters;
#' this function intentionally preserves the original Unicode characters.
#'
#' @param articles List of [Article()] records.
#' @param keys Optional unique vector of BibTeX citation identifiers.
#'
#' @return A length-one character string containing BibTeX text, or an
#'   empty string for zero articles.
#' @export
#'
#' @examples
#' articles_to_bibtex(list(Article(
#'   title = "An example", authors = "Ada Lovelace", doi = "10.1000/example"
#' )))
articles_to_bibtex <- function(articles, keys = NULL) {
  .check_export_articles(articles)
  citation_keys <- .bibtex_keys(articles, keys)

  records <- vapply(seq_along(articles), function(i) {
    article <- articles[[i]]
    fields <- character()

    add_field <- function(name, value) {
      if (!is.na(value) && nzchar(value)) {
        fields <<- c(
          fields, paste0("  ", name, " = {", .bibtex_escape(value), "}")
        )
      }
    }

    add_field("title", .export_field(article, "title"))
    authors <- .export_authors(article)
    if (length(authors) > 0L) {
      add_field("author", paste(authors, collapse = " and "))
    }
    date <- .export_field(article, "publication_date")
    if (!is.na(date) && grepl("^[0-9]{4}", date)) {
      add_field("year", substr(date, 1L, 4L))
    }
    add_field("doi", .export_field(article, "doi"))
    add_field("abstract", .export_field(article, "abstract"))

    if (length(fields) == 0L) {
      return(paste0("@article{", citation_keys[[i]], "\n}"))
    }
    paste0(
      "@article{", citation_keys[[i]], ",\n",
      paste(fields, collapse = ",\n"), "\n}"
    )
  }, character(1L), USE.NAMES = FALSE)

  if (length(records) == 0L) {
    return("")
  }
  paste0(paste(records, collapse = "\n\n"), "\n")
}

# No RIS value may contain a newline, which would otherwise introduce a
# forged RIS tag. Preserve Unicode and all other characters.
.ris_value <- function(value) {
  gsub("[\r\n\t]+", " ", value)
}

#' Render article records as RIS citations
#'
#' Produces RIS records with TY/ER delimiters, one AU tag per author, and
#' optional TI, PY, DA, DO, and AB tags. The format uses CRLF line endings,
#' including after the final ER tag.
#'
#' @param articles List of [Article()] records.
#'
#' @return A length-one character string containing RIS records, or an
#'   empty string for zero articles.
#' @export
#'
#' @examples
#' articles_to_ris(list(Article(title = "Paper", authors = "Ada")))
articles_to_ris <- function(articles) {
  .check_export_articles(articles)
  records <- vapply(articles, function(article) {
    lines <- "TY  - JOUR"
    add_tag <- function(tag, value) {
      if (!is.na(value) && nzchar(value)) {
        lines <<- c(lines, paste0(tag, "  - ", .ris_value(value)))
      }
    }

    add_tag("TI", .export_field(article, "title"))
    authors <- .export_authors(article)
    for (author in authors) {
      add_tag("AU", author)
    }

    date <- .export_field(article, "publication_date")
    if (!is.na(date) && grepl("^[0-9]{4}(-[0-9]{2}){0,2}$", date)) {
      add_tag("PY", substr(date, 1L, 4L))
      if (nchar(date) > 4L) {
        add_tag("DA", gsub("-", "/", date, fixed = TRUE))
      }
    }
    add_tag("DO", .export_field(article, "doi"))
    add_tag("AB", .export_field(article, "abstract"))
    paste0(paste(c(lines, "ER  - "), collapse = "\r\n"), "\r\n")
  }, character(1L), USE.NAMES = FALSE)
  paste0(records, collapse = "")
}

.csv_formula_safe <- function(data) {
  for (name in names(data)) {
    values <- data[[name]]
    # Spreadsheet programs can interpret values beginning with formula
    # operators, even if CSV fields are quoted. Prefix with a literal quote.
    risky <- which(
      !is.na(values) & grepl("^[[:space:]]*[=+@-]", values, perl = TRUE)
    )
    values[risky] <- paste0("'", values[risky])
    data[[name]] <- values
  }
  data
}

#' Export article records to CSV, BibTeX, or RIS
#'
#' Writes one file in the selected format. CSV uses UTF-8 text, column names,
#' and base R's standard CSV quoting rules. By default CSV fields beginning
#' with potential spreadsheet formula operators are prefixed with a literal
#' apostrophe for safer opening in spreadsheet applications. This protection
#' affects CSV output only, not the original article records or data frame.
#'
#' RIS files are written with CRLF line endings as prescribed by RIS.
#' BibTeX is written as UTF-8, so classic BibTeX may need extra encoding
#' conversion for accented characters.
#'
#' @param articles List of [Article()] records.
#' @param file Nonempty target file path. Existing files are replaced.
#' @param format Either "csv", "bibtex", or "ris".
#' @param author_separator Scalar authors separator used in CSV only.
#' @param keys Optional BibTeX citation keys; ignored for other formats.
#' @param sanitize_csv Logical. Protect cells beginning with =, +, -, or @.
#'
#' @return The file path invisibly.
#' @export
#'
#' @examples
#' example <- list(Article(title = "Example", doi = "10.1000/example"))
#' target <- tempfile(fileext = ".bib")
#' export_articles(example, target, format = "bibtex")
#' unlink(target)
export_articles <- function(
    articles, file, format = c("csv", "bibtex", "ris"),
    author_separator = "; ", keys = NULL, sanitize_csv = TRUE) {
  .check_export_articles(articles)
  if (!is.character(file) || length(file) != 1L ||
      is.na(file) || !nzchar(trimws(file))) {
    stop("file must be a nonempty file path", call. = FALSE)
  }
  format <- match.arg(format)
  if (!is.logical(sanitize_csv) || length(sanitize_csv) != 1L ||
      is.na(sanitize_csv)) {
    stop("sanitize_csv must be TRUE or FALSE", call. = FALSE)
  }

  if (format == "csv") {
    data <- articles_to_data_frame(articles, author_separator)
    if (sanitize_csv) {
      data <- .csv_formula_safe(data)
    }
    utils::write.csv(
      data, file = file, row.names = FALSE, na = "", fileEncoding = "UTF-8"
    )
  } else {
    output <- if (format == "bibtex") {
      articles_to_bibtex(articles, keys = keys)
    } else {
      articles_to_ris(articles)
    }
    connection <- base::file(file, open = "wb")
    on.exit(close(connection), add = TRUE)
    writeBin(charToRaw(enc2utf8(output)), connection)
  }
  invisible(file)
}
