# DOI identity and conservative cross-source reconciliation.
# DOI comparison is ASCII-case-insensitive; Unicode suffixes are preserved.

.normalize_doi_one <- function(value) {
  if (is.na(value) || !nzchar(trimws(value))) {
    return(NA_character_)
  }
  doi <- trimws(value)
  resolver <- "^(?:https?://)?(?:dx\\.)?doi\\.org/"
  if (grepl(resolver, doi, perl = TRUE, ignore.case = TRUE)) {
    doi <- sub(resolver, "", doi, perl = TRUE, ignore.case = TRUE)
    doi <- sub("[?#].*$", "", doi, perl = TRUE)
    doi <- utils::URLdecode(doi)
  } else if (grepl("^(?:urn:)?doi:", doi, perl = TRUE,
                   ignore.case = TRUE)) {
    doi <- sub("^(?:urn:)?doi:[[:space:]]*", "", doi, perl = TRUE,
               ignore.case = TRUE)
    doi <- utils::URLdecode(doi)
  }
  doi <- trimws(doi)
  # Require an identifiable DOI prefix/suffix, not online registration.
  if (!grepl("^10\\.[0-9]+(?:\\.[0-9]+)*/.+$", doi, perl = TRUE) ||
      grepl("[[:cntrl:]]", doi)) {
    return(NA_character_)
  }
  chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", doi)
}

#' Normalize DOI identifiers for equality comparisons
#'
#' Accepts bare DOIs, doi: and urn:doi: identifiers and doi.org resolver URLs.
#' URI representations are decoded once; bare percent signs are preserved.
#' Matching is case-insensitive only for ASCII A-Z per DOI rules. The function
#' performs no network calls and does not verify DOI registration.
#'
#' @param dois Character vector of DOI values, including NA when missing.
#' @return Character vector of canonical DOI strings, NA for invalid values.
#' @export
#' @examples
#' normalize_doi(c(" DOI:10.1000/Example ",
#'                 "https://doi.org/10.1000/example"))
normalize_doi <- function(dois) {
  if (!is.character(dois)) {
    stop("dois must be a character vector", call. = FALSE)
  }
  out <- vapply(dois, .normalize_doi_one, character(1L), USE.NAMES = FALSE)
  names(out) <- names(dois)
  out
}

.article_present <- function(value) {
  is.character(value) && length(value) > 0L &&
    !anyNA(value) && any(nzchar(trimws(value)))
}

.validate_dedup_article <- function(article) {
  if (!is.list(article) || !inherits(article, "Article")) {
    stop("articles must contain Article records", call. = FALSE)
  }
  for (field in c("title", "abstract", "publication_date", "doi")) {
    value <- article[[field]]
    if (!is.null(value) &&
        (!is.character(value) || length(value) != 1L || is.na(value))) {
      stop(sprintf("Article field '%s' must be a scalar string or NULL",
                   field), call. = FALSE)
    }
  }
  if (!is.character(article[["authors"]]) || anyNA(article[["authors"]])) {
    stop("Article authors must be a character vector without NA",
         call. = FALSE)
  }
  invisible(NULL)
}

.compatible_article_dates <- function(a, b) {
  pattern <- "^[0-9]{4}(-[0-9]{2}){0,2}$"
  if (!grepl(pattern, a) || !grepl(pattern, b)) {
    return(FALSE)
  }
  identical(a, b) ||
    startsWith(a, paste0(b, "-")) || startsWith(b, paste0(a, "-"))
}

.format_article_conflict <- function(value) {
  paste(value, collapse = " | ")
}

#' Deduplicate and reconcile scholarly articles using exact DOI identity
#'
#' Combines Scopus and Crossref Article records by normalized DOI, preserving
#' first-occurrence order and list names. Records with missing or malformed
#' DOIs remain separate, even when titles match.
#'
#' The first record has field priority. Later duplicates fill missing titles,
#' abstracts and authors, and may refine compatible partial dates (2024 vs.
#' 2024-06-03). Differing populated values are reported, not silently mixed.
#' Returned valid DOI fields are normalized; input records remain unchanged.
#'
#' @param articles List of Article records, such as the concatenation of
#'   normalized Scopus and Crossref results.
#' @param on_conflict One of "keep_first" (default), "warn" or "error".
#' @param report If TRUE, return an audit report as well as the articles.
#' @return Reconciled list of Article records, or a list of three components
#'   when report is TRUE: articles, mapping and conflicts. The mapping includes
#'   input_index, output_index, original_doi, canonical_doi, status.
#'   Status is unique, duplicate or unkeyed. Conflicts identify which fields
#'   were retained and ignored, without losing the original input.
#' @export
#' @examples
#' scopus <- list(Article(title = "Study", doi = "10.1000/Example"))
#' crossref <- list(Article(
#'   abstract = "Recovered", doi = "https://doi.org/10.1000/example"
#' ))
#' deduplicate_articles(c(scopus, crossref), report = TRUE)
deduplicate_articles <- function(
    articles, on_conflict = c("keep_first", "warn", "error"),
    report = FALSE) {
  if (!is.list(articles)) {
    stop("articles must be a list of Article records", call. = FALSE)
  }
  for (article in articles) {
    .validate_dedup_article(article)
  }
  on_conflict <- match.arg(on_conflict)
  if (!is.logical(report) || length(report) != 1L || is.na(report)) {
    stop("report must be TRUE or FALSE", call. = FALSE)
  }

  n <- length(articles)
  original_dois <- vapply(articles, function(article) {
    if (is.null(article$doi)) NA_character_ else article$doi
  }, character(1L), USE.NAMES = FALSE)
  canonical_dois <- normalize_doi(original_dois)
  mapping <- data.frame(
    input_index = seq_len(n),
    output_index = integer(n),
    original_doi = original_dois,
    canonical_doi = canonical_dois,
    status = rep("", n),
    stringsAsFactors = FALSE
  )
  conflicts <- list()
  seen <- new.env(hash = TRUE, parent = emptyenv())
  # Preallocate the upper bound; trim after reconciling duplicate DOIs.
  output <- vector("list", n)
  output_count <- 0L

  for (i in seq_len(n)) {
    article <- articles[[i]]
    doi <- canonical_dois[[i]]
    if (is.na(doi)) {
      output_count <- output_count + 1L
      output[output_count] <- articles[i]
      mapping$output_index[[i]] <- output_count
      mapping$status[[i]] <- "unkeyed"
      next
    }
    article$doi <- doi
    key <- paste0("doi:", doi)
    if (!exists(key, envir = seen, inherits = FALSE)) {
      part <- articles[i]
      part[[1L]] <- article
      output_count <- output_count + 1L
      output[output_count] <- part
      assign(key, output_count, envir = seen)
      mapping$output_index[[i]] <- output_count
      mapping$status[[i]] <- "unique"
      next
    }

    index <- get(key, envir = seen, inherits = FALSE)
    mapping$output_index[[i]] <- index
    mapping$status[[i]] <- "duplicate"
    primary <- output[[index]]

    register_conflict <- function(field, kept, ignored) {
      if (on_conflict == "error") {
        stop(sprintf("Conflicting %s metadata for DOI %s (input %d)",
                     field, doi, i), call. = FALSE)
      }
      conflicts[[length(conflicts) + 1L]] <<- data.frame(
        input_index = i,
        output_index = index,
        canonical_doi = doi,
        field = field,
        kept = .format_article_conflict(kept),
        ignored = .format_article_conflict(ignored),
        stringsAsFactors = FALSE
      )
    }

    for (field in c("title", "abstract", "publication_date")) {
      first <- primary[[field]]
      second <- article[[field]]
      if (!.article_present(second)) {
        next
      }
      if (!.article_present(first)) {
        primary[[field]] <- second
        next
      }
      if (identical(trimws(first), trimws(second))) {
        next
      }
      if (field == "publication_date" &&
          .compatible_article_dates(first, second)) {
        if (nchar(second) > nchar(first)) {
          primary[[field]] <- second
        }
        next
      }
      register_conflict(field, first, second)
    }

    if (.article_present(article$authors)) {
      if (!.article_present(primary$authors)) {
        primary$authors <- article$authors
      } else if (!identical(trimws(primary$authors),
                            trimws(article$authors))) {
        register_conflict("authors", primary$authors, article$authors)
      }
    }
    output[[index]] <- primary
  }

  output <- output[seq_len(output_count)]
  # Slice assignment to a preallocated list does not carry names from the
  # source list. Restore only names of first occurrences and DOI-free rows.
  # Fully unnamed inputs must remain unnamed, as before preallocation.
  if (!is.null(names(articles))) {
    retained <- mapping$status != "duplicate"
    names(output) <- names(articles)[retained]
  }

  conflict_table <- if (length(conflicts) == 0L) {
    data.frame(
      input_index = integer(), output_index = integer(),
      canonical_doi = character(), field = character(),
      kept = character(), ignored = character(), stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, conflicts)
  }
  if (on_conflict == "warn" && nrow(conflict_table) > 0L) {
    warning(
      sprintf("Found %d conflicting metadata fields across %d input records",
              nrow(conflict_table),
              length(unique(conflict_table$input_index))),
      call. = FALSE
    )
  }
  if (report) {
    return(list(articles = output, mapping = mapping,
                conflicts = conflict_table))
  }
  output
}
