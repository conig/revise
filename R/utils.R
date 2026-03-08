checksum_exists <- function(corpus, checksum) {
  UseMethod("checksum_exists", corpus)
}
#' @export
checksum_exists.revise_corpus <- function(corpus, checksum) {
  all_csums <- sapply(corpus, `[[`, "checksum")
  return(isTRUE(checksum %in% all_csums))
}

#' @export
checksum_exists.revise_manuscript <- function(corpus, checksum) {
  return(isTRUE(checksum == corpus[["checksum"]]))
}

#' read_md
#'
#' Read in a markdown document for further analysis
#' @param path to markdown manuscript

read_md <- function(path) {
  unname(unlist(utils::read.delim(
    path,
    sep = "\n",
    quote = "",
    header = FALSE
  )))
}

#' extract_refs
#'
#' Extract table and figure references
#' @param path to markdown manuscript

extract_refs <- function(path) {
  lines <- read_md(path)
  lines <- paste(lines, collapse = "\n")
  references <- unlist(stringr::str_extract_all(lines, "\\\\\\@ref.+?\\)"))
  figures <- data.frame(text = unique(grep("fig:", references, value = TRUE)))
  tables <- data.frame(text = unique(grep("tab:", references, value = TRUE)))
  figures$ref <- seq_along(figures[, 1])
  tables$ref <- seq_along(tables[, 1])
  list(figures = figures, tables = tables)
}


#' @export
#' @importFrom cli cli_h2
print.revise_manuscript = function(x, ...) {
  cli::cli_h2("<Manuscript>")
  cli_msg("*" = "{length(x$sec)} section{c('s', '')[(length(x$sec) == 1)+1L]}")
  if (is.null(x$PDF)) {
    cli_msg("x" = "No PDF attached")
  } else {
    cli_msg("*" = "{nrow(x$PDF)} pages")
  }
}

#' evaluate_inline
#'
#' Evaluates embedded inline r code within a string of text
#' @param string a section of text containing inline `r` elements
#' @param envir the environment in which to evaluate the code
#' @param trust_manuscript logical. Should inline `r` code be trusted and
#'   evaluated? Defaults to `FALSE`. When called via [get_revision()], the default
#'   comes from `getOption("revise_trust_manuscript")`.
#' @return A character string with inline `r` code evaluated.
#' @details
#' Inline `r` code is detected using backticks and evaluated via
#' [glue::glue()] with `.open = "`r "` and `.close = "`"`.
#' If inline `r` chunks are present and `trust_manuscript` is not `TRUE`,
#' this function errors with the inline code that would have been evaluated.
#' Set `options(revise_trust_manuscript = TRUE)` and call [get_revision()], or pass
#' `trust_manuscript = TRUE` directly, or disable evaluation in
#' [get_revision()] by setting `evaluate = FALSE`.
#'
#' @examples
#' revise:::evaluate_inline("1+1 = `r 1+1`")

evaluate_inline <- function(
  string,
  envir = parent.frame(),
  trust_manuscript = FALSE
) {
  inline_matches <- unlist(stringr::str_extract_all(string, "`r\\s+[^`]*`"))
  if (length(inline_matches) > 0 && !isTRUE(trust_manuscript)) {
    inline_code <- trimws(gsub("^`r\\s+|`$", "", inline_matches))
    inline_code <- unique(inline_code[nzchar(inline_code)])
    inline_items <- if (length(inline_code) > 0) inline_code else inline_matches
    inline_items <- unique(inline_items[nzchar(inline_items)])
    if (length(inline_items) == 0) {
      inline_items <- unique(inline_matches)
    }
    inline_list <- paste0("  - `", inline_items, "`", collapse = "\n")
    inline_section <- paste(
      "The string(s) to evaluate are:",
      inline_list,
      sep = "\n"
    )
    message <- paste(
      "revise has detected inline r code to evaluate.",
      inline_section,
      paste(
        "Please ensure you trust the manuscript before executing its code.",
        "To proceed do one of the following:",
        "  1) set argument trust_manuscript=TRUE",
        "  2) set option(revise_trust_manuscript=TRUE)",
        "  3) in get_revision set evaluate=FALSE so inline code is not executed",
        "Do not trust manuscripts unless you wrote them, or have reviewed all in-line code for safety.",
        sep = "\n"
      ),
      sep = "\n\n"
    )
    stop(message, call. = TRUE)
  }
  glue::glue(string, .open = "`r ", .close = "`", .envir = envir)
}

# process_pdf
#
# process pdf file
# @param path path to pdf

process_pdf <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("pdf")) {
    stop("Extracting page numbers only works for pdf documents")
  }
  doc <- get_pdf_text(path)
  running_head = gsub("\n.*", "", doc$text[2])
  running_head = trimws(gsub("[0-9]", "", running_head))
  running_head = gsub("\\s{1,}", " ", running_head)
  is_head <- unlist(lapply(doc$text, function(x) grepl(running_head, x)))
  prop_head <- prop.table(table(is_head))["TRUE"]

  doc$text <- tolower(doc$text)
  if (!is.null(running_head)) {
    doc$text <- stringr::str_remove_all(
      doc$text,
      tolower(glue::glue("^{running_head}"))
    )
  }

  doc <- dplyr::summarise(
    dplyr::group_by(doc, page_id),
    text = paste(text, collapse = " "),
    .groups = "drop"
  )

  doc$text <- trimws(stringr::str_remove_all(doc$text, "[[:digit:]]")) # remove all numbers
  doc$text <- stringr::str_remove_all(doc$text, "\\[.{0,50}\\]") # remove square brackets
  doc$text <- stringr::str_remove_all(doc$text, "\\(.{0,50}\\)") # remove parentheses
  doc$text <- trimws(stringr::str_remove_all(doc$text, "[[:punct:]]")) # remove all punctuation
  if (prop_head > .7) {
    attr(doc, "running_head") <- running_head
  } else {
    attr(doc, "running_head") <- NULL
  }
  doc
}

# find_pages
#
# Searches through a manuscript for a page reference
# @param manuscript the manuscript object to search
# @param string the string to find
# @export

find_pages <- function(manuscript, string) {
  if (is.null(manuscript$PDF)) {
    stop(
      "No PDF attached to the manuscript object: not possible to identify page numbers."
    )
  }
  get_pdf_pagenumber(string, pdf_text = manuscript$PDF)
}

# clean_string
#
# Cleaning steps for identifying strings
# @param string string to clean

clean_string <- function(string) {
  string <- gsub("\\[.{0,50}\\]", "", string) # remove square brackets
  string <- gsub("\\*|\\#", "", string) # remove rmarkdown formatting
  string <- gsub("[0-9]", "", string) # remove numbers
  string <- gsub("\\(.{0,50}\\)", "", tolower(string)) # remove parentheses
  string <- gsub("\\n", " ", string)
  string <- gsub("[[:punct:]]", "", tolower(string)) # remove punctuation
  trimws(gsub("\\s{2,}", " ", string)) # remove additional spaces
}

#' Detect empty page-match results
#'
#' Treats zero-length, blank, and whitespace-only page IDs as missing so the
#' page matching pipeline can distinguish failed matches from legitimate page
#' labels.
#' @param page_id Character vector returned by page matching helpers.
#' @return Logical scalar indicating whether the match should be treated as missing.
page_match_is_missing <- function(page_id) {
  length(page_id) == 0 || !any(nzchar(trimws(page_id)))
}

#' Build searchable page candidates from PDF text
#'
#' Creates a candidate table containing both single-page text and adjacent
#' two-page windows. This allows later matching to detect text that falls
#' wholly on one page or straddles a page break.
#' @param pdf_text Data frame of processed PDF text with `page_id` and `text`.
#' @return Data frame with candidate `page_id` labels and cleaned candidate text.
build_pdf_page_candidates <- function(pdf_text) {
  if (length(pdf_text$text) == 0) {
    return(data.frame(
      page_id = character(),
      text = character(),
      stringsAsFactors = FALSE
    ))
  }

  page_ids <- as.character(pdf_text$page_id)
  doc <- clean_string(pdf_text$text)
  singles <- data.frame(
    page_id = page_ids,
    text = doc,
    stringsAsFactors = FALSE
  )
  singles <- singles[nzchar(singles$text), , drop = FALSE]

  if (length(doc) < 2) {
    return(singles)
  }

  windows <- data.frame(
    page_id = paste0(page_ids[-length(page_ids)], "--", page_ids[-1]),
    text = paste(doc[-length(doc)], doc[-1]),
    stringsAsFactors = FALSE
  )
  windows <- windows[nzchar(windows$text), , drop = FALSE]

  rbind(singles, windows)
}

#' Find an exact page match
#'
#' Searches the candidate text for an exact fixed-string occurrence of the
#' cleaned target string. Exact matches are preferred over fuzzy scoring.
#' @param string Cleaned target string.
#' @param candidates Data frame produced by [build_pdf_page_candidates()].
#' @return Matching page ID, or `character(0)` when no exact match is found.
find_exact_page_match <- function(string, candidates) {
  matches <- which(vapply(
    candidates$text,
    function(text) grepl(string, text, fixed = TRUE),
    logical(1)
  ))

  if (length(matches) == 0) {
    return(character(0))
  }

  candidates$page_id[matches[1]]
}

#' Build contiguous word n-grams
#'
#' Generates contiguous phrases from a token vector so approximate page
#' matching can score how much of a target passage survives cleaning and PDF
#' extraction.
#' @param tokens Character vector of cleaned word tokens.
#' @param n Size of each n-gram.
#' @return Character vector of unique contiguous n-grams.
build_word_ngrams <- function(tokens, n) {
  if (length(tokens) == 0) {
    return(character(0))
  }
  if (length(tokens) <= n) {
    return(paste(tokens, collapse = " "))
  }

  starts <- seq_len(length(tokens) - n + 1)
  unique(vapply(starts, function(i) {
    paste(tokens[i:(i + n - 1)], collapse = " ")
  }, character(1)))
}

#' Score phrase coverage within a candidate page
#'
#' Estimates how much of the cleaned target passage is present in a candidate by
#' checking the proportion of target n-grams that appear verbatim in that
#' candidate.
#' @param string Cleaned target string.
#' @param candidate_text Cleaned candidate page or page-window text.
#' @return Numeric coverage score between 0 and 1.
score_pdf_candidate_coverage <- function(string, candidate_text) {
  if (!nzchar(string) || !nzchar(candidate_text)) {
    return(0)
  }

  string_tokens <- strsplit(string, "\\s+")[[1]]
  if (length(string_tokens) == 0) {
    return(0)
  }

  ngram_n <- max(3, min(8, length(string_tokens)))
  ngrams <- build_word_ngrams(string_tokens, ngram_n)
  if (length(ngrams) == 0) {
    return(0)
  }

  mean(vapply(ngrams, function(ngram) {
    grepl(ngram, candidate_text, fixed = TRUE)
  }, logical(1)))
}

#' Score approximate edit-distance fit for a candidate page
#'
#' Slides a window over the candidate text and computes the best normalized edit
#' distance against the cleaned target. This is used as a fallback when phrase
#' coverage is not decisive.
#' @param string Cleaned target string.
#' @param candidate_text Cleaned candidate page or page-window text.
#' @return Numeric distance score where smaller values are better.
score_pdf_candidate <- function(string, candidate_text) {
  if (!nzchar(string) || !nzchar(candidate_text)) {
    return(Inf)
  }

  string_tokens <- strsplit(string, "\\s+")[[1]]
  candidate_tokens <- strsplit(candidate_text, "\\s+")[[1]]

  if (length(string_tokens) == 0 || length(candidate_tokens) == 0) {
    return(Inf)
  }

  target_n <- length(string_tokens)
  if (length(candidate_tokens) <= target_n) {
    windows <- candidate_text
  } else {
    starts <- seq_len(length(candidate_tokens) - target_n + 1)
    windows <- vapply(starts, function(i) {
      paste(candidate_tokens[i:(i + target_n - 1)], collapse = " ")
    }, character(1))
  }

  distances <- vapply(windows, function(window) {
    utils::adist(string, window, ignore.case = TRUE)[1] /
      max(nchar(string), nchar(window), 1L)
  }, numeric(1))

  min(distances)
}

#' Find the best approximate page match
#'
#' First ranks candidates by phrase coverage, then falls back to normalized
#' edit-distance scoring when coverage is weak. Returns no match when the best
#' candidate still exceeds the allowed distance threshold.
#' @param string Cleaned target string.
#' @param candidates Data frame produced by [build_pdf_page_candidates()].
#' @param max.distance Maximum allowed normalized edit distance.
#' @return Matching page ID, or `character(0)` when no acceptable match is found.
find_approximate_page_match <- function(string, candidates, max.distance) {
  coverage_scores <- vapply(
    candidates$text,
    function(text) score_pdf_candidate_coverage(string, text),
    numeric(1)
  )
  best_coverage <- which.max(coverage_scores)

  if (length(best_coverage) > 0 && is.finite(coverage_scores[best_coverage]) &&
      coverage_scores[best_coverage] >= 0.3) {
    return(candidates$page_id[best_coverage])
  }

  distance_scores <- vapply(
    candidates$text,
    function(text) score_pdf_candidate(string, text),
    numeric(1)
  )
  best <- which.min(distance_scores)

  if (!length(best) || !is.finite(distance_scores[best]) ||
      distance_scores[best] > max.distance) {
    return(character(0))
  }

  candidates$page_id[best]
}

# get_page_number
#
# Finds page number from pdf based on text matching
# @param string text to match
# @param pdf_text text to search
# @param max.distance maximum normalized distance for approximate matches

get_pdf_pagenumber = function(string, pdf_text, max.distance = .15) {
  string <- clean_string(string)

  if (!nzchar(string)) {
    return(character(0))
  }

  candidates <- build_pdf_page_candidates(pdf_text)
  if (nrow(candidates) == 0) {
    return(character(0))
  }

  pnum <- find_exact_page_match(string, candidates)
  if (!page_match_is_missing(pnum)) {
    return(pnum)
  }

  find_approximate_page_match(string, candidates, max.distance = max.distance)
}

# header_to_bold
#
# Converts headers to bold
# @param string a string
# @return string

header_to_bold = function(string) {
  while (grepl("(?<!\\{)#{1,}.{0,100}\\\n", string, perl = TRUE)) {
    target <- stringr::str_extract(string, "(?<!\\{)#{1,}.+?(\\n){1,}")
    n_hash <- sum(strsplit(target, split = "")[[1]] == "#")
    replacement <- gsub("(?<!\\{)#{1,}\\s?", "**", target, perl = TRUE)
    if (n_hash < 3) {
      replacement <- gsub("\\n{1,}", "**\\\n\\\n", replacement)
    } else {
      replacement <- gsub("\\n{1,}", ".** ", replacement)
    }
    string <- gsub(target, replacement, string)
  }

  string
}


utils::globalVariables(c("text", "page_id", ".revise_manuscripts"))

check_dup_sections <- function(
  sect_nams,
  revise_errors = getOption("revise_errors")
) {
  if (any(duplicated(sect_nams))) {
    m <- paste(
      "The following sections have duplicate names. When referencing sections by name, the first section with that name will be selected. Please use unique section names:\n",
      paste0("  '", sect_nams[duplicated(sect_nams)], "'\n")
    )

    if (!revise_errors) {
      warning(m, call. = FALSE)
    } else {
      stop(m, call. = FALSE)
    }
  }
}

.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("revise_errors"))) {
    options(revise_errors = TRUE)
  }

  if (is.null(getOption("reviewer_chunkname"))) {
    options(reviewer_chunkname = c("asis", "reviewer"))
  }
}

# path
#
# Determines whether a path is to a .doc or .docx
# @param path to file

is_docx <- function(path) {
  ext <- tolower(tools::file_ext(path))
  ext %in% c("doc", "docx")
}
