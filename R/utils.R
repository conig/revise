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
#'   comes from `getOption("revise_trust_manuscript", FALSE)`.
#' @return A character string with inline `r` code evaluated.
#' @details
#' [glue::glue()] detects inline R expressions delimited by backticks.
#' If inline `r` chunks are present and `trust_manuscript` is not `TRUE`,
#' this function errors with the inline code that would have been evaluated.
#' Set `options(revise_trust_manuscript = TRUE)` and call [get_revision()], or pass
#' `trust_manuscript = TRUE` directly, or disable evaluation in
#' [get_revision()] by setting `evaluate = FALSE`.
#'
#' @examples
#' revise:::evaluate_inline("1+1 = `r 1+1`", trust_manuscript = TRUE)

evaluate_inline <- function(
  string,
  envir = parent.frame(),
  trust_manuscript = FALSE
) {
  expressions <- character()
  transformer <- if (isTRUE(trust_manuscript)) {
    glue::identity_transformer
  } else {
    function(text, envir) {
      # Use glue's own parser to report exactly what it would evaluate.
      expressions <<- c(expressions, trimws(text))
      ""
    }
  }
  result <- glue::glue(
    string, .open = "`r ", .close = "`", .envir = envir, .transformer = transformer
  )
  if (length(expressions)) {
    stop(
      "revise has detected inline r code to evaluate:\n",
      paste0("  - `", unique(expressions), "`", collapse = "\n"),
      "\nOnly allow evaluation if you trust the manuscript. Set ",
      "trust_manuscript = TRUE (or options(revise_trust_manuscript = TRUE) ",
      "for get_revision()). To insert the excerpt without running code, ",
      "use get_revision(..., evaluate = FALSE)."
    )
  }
  as.character(result)
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


utils::globalVariables(".revise_manuscripts")

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
