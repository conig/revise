with_reviewer_engine <- function(format, engine, reset = NULL) {
  pre_knit <- format$pre_knit
  on_exit <- format$on_exit
  previous_engines <- list()

  format$pre_knit <- function(input, ...) {
    if (is.function(pre_knit)) pre_knit(input = input, ...)
    # rmarkdown also calls pre_knit for plain Markdown, without on_exit.
    if (!tolower(tools::file_ext(input)) %in% c("r", "rmd", "rmarkdown", "qmd")) {
      return(invisible(NULL))
    }
    if (!is.null(reset)) reset()
    engines <- getOption("reviewer_chunkname")
    previous_engines <<- knitr::knit_engines$set(
      stats::setNames(rep(list(engine), length(engines)), engines)
    )
  }
  format$on_exit <- function() {
    knitr::knit_engines$delete(names(previous_engines))
    knitr::knit_engines$set(Filter(Negate(is.null), previous_engines))
    if (is.function(on_exit)) on_exit()
  }

  format
}

#' Dynamic PDF revision letter output
#'
#' R Markdown output format for creating PDF journal revision letters with
#' dynamic excerpts from a tagged manuscript.
#' @param ... Additional arguments passed on to [papaja::revision_letter_pdf()].
#'   `revise_letter_pdf()` supplies the `includes` argument so that revise's
#'   LaTeX header is used; callers should not pass `includes` through `...`.
#' @param comment_reset_by_section Logical scalar. Should reviewer comment
#'   numbering reset at each section header (for example, for each reviewer)?
#'   Defaults to `TRUE`. Pass `comment_reset_by_section = FALSE` by name for continuous numbering.
#' @return An R Markdown output format object for [rmarkdown::render()].
#' @details This function wraps [papaja::revision_letter_pdf()], installs the
#'   revise knitr engine for PDF revision chunks, and adds the package LaTeX
#'   header required for reviewer-comment cross-references.
#' @seealso [papaja::revision_letter_pdf()], [bookdown::pdf_document2()], [rmarkdown::pdf_document()]
#' @export
revise_letter_pdf <- function(..., comment_reset_by_section = TRUE) {
  if (!is.logical(comment_reset_by_section) || length(comment_reset_by_section) != 1L || is.na(comment_reset_by_section)) {
    stop("`comment_reset_by_section` must be TRUE or FALSE.")
  }

  headers <- "header.tex"
  if (!comment_reset_by_section) headers <- c(headers, "header_no_section_reset.tex")
  format <- papaja::revision_letter_pdf(
    ..., includes = rmarkdown::includes(
      in_header = system.file(headers, package = "revise")
    )
  )
  format$knitr$opts_chunk$escape <- TRUE
  with_reviewer_engine(format, process_chunk_pdf)
}

#' Dynamic DOCX revision letter output
#'
#' R Markdown output format for creating Word journal revision letters with
#' dynamic excerpts from a tagged manuscript.
#' @param ... Additional arguments passed on to [bookdown::word_document2()].
#'   `revise_letter_docx()` supplies the `reference_docx` argument so that
#'   revise's response-letter template is used; callers should not pass
#'   `reference_docx` through `...`.
#' @return An R Markdown output format object for [rmarkdown::render()].
#' @details This function wraps [bookdown::word_document2()], installs the
#'   revise knitr engine for DOCX revision chunks, and converts reviewer-comment
#'   cross-references to numbered plain text.
#' @seealso [bookdown::word_document2()]
#' @export
revise_letter_docx <- function(...) {
  format <- bookdown::word_document2(
    ..., reference_docx = system.file("response_letter_template.docx", package = "revise")
  )

  labels <- character()
  format <- with_reviewer_engine(format, function(options) {
    labels <<- c(labels, options$label)
    process_chunk_docx(options, length(labels))
  }, reset = function() labels <<- character())

  document_hook <- format$knitr$knit_hooks$document
  format$knitr$knit_hooks$document <- function(x) {
    if (is.function(document_hook)) x <- document_hook(x)
    # All labels are now known, including references to later chunks.
    replace_docx_comment_macros(x, labels)
  }
  format
}

#' Dynamic plain-text revision letter output
#'
#' R Markdown output format for creating plain-text journal revision letters
#' with dynamic excerpts from a tagged manuscript.
#' @param ... Additional arguments passed on to [rmarkdown::output_format()].
#'   `revise_letter_txt()` supplies the knitr options, pandoc options, and
#'   post-processor required for revise's plain-text output; callers should not
#'   pass `knitr`, `pandoc`, or `post_processor` through `...`.
#' @return An R Markdown output format object for [rmarkdown::render()].
#' @details This function wraps [rmarkdown::output_format()] and installs the
#'   revise knitr engine for plain-text revision chunks.
#' @seealso [rmarkdown::output_format()]
#' @export
revise_letter_txt <- function(...) {
  format <- rmarkdown::output_format(
    rmarkdown::knitr_options(opts_chunk = list(
      echo = FALSE,
      format = "txt"
    )),
    pandoc = rmarkdown::pandoc_options(to = "plain"),
    post_processor = letter_post_txt,
    ...
  )
  with_reviewer_engine(format, process_chunk_txt)
}

letter_post_txt <- function(front_matter, input, output_file, clean, quiet = FALSE) {
  lines <- readLines(output_file, encoding = "UTF-8")
  target <- sub("\\.plain$", ".txt", output_file)

  letter_head <- readLines(system.file("header.txt", package = "revise"))

  letter_head <- gsub("\\{\\{Title\\}\\}", stringr::str_wrap(front_matter$title, 72), letter_head)
  letter_head <- gsub("\\{\\{Author\\}\\}", front_matter$authors, letter_head)
  letter_head <- gsub("\\{\\{Journal\\}\\}", front_matter$journal, letter_head)
  letter_head <- gsub("\\{\\{ID\\}\\}", front_matter$manuscript, letter_head)

  lines <- c(letter_head, lines)

  write(lines, target)
  if (target != output_file) unlink(output_file)
  target
}

# letter
#
# This function is deprecated
# @param ... arguments passed to letter.pdf
# @export

# letter <- function(...){
#   .Deprecated("letter.pdf", package = "revise", msg = "revise::letter is deprecated.\n\nPlease specify the type of document required...\n- revise::letter.pdf\n- revise::letter.txt\n- revise::letter.docx")
#   letter.pdf(...)
#
# }
