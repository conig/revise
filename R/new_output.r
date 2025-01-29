set_engine <- function(process_chunk){

  envir <- parent.frame()

  command <- glue::glue(
    "knitr::knit_engines$set({getOption('reviewer_chunkname')} = {process_chunk})"
  )

  for(i in command) eval(str2lang(i), envir = envir)

}

#' Dynamic PDF Revision Letter
#'
#' Template for creating journal revision letters
#' with dynamic excerpts from a manuscript.
#' @param ... Arguments passed on to [papaja::revision_letter_pdf()].
#' @details This function wraps [papaja::revision_letter_pdf()].
#' @seealso [papaja::revision_letter_pdf()], [bookdown::pdf_document2()], [rmarkdown::pdf_document()]
#' @inherit papaja::apa6_pdf return
#' @export
revise_letter_pdf <- function(...) {
  extra_tex <-
    rmarkdown::includes(in_header = system.file("header.tex", package = "revise"))

  knitr::opts_chunk$set(escape = TRUE)

  set_engine("process_chunk_pdf")

  # knitr::knit_engines$set(reviewer = process_chunk_pdf,
  #                         asis = process_chunk_pdf)

  papaja::revision_letter_pdf(...,
                               includes = extra_tex)
}

#' Dynamic docx Revision Letter
#'
#' Template for creating journal revision letters
#' with dynamic excerpts from a manuscript.
#' @param ... Arguments passed on to [bookdown::word_document2()].
#' @details This function wraps [bookdown::word_document2()].
#' @seealso [bookdown::word_document2()]
#' @inherit bookdown::word_document2 return
#' @export
revise_letter_docx <- function(...) {
  # knitr::knit_engines$set(reviewer = process_chunk_docx,
  #                         asis = process_chunk_docx)

  set_engine("process_chunk_docx")

  bookdown::word_document2(...,
                           reference_docx = system.file("response_letter_template.docx", package = "revise"))

}

#' Dynamic txt Revision Letter
#'
#' Template for creating journal revision letters
#' with dynamic excerpts from a manuscript.
#' @param ... Arguments passed on to [rmarkdown::output_format()].
#' @details This function wraps [rmarkdown::output_format()].
#' @seealso [rmarkdown::output_format()]
#' @inherit rmarkdown::output_format return
#' @export
revise_letter_txt <- function(...) {
  extra_tex <-
    rmarkdown::includes(in_header = system.file("header.tex", package = "revise"))

    set_engine("process_chunk_txt")

    rmarkdown::output_format(
      rmarkdown::knitr_options(opts_chunk = list(echo = FALSE,
                                                 format = "txt")),
      pandoc = rmarkdown::pandoc_options(to = "plain"),
      post_processor = letter_post_txt,
      ...
    )
}

letter_post_txt <- function(front_matter, input , output_file, clean, quiet = FALSE){

  lines <- readLines(output_file, encoding = "UTF-8")
  unlink(output_file)
  output_file <- gsub("\\.plain", ".txt", output_file)

  letter_head <- readLines(system.file("header.txt", package = "revise"))

  letter_head <- gsub("\\{\\{Title\\}\\}", stringr::str_wrap(front_matter$title, 72), letter_head)
  letter_head <- gsub("\\{\\{Author\\}\\}",front_matter$authors, letter_head)
  letter_head <- gsub("\\{\\{Journal\\}\\}",front_matter$journal, letter_head)
  letter_head <- gsub("\\{\\{ID\\}\\}",front_matter$manuscript, letter_head)

  lines <- c(letter_head, lines)

  write(lines, output_file)
  output_file

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
