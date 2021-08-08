#' letter
#'
#' Revise and resubmit letter
#' @param ... arguments passed to papaja:::revision_letter_pdf
#' @export
letter <- function(...) {
  extra_tex <-
    rmarkdown::includes(in_header = system.file("header.tex", package = "revise"))

  papaja:::revision_letter_pdf(...,
                               includes = extra_tex)
}
