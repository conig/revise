replace_docx_comment_macros <- function(text, labels) {
  pattern <- "\\\\{1,2}([Cc]omment)\\{([^{}]+)\\}"
  stringr::str_replace_all(text, pattern, function(tokens) {
    parts <- stringr::str_match(tokens, pattern)
    label <- parts[, 3]
    # If knitr permits duplicate labels, resolve them to the last occurrence.
    index <- match(label, rev(labels))
    paste(parts[, 2], ifelse(is.na(index), label, length(labels) - index + 1L))
  })
}

process_chunk_pdf <- function(options) {

  code <- paste(options$code, collapse = "\n")

  if(options$escape) code <- escape_latex(code)

    # if the format is PDF
    start <- paste0("\\reviewerid{", options$label, "}{")

    paste(start, "\n" , code , "}", sep = "\n")

}

process_chunk_txt <- function(options) {
  code <- paste(options$code, collapse = " ")

    comment_label <- options$label

    if(grepl("unnamed-chunk", comment_label)) {
      comment_label <-
        as.numeric(paste(unlist(
          stringr::str_extract_all(comment_label, "[0-9]")
        ), collapse = ""))
    }

    glue::glue("____\nCOMMENT {comment_label}\n\n -\nRC:\n{code}\n\n")

}

process_chunk_docx <- function(options, number) {
  code <- options$code
  nonempty <- nzchar(code)
  code[nonempty] <- paste0("**", code[nonempty], "**")
  code <- paste(code, collapse = "\n")
  glue::glue("____\n<span class='underline'>**COMMENT {number}**</span>\n\n \n**RC:**\n{code}</br>\n\n")
}
