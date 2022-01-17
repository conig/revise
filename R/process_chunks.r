process_chunk <- function(options) {

  code <- paste(options$code, collapse = "")

  if (!is.null(options$format)) { # if the format is a txt file

    comment_label <- options$label

    if(grepl("unnamed-chunk", comment_label)) {
      comment_label <-
        as.numeric(paste(unlist(
          stringr::str_extract_all(comment_label, "[0-9]")
        ), collapse = ""))
    }

    return(glue::glue("____\nComment {comment_label}\n\n -\nRC:\n{code}\n\n"))
  } else{
    # if the format is PDF
    start <- paste0("\\reviewer{", options$label, "}{")

    paste(start, "\n" , code , "}", sep = "\n")

  }

}
