init_docx_comment_state <- function() {
  state <- new.env(parent = emptyenv())
  state$counter <- 0L
  state$label_to_number <- new.env(parent = emptyenv())

  options(revise.docx_comment_state = state)
  invisible(state)
}

get_docx_comment_state <- function() {
  state <- getOption("revise.docx_comment_state")

  if (
    is.null(state) ||
    !is.environment(state) ||
    is.null(state$counter) ||
    is.null(state$label_to_number) ||
    !is.environment(state$label_to_number)
  ) {
    state <- init_docx_comment_state()
  }

  state
}

register_docx_comment <- function(label) {
  state <- get_docx_comment_state()
  state$counter <- state$counter + 1L

  label <- as.character(label)[1]
  if (!is.na(label) && nzchar(label)) {
    assign(label, state$counter, envir = state$label_to_number)
  }

  state$counter
}

lookup_docx_comment <- function(label) {
  label <- as.character(label)[1]
  if (is.na(label) || !nzchar(label)) {
    return(NA_integer_)
  }

  state <- get_docx_comment_state()
  if (!exists(label, envir = state$label_to_number, inherits = FALSE)) {
    return(NA_integer_)
  }

  get(label, envir = state$label_to_number, inherits = FALSE)
}

replace_docx_comment_macro <- function(text, command) {
  pattern <- paste0("\\\\", command, "\\{([^{}]+)\\}")
  out <- text

  for (i in seq_along(out)) {
    matches <- gregexpr(pattern, out[[i]], perl = TRUE)
    if (identical(matches[[1]], -1L)) {
      next
    }

    tokens <- regmatches(out[[i]], matches)[[1]]
    replacements <- vapply(tokens, function(token) {
      label <- sub(pattern, "\\1", token, perl = TRUE)
      number <- lookup_docx_comment(label)

      if (is.na(number)) {
        paste(command, label)
      } else {
        paste(command, number)
      }
    }, character(1))

    regmatches(out[[i]], matches) <- list(replacements)
  }

  out
}

replace_docx_comment_macros <- function(text) {
  text <- replace_docx_comment_macro(text, "Comment")
  replace_docx_comment_macro(text, "comment")
}

process_docx_document <- function(x) {
  replace_docx_comment_macros(x)
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

process_chunk_docx <- function(options) {

  code <- options$code
  code <- replace_docx_comment_macros(code)
  code[nchar(code) > 0] <- paste0("**",code[nchar(code) > 0],"**")

  code <- paste(code, collapse = "\n")

    comment_label <- register_docx_comment(options$label)

    glue::glue("____\n<span class='underline'>**COMMENT {comment_label}**</span>\n\n \n**RC:**\n{code}</br>\n\n")

}
