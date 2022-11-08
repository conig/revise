#' get_revision
#'
#' Extract and format revision
#' @param id the id from a html tag
#' @param manuscript Optional; a manuscript object in which to search
#' for `id`. Defaults to `NULL`, in which case the manuscript object
#' is found in the environment.
#' @param quote is the output chunk quoted?
#' @param evaluate logical. Should inline rchunks be executed?
#' @param split_string should only the start and end of the string be searched for?
#' @param search_length numeric. Searches for the first n and n characters in a string. Shorten if difficult to find passages split by floats.
#' @param include_pgnum logical. include PDF page number?
#' @export

get_revision = function(id,
                        manuscript = NULL,
                        quote = TRUE,
                        evaluate = TRUE,
                        split_string = FALSE,
                        search_length = 300,
                        include_pgnum = TRUE) {
  if(is.null(manuscript)){
    if(".revise_manuscripts" %in% objects(envir = parent.frame(1), all.names = TRUE)){
      manuscript <- .revise_manuscripts
    } else {
      warning("Argument 'manuscript' is NULL, and no manuscript exists in the environment.")
      return(NULL)
    }
  }
  if(is.null(manuscript[["sections"]])) return(NULL)
  check_dup_sections(manuscript$sections)
  string <- manuscript$sections[[id]]
  if(is.null(string)){
    similar_id <- agrep(id, names(manuscript$sections), value = TRUE)
    similar_id <- paste(similar_id, collapse = " | ")
    message = paste0("Couldn't find a section in the manuscript tagged as '", id, "'.")
    if(nchar(similar_id) > 0){
      message <- paste0(message, " Did you mean: ", similar_id,"?")
    }
    warning(message, call. = FALSE)
    return(paste0("**", message, "**"))
  }

  if (evaluate) {
    string <- evaluate_inline(string)
  }

  if (!is.null(manuscript$PDF) & include_pgnum) {

    if((nchar(string) > search_length) | split_string){

      start_string <- substring(string, 1, search_length)
      end_string <- substring(string, nchar(string) - search_length, nchar(string))

      pnum.start <-
        get_pdf_pagenumber(start_string, pdf_text = manuscript$PDF)
      pnum.end <-
        get_pdf_pagenumber(end_string, pdf_text = manuscript$PDF)

      pnum.start <- gsub("\\-.*","",pnum.start)
      pnum.end <- gsub(".*\\-","",pnum.end)

      pnum <- paste(unique(c(pnum.start, pnum.end)),collapse = "-")

    }else{
      pnum = get_pdf_pagenumber(string, pdf_text = manuscript$PDF)
    }

    if (length(pnum) == 0){
      warning("Couldn't match the extracted text to the target PDF: ", id,". Have you knit the manuscript since making recent changes?")
      pnum <- "?"
    }

    string = paste0(string,
                    "\n\n\\begin{flushright}Pg. ",
                    pnum,
                    "\\end{flushright}")
  }

  string <- header_to_bold(string)

  for (i in seq_along(manuscript$refs$tables[, 1])) {
    # replace \\@ref(tab:)
    string <-
      gsub(manuscript$refs$tables$text[i],
           manuscript$refs$tables$ref[i],
           string,
           fixed = TRUE)
  }

  for (i in seq_along(manuscript$refs$figures[, 1])) {
    # replace \\@ref(fig:)
    string <-
      gsub(manuscript$refs$figures$text[i],
           manuscript$refs$figures$ref[i],
           string,
           fixed = TRUE)
  }

  if (quote) {
    string <- gsub("\\n", "\\\n>", string)
    string <- paste0(">", string)
  }


  string
}
