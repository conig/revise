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
#' @param revise_errors logical. If FALSE, failure to match manuscript sections will result in warnings rather than errors.
#' @param envir The environment in which to find the manuscript.
#' @export

get_revision <- function(id,
                                 manuscript = NULL,
                                 quote = TRUE,
                                 evaluate = TRUE,
                                 split_string = FALSE,
                                 search_length = 300,
                                 include_pgnum = TRUE,
                                 revise_errors = getOption("revise_errors"),
                         envir = parent.frame(1L)) {
  if(is.null(manuscript)){
    if(exists(".revise_manuscripts", where = envir)){
      manuscript <- get(".revise_manuscripts", envir = envir)
    } else {
      warning("Argument 'manuscript' is NULL, and no manuscript exists in the environment.")
      return(NULL)
    }
  }
  cl <- match.call()
  cl[["manuscript"]] <- manuscript
  if(inherits(manuscript, "revise_corpus")){
    cl[[1L]] <- str2lang("revise:::get_revision.revise_corpus")
  } else {
    cl[[1L]] <- str2lang("revise:::get_revision.default")
  }
  eval.parent(cl)
}

get_revision.revise_corpus <- function(id,
                         manuscript = NULL,
                         quote = TRUE,
                         evaluate = TRUE,
                         split_string = FALSE,
                         search_length = 300,
                         include_pgnum = TRUE,
                         revise_errors = getOption("revise_errors"),
                         envir) {
  all_sect_names <- unlist(lapply(manuscript, function(x){ names(x[["sections"]]) }))
  if(!isTRUE(length(all_sect_names) > 0)) return(NULL)
  tryCatch(check_dup_sections(all_sect_names, revise_errors = revise_errors), warning = function(w){
    if(!isTRUE(sapply(manuscript, function(x){ any(duplicated(names(x[["sections"]]))) }))){
      w <- paste0(w, "You have loaded multiple manuscripts, and these manuscripts contain identical section names.")
    }
    warning(gsub("simpleWarning: ", "", w, fixed = TRUE), call. = FALSE)
  })
  # Here you could check for similar names; if so, turn that into a function
  if(!id %in% all_sect_names) return(NULL)
  # If it does exist, select the first instance
  cl <- match.call()
  cl[["manuscript"]] <- manuscript[[which(sapply(manuscript, function(x){ any(names(x[["sections"]]) == id)}))[1]]]
  cl[[1L]] <- str2lang("revise:::get_revision.default")
  eval.parent(cl)
}

get_revision.default <- function(id,
                        manuscript = NULL,
                        quote = TRUE,
                        evaluate = TRUE,
                        split_string = FALSE,
                        search_length = 300,
                        include_pgnum = TRUE,
                        revise_errors = getOption("revise_errors"),
                        envir) {
  if(is.null(manuscript)){
    if(".revise_manuscripts" %in% objects(envir = parent.frame(1), all.names = TRUE)){
      manuscript <- get(".revise_manuscripts", envir = parent.frame(1))
    } else {
      warning("Argument 'manuscript' is NULL, and no manuscript exists in the environment.")
      return(NULL)
    }
  }
  if(is.null(manuscript[["sections"]])) return(NULL)
  check_dup_sections(names(manuscript$sections), revise_errors = revise_errors)
  string <- manuscript$sections[[id]]

  if(is.null(string)){
    similar_id <- agrep(id, names(manuscript$sections), value = TRUE)
    similar_id <- paste(similar_id, collapse = " | ")
    message <- paste0("Couldn't find a section in the manuscript tagged as '", id, "'.")
    if(nchar(similar_id) > 0){
      message <- paste0(message, " Did you mean: ", similar_id,"?")
    }

    if(!revise_errors){
      warning(message)
      return(paste0("**", message, "**"))
    }else{
      stop(message)
    }

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
