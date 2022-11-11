checksum_exists <- function(corpus, checksum){
  UseMethod("checksum_exists", corpus)
}

checksum_exists.revise_corpus <- function(corpus, checksum){
  all_csums <- sapply(corpus, `[[`, "checksum")
  return(isTRUE(checksum %in% all_csums))
}

checksum_exists.revise_manuscript <- function(corpus, checksum){
  return(isTRUE(checksum == corpus[["checksum"]]))
}

#' read_md
#'
#' Read in a markdown document for further analysis
#' @param path to markdown manuscript

read_md <- function(path){
  unname(unlist(utils::read.delim(path, sep = "\n", quote = "", header = FALSE)))
}

#' extract_refs
#'
#' Extract table and figure references
#' @param path to markdown manuscript

extract_refs <- function(path){
  lines <- read_md(path)
  lines <- paste(lines, collapse = "\n")
  references <- unlist(stringr::str_extract_all(lines, "\\\\\\@ref.+?\\)"))
  figures <- data.frame(text = unique(grep("fig:",references, value = TRUE)))
  tables <- data.frame(text = unique(grep("tab:",references, value = TRUE)))
  figures$ref <- seq_along(figures[,1])
  tables$ref <- seq_along(tables[,1])
  list(figures = figures, tables = tables)

}


#' @export
print.revise_manuscript = function(x, ...){

  n_sections <- glue::glue("\033[34m- {length(x$sec)} sections\033[39m")
  if(is.null(x$PDF)){
    PDF_info <- "\033[31mNo PDF attached\033[39m"
  }else{
    PDF_info <- glue::glue("\033[32m- {nrow(x$PDF)} pages\033[39m")
  }

  cat("<Manuscript>")
  cat("\n")
  cat(n_sections)
  cat("\n")
  cat(PDF_info)



}

#' evaluate_inline
#'
#' Evaluates embedded rchunks within a string of text
#' @param string a section of text with includes inline elements
#' @examples
#' revise:::evaluate_inline("1+1 = `r 1+1`")

evaluate_inline <- function(string){
  glue::glue(string, .open = "`r", .close = "`")
}

#' process_pdf
#'
#' process pdf file
#' @param path path to pdf

process_pdf <- function(path){
  ext <- tolower(tools::file_ext(path))
  if(!ext %in% c("pdf")){
    stop("Extracting page numbers only works for pdf documents")
  }
  doc <- get_pdf_text(path)
  running_head = gsub("\n.*","",doc$text[2])
  running_head = trimws(gsub("[0-9]","",running_head))
  running_head = gsub("\\s{1,}"," ", running_head)
  is_head <- unlist(lapply(doc$text, function(x) grepl(running_head,x)))
  prop_head <- prop.table(table(is_head))["TRUE"]

  doc$text <- tolower(doc$text)
  if(!is.null(running_head)){
    doc$text <- stringr::str_remove_all(doc$text, tolower(glue::glue("^{running_head}")))
  }

  doc <-  dplyr::summarise(
    dplyr::group_by(doc, page_id),
    text = paste(text, collapse = " "),
    .groups = "drop"
  )

  doc$text <- trimws(stringr::str_remove_all(doc$text, "[[:digit:]]")) # remove all numbers
  doc$text <- stringr::str_remove_all(doc$text, "\\[.{0,50}\\]") # remove square brackets
  doc$text <- stringr::str_remove_all(doc$text, "\\(.{0,50}\\)") # remove parentheses
  doc$text <- trimws(stringr::str_remove_all(doc$text, "[[:punct:]]")) # remove all punctuation
  if(prop_head > .7){
    attr(doc, "running_head") <- running_head
  }else{
    attr(doc, "running_head") <- NULL
  }
  doc
}

#' find_pages
#'
#' Searches through a manuscript for a page reference
#' @param manuscript the manuscript object to search
#' @param string the string to find
#' @export

find_pages <- function(manuscript, string){
  if(is.null(manuscript$PDF)) stop("No PDF attached to the manuscript object: not possible to identify page numbers.")
  get_pdf_pagenumber(string, pdf_text = manuscript$PDF)
}

#' clean_string
#'
#' Cleaning steps for identifying strings
#' @param string string to clean

clean_string <- function(string){
  string <- gsub("\\[.{0,50}\\]","",string) # remove square brackets
  string <- gsub("\\*|\\#", "", string) # remove rmarkdown formatting
  string <- gsub("[0-9]","", string) # remove numbers
  string <- gsub("\\(.{0,50}\\)", "", tolower(string)) # remove parentheses
  string <- gsub("[[:punct:]]", "", tolower(string)) # remove parentheses
  string <- gsub("\\s{2,}", " ", string) # remove additional spaces
  string <- gsub("\\n", " ", string)
  string
}

#' get_page_number
#'
#' Finds page number from pdf based on text matching
#' @param string text to match
#' @param pdf_text text to search
#' @param max.distance argument passed to agrep

get_pdf_pagenumber = function(string, pdf_text, max.distance = .15){

  string <- clean_string(string)

  doc <- clean_string(pdf_text$text)

  pnum <- agrep(string, doc, ignore.case = TRUE, max.distance = max.distance)

  if(length(pnum) > 0) return(paste(pnum, collapse = ", "))

  l <- lapply(seq_len(length(doc)), function(p){ # look at combinations of pages if no match

    pages = sapply(c(doc[p], doc[p + 1]), function(x){
      tolower(unlist(x))
    })

    pages <- lapply(seq_along(pages), function(i){
      page <- pages[[i]]
      page <- gsub("\\\n", " ", page)
      page <- gsub(r"(\s{2,})", " ", page)
      page <- trimws(page)
      page
    })

    data.frame(
      page_id = glue::glue("{p}-{p + 1}"),
      text = trimws(paste(pages, collapse = " "))
    )
  }) # ---

  doc <- do.call(rbind, l)
  pnum <- agrep(string, doc$text, ignore.case = TRUE, max.distance = max.distance)
  doc$page_id[pnum]

}

#' header_to_bold
#'
#' Converts headers to bold
#' @param string a string
#' @return string

header_to_bold = function(string){

  while(grepl("(?<!\\{)#{1,}.{0,100}\\\n",string, perl = TRUE)){
    target <- stringr::str_extract(string, "(?<!\\{)#{1,}.+?(\\n){1,}")
    n_hash <- sum(strsplit(target, split = "")[[1]]=="#")
    replacement <- gsub("(?<!\\{)#{1,}\\s?","**", target, perl = TRUE)
    if(n_hash < 3){
      replacement <- gsub("\\n{1,}","**\\\n\\\n", replacement)
    } else{
      replacement <- gsub("\\n{1,}",".** ", replacement)
    }
    string <- gsub(target, replacement, string)
  }

  string

}


utils::globalVariables(c("text", "page_id", ".revise_manuscripts"))

check_dup_sections <- function(sections){
  if(any(duplicated(names(sections)))){
    warning("The following sections have duplicate names. When referencing sections by name, the section with that name will be selected. Please use unique section names:\n", paste0("  '", names(sections)[duplicated(names(sections))], "'\n"), call. = FALSE)
  }
}

.onLoad <- function(libname, pkgname){

  if(is.null(getOption("revise_errors"))){
    options(revise_errors = TRUE)
  }

}
