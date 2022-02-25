#' extract_sections
#'
#' Extracts markdown sections
#' @param path path to markdown file

extract_md_sections <- function(path){
  if(!grepl("md$",basename(path), ignore.case = TRUE)) stop("can only be used on rmd, or md files")
  lines <- readLines(path)
  lines <- paste(lines, collapse = "\n\n")
  #comments <- stringr::str_extract_all(lines, "\\[(?!\\s*\\@).+?(?=\\{\\#)\\{\\#[^\\[]*\\}")
  comments <- unlist(stringr::str_extract_all(lines, "\\[(?!\\s*\\@).+?\\]\\{#.+?\\}"))
  sections <- lapply(comments, function(x){
    tag <- stringr::str_extract(x,"\\{\\#.*\\}")
    tag <- gsub("[\\{\\}\\#]","",tag)
    tag <- trimws(tag)
    section <- gsub("\\{\\#.*\\}", "", x)
    section <- substring(section, 2,nchar(section)-1)
    data.frame(tag = tag, section = section)
  })
  do.call(rbind, sections)
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

#' extract_html_sections
#'
#' A function to extract text within spans
#' @param path character path to file

extract_html_sections <- function(path){
  lines <- read_md(path)
  df <- data.frame(lines)
  df$is_chunk <- FALSE
  is_chunk <- FALSE
  for(i in seq_along(df$is_chunk)){
    if(grepl("\\`\\`\\`.*\\{",df$lines[i])) is_chunk <- TRUE
    df$is_chunk[i] <- is_chunk
    if(grepl("\\`\\`\\`$",df$lines[i])) is_chunk <- FALSE
  }
  document <- paste(df$lines[!df$is_chunk], collapse = "\n\n")
  tmp <- tryCatch(xml2::read_html(document), error = function(e) tmp <- NULL)
  if(is.null(tmp)) return(tmp)
  tmp <- rvest::html_nodes(tmp, xpath = glue::glue('//*[@id]') )

  section<- rvest::html_text(tmp)
  section <- gsub("\\\\n","\\\n", section)
  tag <- rvest::html_attr(tmp, "id")
  data.frame(tag, section)
}

#' extract_sections
#'
#' Extracts markdown sections
#' @param path character path to file

extract_md_sections <- function(path){
  if(!grepl("md$",basename(path), ignore.case = TRUE)) stop("can only be used on rmd, or md files")
  lines <- readLines(path)
  lines <- paste(lines, collapse = "\n\n")
  #comments <- stringr::str_extract_all(lines, "\\[(?!\\s*\\@).+?(?=\\{\\#)\\{\\#[^\\[]*\\}")
  comments <- unlist(stringr::str_extract_all(lines, "\\[(?!\\s*\\@).*?\\]\\{#.+?\\}"))
  sections <- lapply(comments, function(x){
    tag <- stringr::str_extract(x,"\\{\\#.*\\}")
    tag <- gsub("[\\{\\}\\#]","",tag)
    tag <- trimws(tag)
    section <- gsub("\\{\\#.*\\}", "", x)
    section <- substring(section, 2,nchar(section)-1)
    data.frame(tag = tag, section = section)
  })
  do.call(rbind, sections)
}

#' read_manuscript
#'
#' Reads in rmarkdown manuscript and an associated PDF as possible
#' @param address path to a rmarkdown file
#' @param id if provided spans will be searched for text which will be returned
#' @param PDF if TRUE, or path provided, a PDF will be loaded for page matching.
#' @export

read_manuscript <- function(address, id = NULL, PDF = FALSE){
  if(!is.null(id)) return(read_spans(address, id))
  rmd <- paste(read_md(address),collapse = "\n")
  sections <- rbind(extract_md_sections2(rmd), extract_html_sections(address))
  section_names <- sections$tag
  sections <- as.list(sections$section)
  names(sections) <- section_names
  if(!is.null(PDF)) {
    if (PDF == TRUE) {
      floats_in_text <-
      tryCatch({
        rmarkdown::yaml_front_matter(address)$floatsintext
      }, error = function(e) return(FALSE))

      if(floats_in_text){
        warning("floatsintext is set to TRUE in target rmarkdown. This may disrupt page number matching where target strings cross pages.")
      }

      PDF <- gsub(tools::file_ext(address), "pdf", address)
    }
    if (class(PDF) == "character") {
      PDF <- process_pdf(PDF)
    } else{
      PDF <- NULL
    }
  } else{
    PDF <- NULL
  }
  refs <- extract_refs(address)
  manuscript <- list(sections = sections,PDF = PDF, refs = refs, rmd = rmd)
  class(manuscript) <- "manuscript"
  manuscript

}

#' print.manuscript
#'
#' Print method for manuscripts
#' @param x an object of class "manuscript"
#' @param ... other arguments
#' @export

print.manuscript = function(x, ...){

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


#' read_spans
#'
#' @param address string url or file location of knited html
#' @param id the id from a html tag


read_spans <- function(address, id){
  # Test for missing or non-character id
  if(missing(id)|!is.character(id)) stop("Please specify an id string")
  # Read in data and parse
  tmp = rvest::html_nodes(xml2::read_html(address),
                          xpath = glue::glue('//*[@id="{id}"]'))
  # Test if id is unique
  if(length(tmp)>1) warning("ID is not unique")
  # Extract
  tmp <- stringr::str_trim(rvest::html_text(tmp))
  return(tmp)
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

  doc <- textreadr::read_pdf(path)
  running_head = gsub("\n.*","",doc$text[2])
  running_head = trimws(gsub("[0-9]","",running_head))
  running_head = gsub("\\s{1,}"," ", running_head)
  is_head <- unlist(lapply(doc$text, function(x) grepl(running_head,x)))
  prop_head <- prop.table(table(is_head))

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


#' get_revision
#'
#' Extract and format revision
#' @param manuscript the manuscript object from which to extract revisions
#' @param id the id from a html tag
#' @param quote is the output chunk quoted?
#' @param evaluate bool. Should inline rchunks be executed?
#' @export

get_revision = function(manuscript,
                        id,
                        quote = TRUE,
                        evaluate = TRUE) {
  string <- manuscript$sections[id][[1]]
  if(is.null(string)){
    similar_id <- agrep(id, names(manuscript$sections), value = TRUE)
    similar_id <- paste(similar_id, collapse = " | ")
    message = paste0("Couldn't find a section in the manuscript tagged as '", id, "'.")
    if(nchar(similar_id) > 0){
      message <- paste0(message, " Did you mean: ", similar_id,"?")
    }
    stop(message, call. = FALSE)
  }

  if (evaluate) {
    string <- evaluate_inline(string)
  }

  if (!is.null(manuscript$PDF)) {

    if(nchar(string) > 2000){

      start_string <- substring(string, 1, 1000)
      end_string <- substring(string, nchar(string) - 1000, nchar(string))

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

    if (length(pnum) == 0)
      stop("Couldn't match the extracted text to the target PDF: ", id,". Have you knit the manuscript since making recent changes?")
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

utils::globalVariables(c("text", "page_id"))
