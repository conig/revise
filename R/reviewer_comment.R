#' reviewer_comment
#'
#' A comment to style chunks for reviewer comments

reviewer_comment <- function(){

  context <- rstudioapi::getActiveDocumentContext()

    #assign("context", context, envir = globalenv())

    if(length(context$selection) == 0){
      return(rstudioapi::showDialog(title = ":'(", message = "The ReviewerComment addin will not work in the visual markdown editor. Please switch to the source editor."))
    }

  contents = context$contents

  if(!any(grepl("^---$", contents))){
    append_to_front <- rnr_header(detect_primary_file())
    append_to_rear <- '\\clearpage

# References

\\begingroup

<div id="refs"></div>

\\endgroup
'
  output_format = "pdf"

  }else{
    append_to_front = NULL
    append_to_rear = NULL
    output_format <- NULL
  }


  selection <- context$select[[1]]$range
  start <- selection$start[[1]]
  end <- selection$end[[1]]
  document_end <- length(context$contents)

  comment_n <- stringr::str_extract_all(contents, r"(\\reviewer\{c[0-9]*\})", simplify = TRUE)
  if(length(comment_n) == 0){
    comment_n <- 1
  }else{
    comment_n <- max(as.numeric(stringr::str_extract_all(comment_n, "\\d*", simplify = TRUE)), na.rm = TRUE)
    comment_n <- comment_n + 1
  }

  if (is.null(output_format)) {
    output_format <- rmarkdown::yaml_front_matter(context$path)$output
    output_format <- tools::file_ext(output_format)
  }

  if (output_format == "pdf") {
    start_content <- glue::glue("```{=tex}
\\reviewer{c!!<comment_n>!!}{

",
.open = "!!<",
.close = ">!!")

    end_content <- "
  }
```"
  }

  if(output_format == "txt"){
    start_content <- "```{reviewer}"
    end_content <- "
```"
  }

  before <- contents[1:start-1]

  middle <- contents[start:end]
  if(document_end != end){
    after <- contents[(end+1) : length(contents)]
  }else{
    after <- ""
  }

  new = c(append_to_front,before, start_content, middle, end_content, after, append_to_rear)

  rstudioapi::setDocumentContents(paste(new,collapse = "\n"), id = context$id)

}


rnr_header <- function(file){
  yaml <- tryCatch(rmarkdown::yaml_front_matter(file), error = function(e) return(NA))

title <- tryCatch(yaml["title"][[1]], error = function(e) return("paper title"))
author <- tryCatch(yaml["author"][[1]][[1]]$name, error = function(e) return("Author name"))
bibliography <- tryCatch(yaml["bibliography"][[1]], error = function(e) return("bibliography.json"))
csl <- tryCatch(yaml["csl"][[1]], error = function(e) return(""))

if(is.null(title)) title <- "paper title"
if(is.null(author)) author <- "Author name"
if(is.null(bibliography)) bibliography <- "bibliography.json"
if(is.null(csl)) csl <- ""
if(length(bibliography) > 1) bibliography <- glue::glue("[{paste(bibliography, collapse = ', ')}]")

header <- glue::glue(r'(---
title          : "!!<title>!!"
authors        : "!!<author>!! on behalf of co-authors"
journal        : "your journal"
manuscript     : "MANUSCRIPT-ID"
handling_editor: ""

class             : "draft"
bibliography      : !!<bibliography>!!
csl               : "!!<csl>!!"
output            : revise::letter
---

Dear Dr `r rmarkdown::metadata$handling_editor`,

Thank you for considering our manuscript for publication at _`r rmarkdown::metadata$journal`_. We appreciate the feedback that you, and the reviewers have provided. In the following itemised list we respond to each comment point-by-point.

```{r setup-chunk}
# knitr::knit_engines$set(reviewer = revise:::process_chunk)
# manuscript <- revise::read_manuscript("!!<file>!!", PDF = TRUE)
# get_revision <- function(id, ...) revise::get_revision(manuscript, id, ...)
# load("manuscript_workspace.rData")
```

)', .open = "!!<", .close = ">!!")

header <- gsub("csl\\s*:\\s*\"NA\"", "" , header)
header <- gsub("bibliography\\s*:\\s*NA", "" , header)
header

}

detect_primary_file <- function(){

  files <- list.files(pattern = ".rmd$", ignore.case = TRUE)
  contents <- suppressWarnings(lapply(files, readLines))

  out <- data.frame(files = files,
             has_papaja = sapply(contents, function(x)
               any(grepl("papaja::apa", x, fixed = TRUE))),
             filesize = sapply(files, file.size))
  if(nrow(out) == 0) return(NA)

   out[order(-out$has_papaja,-out$filesize),"files"][[1]]

}


