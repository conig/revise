insert_at <- function(string, position, insert){
  string <- unlist(strsplit(string, ""))


  if (position == 1) {
    new = c(insert, string)
  }

    if (position == length(string)) {
      new <- c(string, insert)
    }

    if (position > 1 & position < length(string)) {
      new = c(string[1:position],
              insert,
              string[(position+1):length(string)])
    }


  paste(new,collapse = "")

}

#' reviewer_comment
#'
#' A comment to style chunks for reviewer comments

anchor_tag <- function(){

  context <- rstudioapi::getActiveDocumentContext()
  #assign("context", context, envir = globalenv())
  contents <- context$contents
  start <- context$selection[[1]]$range$start
  end <- context$selection[[1]]$range$end

  end_line <- contents[end[[1]]]
  end_position <- end[[2]] - 1
  if(end_position > (nchar(end_line))) end_position <- nchar(end_line)

  contents[end[[1]]] <-  insert_at(end_line, end_position,"]{#}")

  start_position <-  start[[2]] - 1
  if(start_position <= 0) start_position <- 1

  contents[start[[1]]] <- insert_at(contents[start[[1]]],start_position,"[")

  rstudioapi::setDocumentContents(paste(contents,collapse = "\n"), id = context$id)

  col <- stringr::str_locate(contents[end[[1]]], "\\{\\#")[2] + 1
  pos <- rstudioapi::document_position(end[[1]],col)
  rstudioapi::setCursorPosition(pos, id = context$id)
}
