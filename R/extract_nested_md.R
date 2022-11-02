
#' extract_md_sections2
#'
#' Extract text when there are nested parentheses
#' @param string string to extract md sections from
#' @param is_span logical, whether to extract sections in square brackets (default) or spans

extract_md_sections2 <- function(string,
                                 is_span = FALSE
){

  if(is_span){
    regex_header = "<span.+?id.+?=.+?\\b(.+?)\\b.+?>"
    regex_end = "(?<=</span>)"
    regex_issection = "</span>"
    open = "<span"
    close = "</span>"
  } else {
    regex_header = "(?<=\\{#)[^{}]+(?=\\})"
    regex_end = "\\{#.+?\\}"
    regex_issection = "\\]\\{$"
    open = "["
    close = "]"
  }
  string <- paste0(string, collapse = "\r\n")
  sectionheaders <- regmatches(string, gregexpr(regex_header, string, perl=TRUE))[[1]]

  sectionends <- gregexpr(pattern = regex_end, text = string, perl = TRUE)[[1]]
  sections <- lapply(sectionends, function(i){
    substring(string, first = 1, last = i)
  })

  sections <- sections[grepl(pattern = regex_issection, x = sections)]
  sections_clean <- lapply(sections, function(x){
    openbrackets <- list(position = gregexpr(pattern = open, text = x, fixed = TRUE)[[1]])
    openbrackets$type <- rep("open", times = length(openbrackets$position))
    closingbrackets <- list(position = gregexpr(pattern = close, text = x, fixed = TRUE)[[1]])
    closingbrackets$type <- rep("closed", times = length(closingbrackets$position))
    allbrackets <- openbrackets
    allbrackets$position <- c(allbrackets$position, closingbrackets$position)
    ordr <- order(allbrackets$position, decreasing = FALSE)
    allbrackets$type <- c(openbrackets$type, closingbrackets$type)[ordr]
    allbrackets$position <- allbrackets$position[ordr]
    substring(text = x, first = (findposition(x = allbrackets)+1), last = (nchar(x)-2))
  })

  if(is_span){
    sectionheaders <- gsub("^.+?[\"'](.+?)[\"'].*$", "\\1", sectionheaders)
    sections_clean <- lapply(sections_clean, gsub, pattern = "^.+?>(.+)<.+?$", replacement = "\\1")
  }

  data.frame(tag = sectionheaders, section = unlist(sections_clean))
}




findposition <- function(x, level = 0){
  len <- length(x$position)
  x$position <- x$position[-len]
  x$type <- x$type[-len]
  if(tail(x$type, 1) == "open"){
    if(level == 0){
      return(tail(x$position, 1))
    } else {
      findposition(x = x, level = level -1)
    }
  } else {
    findposition(x = x, level = level+1)
  }
}
