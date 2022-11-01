
#' extract_md_sections2
#'
#' Extract text when there are nested parentheses
#' @param string string to extract md sections from

extract_md_sections2 <- function(string){
  string <- paste0(string, collapse = "\r\n")
  sectionheaders <- regmatches(string, gregexpr("(?<=\\{#)[^{}]+(?=\\})", string, perl=TRUE))[[1]]

  sectionends <- gregexpr(pattern = "\\{#.+?\\}", text = string)[[1]]
  sections <- lapply(sectionends, function(i){
    substring(string, first = 1, last = i)
  })
  # sections <- strsplit(x = string, split = "\\{#.+?\\}")[[1]]
  sections <- sections[grepl(pattern = "\\]\\{$", x = sections)]
  sections_clean <- lapply(sections, function(x){
    openbrackets <- list(position = gregexpr(pattern = "[", text = x, fixed = TRUE)[[1]])
    openbrackets$type <- rep("open", times = length(openbrackets$position))
    closingbrackets <- list(position = gregexpr(pattern = "]", text = x, fixed = TRUE)[[1]])
    closingbrackets$type <- rep("closed", times = length(closingbrackets$position))
    allbrackets <- openbrackets
    allbrackets$position <- c(allbrackets$position, closingbrackets$position)
    ordr <- order(allbrackets$position, decreasing = FALSE)
    allbrackets$type <- c(openbrackets$type, closingbrackets$type)[ordr]
    allbrackets$position <- allbrackets$position[ordr]
    substring(text = x, first = (findposition(x = allbrackets)+1), last = (nchar(x)-2))
  })
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
