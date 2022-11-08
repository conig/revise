#' abstract
#'
#' Create a structured abstract including cached results
#' @param ... headed abstract sections
#' @param cache_name character indicating the name the cached r chunk
#' @param sep how should sections be pasted together?
#' @export

abstract <- function(..., cache_name = NULL, sep = "\n\n") {
  elip <- list(...)

  headers <- paste("**",names(elip),"**",sep)
  text <- paste(headers, elip, collapse = sep)

curlys <- unlist(stringr::str_extract_all(text, "(?<=\\{).+?(?=\\})"))
cache_objects <- lapply(curlys, function(x) knitr::load_cache(label = cache_name, object = x))
curlys <- paste0("\\{",curlys,"\\}")

for(i in seq_along(curlys)){
 text <- gsub(curlys[i],cache_objects[i],text)

}
text

}
