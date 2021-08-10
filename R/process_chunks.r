process_chunk <- function(options){

  start <- paste0("\\reviewer{",options$label,"}{")

  paste(start,paste("\n",options$code, collapse = ""),"}", sep = "\n")
}
