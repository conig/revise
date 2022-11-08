#' @importFrom pdftools pdf_text
get_pdf_text <- function (file, ...){
  text <- pdftools::pdf_text(file, ...)
  Encoding(text) <- "UTF-8"
  text <- strsplit(text, split = "\\r\\n")

  text <- lapply(text, function(x) x[!grepl("^\\s*$", x)])
  out <- data.frame(page_id = rep(seq_along(text), sapply(text,
                                                          length)), element_id = unlist(sapply(text, function(x) seq_len(length(x)))),
                    text = unlist(text), stringsAsFactors = FALSE)
  out[["text"]] <- trimws(out[["text"]])
  out
}
