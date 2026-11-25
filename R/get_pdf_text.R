#' @importFrom pdftools pdf_text
process_pdf <- function(path) {
  if (tolower(tools::file_ext(path)) != "pdf") {
    stop("Extracting page numbers only works for pdf documents")
  }
  pages <- pdftools::pdf_text(path)
  lines <- lapply(strsplit(pages, "\r\n|\r|\n"), function(x) {
    x <- trimws(x)
    x[nzchar(x)]
  })

  # Only remove a first line when it recurs across pages with body text.
  # Compare it literally: punctuation in a printed title isn't a regex.
  first <- vapply(lines, function(x) if (length(x) > 1L) x[1] else "", character(1))
  headers <- trimws(gsub("[[:space:]]+", " ", gsub("[0-9]", "", first)))
  counts <- table(headers[nzchar(headers)])
  head <- names(counts)[counts >= 2L & counts > .7 * length(pages)]
  if (length(head)) {
    matched <- headers == head
    lines[matched] <- lapply(lines[matched], `[`, -1L)
  }

  text <- vapply(lines, paste, character(1), collapse = " ")
  doc <- data.frame(page_id = seq_along(pages), text = clean_string(text))
  if (length(head)) attr(doc, "running_head") <- head
  doc
}
