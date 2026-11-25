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

  # Keep numbers in body text for matching, but not a printed folio at the
  # edge of a page: it would break a quote that continues on the next page.
  lines <- Map(function(x, page) {
    if (length(x) < 2L) return(x)
    edge <- c(1L, length(x))
    x[setdiff(seq_along(x), edge[x[edge] == as.character(page)])]
  }, lines, seq_along(lines))

  # Only remove a first line when it recurs across pages with body text.
  # Compare it literally: punctuation in a printed title isn't a regex.
  headers <- vapply(seq_along(lines), function(page) {
    x <- lines[[page]]
    if (length(x) < 2L) return("")
    title <- gsub("[[:space:]]+", " ", x[1])
    sub(sprintf("^%d | %d$", page, page), "", title)
  }, character(1))
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
