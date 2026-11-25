#' read_docx
#'
#' Read a docx file and extract comments
#' @param path path to docx
#' @importFrom xml2 xml_find_all xml_text xml_attr xml_find_first

read_docx <- function(path) {
  comments <- get_docx_comments(officer::read_docx(path))
  comments <- comments[grepl("Revise::", names(comments), fixed = TRUE)]
  names(comments) <- trimws(gsub("Revise::", "", names(comments), fixed = TRUE))
  lapply(comments, trimws)
}

get_docx_comments <- function(x, markdown = TRUE) {
  body <- xml_find_first(x$doc_obj$get(), "/w:document/w:body")
  comments <- xml_find_all(x$comments$get(), "//w:comments/w:comment")
  ids <- xml_attr(comments, "id")
  text <- vapply(comments, function(comment) {
    paste(xml_text(xml_find_all(comment, ".//w:t")), collapse = "")
  }, character(1))

  excerpts <- lapply(ids, function(id) {
    runs <- xml_find_all(body, paste0(
      ".//w:r[w:t and not(ancestor::w:del)",
      " and preceding::w:commentRangeStart[@w:id='", id, "']",
      " and following::w:commentRangeEnd[@w:id='", id, "']]"
    ))
    # A tracked insertion can sit between a run and its paragraph.
    paragraphs <- xml_find_first(runs, "ancestor::w:p[1]")
    paths <- xml2::xml_path(paragraphs)
    groups <- split(seq_along(runs), factor(paths, levels = unique(paths)))
    parts <- vapply(groups, function(i) {
      content <- paste(xml_text(xml_find_all(runs[i], "w:t")), collapse = "")
      if (markdown && contains_bullets(paragraphs[[i[1]]])) paste("*", content) else content
    }, character(1))
    paste(parts, collapse = if (markdown) "\n\n" else " ")
  })
  names(excerpts) <- text
  if (markdown) excerpts[order(as.integer(ids))] else excerpts
}

contains_bullets <- function(x) {
  grepl(r"(ListParagraph\"/>)", as.character(x))
}
