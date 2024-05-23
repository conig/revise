#' read_docx
#' 
#' Read a docx file and extract comments
#' @param address path to docx
#' @importFrom xml2 xml_find_all xml_text xml_attr xml_parent xml_name xml_find_first

read_docx <- function(address) {
  docx <- officer::read_docx(address)
  comments <- get_docx_comments(docx)
  out <- 
    data.frame(section = unlist(comments$text),
                section_text = comments$commented_text)
  out <- out[grepl("Revise\\:\\:",out$section),]
  out$section <- gsub("Revise\\:\\:","",out$section)
  out$section <- trimws(out$section)
   
  lst <- as.list(trimws(out$section_text))
  names(lst) <- out$section
  lst
}

get_docx_comments <- function(x) {
  stopifnot(inherits(x, "rdocx"))

  # Retrieve comment IDs
  comment_ids <- xml_attr(
    xml_find_all(
      x$doc_obj$get(), "/w:document/w:body//*[self::w:commentRangeStart]"
    ), "id"
  )

  # Retrieve text runs within comment ranges
  comment_text_runs <- lapply(comment_ids, function(id) {
    xml_find_all(
      x$doc_obj$get(),
      paste0(
        "/w:document/w:body//*[self::w:r[w:t and ",
        "preceding::w:commentRangeStart[@w:id='", id, "']",
        " and ",
        "following::w:commentRangeEnd[@w:id='", id, "']]]"
      )
    )
  })

  # Retrieve parent paragraphs of text runs
  comment_paragraphs <- lapply(comment_text_runs, function(runs) {
    unique(xml_parent(runs))
  })

  # Extract and concatenate text within each paragraph
text_per_paragraph <- lapply(comment_paragraphs, function(paragraphs) {
  out <- sapply(paragraphs, function(para) {
    # Filter out standalone <ins> elements
    if (xml_name(para) == "p") {
       comment_start <- xml_find_first(para, ".//w:commentRangeStart")
      comment_end <- xml_find_first(para, ".//w:commentRangeEnd")
      nodes <- xml_find_all(comment_start, ".//following-sibling::*[preceding-sibling::w:commentRangeStart and following-sibling::w:commentRangeEnd]")
      paste(xml_text(xml_find_all(nodes, ".//w:t")), collapse = "")
    } else {
      NULL  # Ignore standalone <ins> elements
    }
  })
  unlist(out[!sapply(out, is.null)])
})

  # Collapse concatenated text per paragraph with newline character
  collapsed_text_per_paragraph <- lapply(text_per_paragraph, function(paragraph_texts) {
    paste(paragraph_texts, collapse = "\n\n")
  })

  # Create data frame for comment IDs
  data <- data.frame(
    comment_id = comment_ids
  )

  # Add parent paragraph IDs
   data$para_id <- lapply(
    comment_paragraphs,
    function(paragraphs) {
      sapply(paragraphs, function(para) {
        xml_attr(para, "paraId")
      })
    }
  )

   # Add concatenated text per paragraph
  data$commented_text <- unlist(collapsed_text_per_paragraph)

  # Retrieve all comments
  comments <- xml_find_all(x$comments$get(), "//w:comments/w:comment")

  # Create output data frame with comment details
  out <- data.frame(
    stringsAsFactors = FALSE,
    comment_id = xml_attr(comments, "id"),
    author = xml_attr(comments, "author"),
    initials = xml_attr(comments, "initials"),
    date = xml_attr(comments, "date")
  )

  # Add comment text
  out$text <- lapply(
    comments,
    function(comment) {
      paragraphs <- xml_find_all(comment, "w:p")
      sapply(paragraphs, function(para) {
        paste(xml_text(xml_find_all(para, ".//w:t")), collapse = "")
      })
    }
  )

  # Merge data frames to combine comment details with paragraph information
  data <- merge(out, data, by = "comment_id", all.x = TRUE)
  data[order(as.integer(data$comment_id)), ]
}

