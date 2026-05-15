#' read_docx
#'
#' Read a docx file and extract comments
#' @param path path to docx
#' @importFrom xml2 xml_find_all xml_text xml_attr xml_parent xml_name xml_find_first xml_remove

read_docx <- function(path) {
  docx <- officer::read_docx(path)
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
        "following::w:commentRangeEnd[@w:id='", id, "'] and ",
        "not(ancestor::w:del)]]"
      )
    )
  })

  # Retrieve the containing paragraphs for each run. Runs can sit inside
  # tracked-change containers such as <w:ins>, so we need the nearest
  # paragraph ancestor rather than the immediate parent node. Deleted runs
  # are filtered out in the XPath above.
  comment_paragraphs <- lapply(comment_text_runs, function(runs) {
    if (length(runs) == 0) {
      return(list())
    }

    paragraph_keys <- vapply(seq_along(runs), function(i) {
      para <- xml_find_first(runs[[i]], "ancestor::w:p[1]")
      para_id <- xml_attr(para, "paraId")
      if (is.na(para_id) || !nzchar(para_id)) {
        xml2::xml_path(para)
      } else {
        para_id
      }
    }, character(1))

    split(seq_along(runs), factor(paragraph_keys, levels = unique(paragraph_keys)))
  })

  # Extract and concatenate text within each paragraph from the already
  # matched runs. This preserves comments that start inside <w:ins> and
  # continue later in the same paragraph.
  text_per_paragraph <- Map(function(paragraph_indexes, runs) {
    vapply(paragraph_indexes, function(indexes) {
      paragraph_runs <- runs[unlist(indexes)]
      para <- xml_find_first(paragraph_runs[[1]], "ancestor::w:p[1]")

      content <- paste(
        vapply(paragraph_runs, function(run) {
          paste(xml_text(xml_find_all(run, ".//w:t")), collapse = "")
        }, character(1)),
        collapse = ""
      )

      if (contains_bullets(para)) {
        paste("*", content)
      } else {
        content
      }
    }, character(1))
  }, comment_paragraphs, comment_text_runs)

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
      names(paragraphs)
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

contains_del <- function(x) {
  grepl("\\<w\\:delText", as.character(x))
}

contains_bullets <- function(x) {
  grepl(r"(ListParagraph\"/>)", as.character(x))
}

# bullets_2_numbers
#
# Convert bullets to numbered lists
# @param x character vector
# @return character vector
# @export
# @examples
# bullets_2_numbers(c("* First bullet\n\n* Second bullet\n\n* Third bullet\n\n"))

bullets_2_numbers <- function(x) {
  pattern <- "(?<=^|\\n)\\*"
# Count number of bullets following newlines
  n_bullets <- gregexpr(pattern, x, perl = TRUE)[[1]] |>
    length()

  replacements <- paste0(seq_len(n_bullets), ".")

  # replace bullets with numbers
  for (i in seq_len(n_bullets)) {
    x <- sub(pattern, replacements[i], x, perl = TRUE)
  }

  x

}

# to_bold
# 
# Add asterisks before or after a target string
# @param string character vector
# @param start start of string to match
# @param end end of string to match
# @examples revise:::to_bold("This is a string", "is", "string")
# export

to_bold <- function(string, start, end) {
  # Try to find a start marker that is not part of a larger word.
  pattern <- paste0("(?<![A-Za-z])", start)
  start_pos <- regexpr(pattern, string, perl = TRUE)
  if (start_pos == -1) {
    # Fallback to a plain search if the boundary‐constrained search fails.
    start_pos <- regexpr(start, string, fixed = TRUE)
    if (start_pos == -1) return(string)
  }
  
  # Work from the found start position.
  sub_string <- substring(string, start_pos)
  end_pos <- regexpr(end, sub_string, fixed = TRUE)
  if (end_pos == -1) return(string)
  
  bold_end <- start_pos + end_pos - 1 + attr(end_pos, "match.length")
  
  prefix <- substring(string, 1, start_pos - 1)
  bold_text <- substring(string, start_pos, bold_end)
  suffix <- substring(string, bold_end + 1, nchar(string))
  
  sprintf("%s**%s**%s", prefix, bold_text, suffix)

}
