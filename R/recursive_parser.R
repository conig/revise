#' extract_sections
#'
#' Extract sections from text string
#' @param string string to extract md sections from
#' @param is_span logical, whether to extract sections in square brackets (default) or spans

extract_sections <- function(string, is_span = FALSE) {
  if (is_span) {
    regex_header = "<span.+?id.{0,}?=.{0,}?\\b(.+?)\\b.+?>"
    regex_end = "(?<=</span>)"
    regex_issection = "</span>"
    open = "<span"
    close = "</span>"
  } else {
    regex_header = "(?<=\\]\\{#)[^{}]+(?=\\})"
    regex_end = "\\]\\{#.+?\\}"
    regex_issection = "\\]\\{#"
    open = "["
    close = "]"
  }
  # Are there any sections?
  if (
    !all(
      grepl(regex_header, string, perl = TRUE),
      grepl(regex_end, string, perl = TRUE),
      grepl(open, string, fixed = TRUE),
      grepl(close, string, fixed = TRUE)
    )
  ) {
    return(NULL)
  }
  sectionends <- gregexpr(pattern = regex_end, text = string, perl = TRUE)[[1]]
  if (!is_span) {
    closingbrackets <- gregexpr(pattern = "}", text = string, fixed = TRUE)[[1]]
    sectionends <- sapply(sectionends, function(x) {
      closingbrackets[which.min(abs(closingbrackets - x))]
    })
  }
  sections <- lapply(sectionends, function(i) {
    substring(string, first = 1, last = i)
  })
  sections <- sections[grepl(pattern = regex_issection, x = sections)]
  sections_clean <- lapply(sections, function(x) {
    openbrackets <- list(
      position = gregexpr(pattern = open, text = x, fixed = TRUE)[[1]]
    )
    openbrackets$type <- rep("open", times = length(openbrackets$position))
    closingbrackets <- list(
      position = gregexpr(pattern = close, text = x, fixed = TRUE)[[1]]
    )
    closingbrackets$type <- rep(
      "closed",
      times = length(closingbrackets$position)
    )
    allbrackets <- openbrackets
    allbrackets$position <- c(allbrackets$position, closingbrackets$position)
    ordr <- order(allbrackets$position, decreasing = FALSE)
    allbrackets$type <- c(openbrackets$type, closingbrackets$type)[ordr]
    allbrackets$position <- allbrackets$position[ordr]
    out <- substring(
      text = x,
      first = (findposition(x = allbrackets)),
      last = (nchar(x))
    )
    headr <- regmatches(out, gregexpr(regex_header, out, perl = TRUE))[[1]]
    headr <- headr[c(length(headr), 1)[as.integer(is_span) + 1]]
    c(out, headr)
  })
  sectionheaders <- sapply(sections_clean, `[[`, 2)
  sections_clean <- sapply(sections_clean, `[[`, 1)
  if (is_span) {
    sectionheaders <- gsub(
      "^.+?id.{0,}?=.{0,}?[\"'](.+?)[\"'].*$",
      "\\1",
      sectionheaders
    )
    sections_clean <- lapply(
      sections_clean,
      gsub,
      pattern = "^.+?>(.+)<.+?$",
      replacement = "\\1"
    )
  } else {
    sections_clean <- lapply(
      sections_clean,
      gsub,
      pattern = "^\\[(.+)\\]\\{#.+?\\}$",
      replacement = "\\1"
    )
  }

  out <- sections_clean
  # if(!length(sectionheaders) == length(out)){
  #   warning("The number of section headers does not correspond to the number of sections. Extracted the following sectionheaders:\n", paste0("  ", sectionheaders, "\n", collapse = ""))
  # }
  names(out) <- sectionheaders #[1:length(out)]
  return(out)
}

#' extract_fenced_sections
#'
#' Extract sections from fenced divs using markdown tags
#' @param string string to extract md sections from
#' @param ignore_ids character vector of ids to skip (default: "refs")
extract_fenced_sections <- function(string, ignore_ids = c("refs")) {
  if (!grepl("(?m)^:{3,}", string, perl = TRUE)) {
    return(NULL)
  }
  lines <- strsplit(string, "\n", fixed = TRUE)[[1]]
  open_line <- "^:{3,}\\s*\\S"
  close_line <- "^:{3,}\\s*$"
  if (!any(grepl(open_line, lines, perl = TRUE))) {
    return(NULL)
  }
  sections <- list()
  headers <- character()
  stack <- list()

  for (i in seq_along(lines)) {
    line <- lines[[i]]
    if (grepl(open_line, line, perl = TRUE)) {
      id <- NA_character_
      id_match <- regexpr("#[^\\s}]+", line, perl = TRUE)
      if (id_match[1] > 0) {
        id <- substr(
          line,
          id_match[1] + 1,
          id_match[1] + attr(id_match, "match.length") - 1
        )
      }
      stack[[length(stack) + 1]] <- list(line = i, id = id, has_id = !is.na(id))
      next
    }

    if (grepl(close_line, line, perl = TRUE)) {
      if (length(stack) == 0) {
        next
      }
      open <- stack[[length(stack)]]
      stack <- stack[-length(stack)]
      if (isTRUE(open$has_id) && !open$id %in% ignore_ids) {
        if (i - open$line > 1) {
          section <- paste(lines[(open$line + 1):(i - 1)], collapse = "\n")
        } else {
          section <- ""
        }
        sections[[length(sections) + 1]] <- section
        headers <- c(headers, open$id)
      }
    }
  }

  if (length(sections) == 0) {
    return(NULL)
  }
  names(sections) <- headers
  sections
}


#' @importFrom utils tail
findposition <- function(x, level = 0) {
  len <- length(x$position)
  x$position <- x$position[-len]
  x$type <- x$type[-len]
  if (tail(x$type, 1) == "open") {
    if (level == 0) {
      return(tail(x$position, 1))
    } else {
      findposition(x = x, level = level - 1)
    }
  } else {
    findposition(x = x, level = level + 1)
  }
}
