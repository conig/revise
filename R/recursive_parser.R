
#' extract_sections
#'
#' Extract sections from text string
#' @param string string to extract md sections from
#' @param is_span logical, whether to extract sections in square brackets (default) or spans

extract_sections <- function(string, is_span = FALSE) {
  tags <- section_tags(string, is_span)
  if (!nrow(tags)) return(NULL)
  text <- substring(string, tags$open + tags$open_len, tags$close - 1L)
  stats::setNames(as.list(text), tags$id)
}

# Pair delimiters before selecting tagged ranges: an untagged span or a
# citation inside a tag still participates in nesting.
section_tags <- function(string, is_span = FALSE) {
  pattern <- if (is_span) r"(<span\b(?:[^>"']|"[^"]*"|'[^']*')*>|</span\s*>)" else
    r"(\[|\](?:\{#[^{}\n]+\})?)"
  hits <- gregexpr(pattern, string, perl = TRUE)
  tokens <- regmatches(string, hits)[[1]]
  open <- startsWith(tokens, if (is_span) "<span" else "[")
  stack <- integer(length(tokens))
  partner <- integer(length(tokens))
  depth <- 0L
  for (i in seq_along(tokens)) {
    if (open[i]) {
      depth <- depth + 1L
      stack[depth] <- i
    } else if (depth) {
      partner[i] <- stack[depth]
      depth <- depth - 1L
    }
  }

  ends <- which(partner > 0L)
  starts <- partner[ends]
  ids <- if (is_span) {
    stringr::str_match(tokens[starts], r"-(^<span(?:[^>"']|"[^"]*"|'[^']*')*?\sid\s*=\s*(["'])(.*?)\1)-")[, 3]
  } else {
    stringr::str_match(tokens[ends], r"(\]\{#(.+)\})")[, 2]
  }
  pos <- as.integer(hits[[1]])
  len <- attr(hits[[1]], "match.length")
  keep <- !is.na(ids) & nzchar(ids)
  data.frame(open = pos[starts[keep]], open_len = len[starts[keep]],
             close = pos[ends[keep]], id = ids[keep])
}

#' extract_fenced_sections
#'
#' Extract sections from fenced divs using markdown tags
#' @param string string to extract md sections from
#' @param ignore_ids character vector of ids to skip (default: "refs")
extract_fenced_sections <- function(string, ignore_ids = c("refs")) {
  lines <- strsplit(string, "\n", fixed = TRUE)[[1]]
  fences <- grep("^:{3,}", lines)
  stack <- integer()
  sections <- list()
  ids <- character()

  for (i in fences) {
    if (!grepl("^:{3,}[ \t]*$", lines[i])) {
      stack <- c(stack, i)
      next
    }
    if (!length(stack)) next

    start <- stack[length(stack)]
    stack <- stack[-length(stack)]
    id <- fenced_div_id(lines[start])
    if (is.na(id) || id %in% ignore_ids) next

    content <- if (i > start + 1L) lines[seq.int(start + 1L, i - 1L)]
    sections[[length(sections) + 1L]] <- paste(content, collapse = "\n")
    ids <- c(ids, id)
  }

  if (length(sections)) stats::setNames(sections, ids)
}

fenced_div_id <- function(line) {
  attributes <- stringr::str_match(line, "^:{3,}[ \t]*\\{(.*)\\}[ \t]*$")[, 2]
  if (is.na(attributes)) return(NA_character_)

  # Quoted attribute values can contain spaces and hashes; keep each token intact.
  tokens <- stringr::str_extract_all(attributes, r"((?:[^[:space:]"']+|"[^"]*"|'[^']*')+)")[[1]]
  ids <- tokens[startsWith(tokens, "#") & nchar(tokens) > 1L]
  substring(ids[1], 2L)
}
