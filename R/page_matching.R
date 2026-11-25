# Single-page candidates come first, so ties favor a single page over a range.
pdf_page_candidates <- function(pdf_text) {
  ids <- trimws(as.character(pdf_text$page_id))
  text <- clean_string(pdf_text$text)
  text[is.na(text)] <- ""
  valid <- !is.na(ids) & nzchar(ids)
  left <- seq_len(max(0L, length(ids) - 1L))
  left <- left[valid[left] & valid[left + 1L]]
  right <- left + 1L

  windows <- trimws(paste(text[left], text[right], recycle0 = TRUE))
  ranges <- paste0(ids[left], "--", ids[right], recycle0 = TRUE)
  candidates <- c(text[valid], windows)
  labels <- c(ids[valid], ranges)
  keep <- nzchar(candidates)
  stats::setNames(candidates[keep], labels[keep])
}

word_windows <- function(words, size) {
  vapply(seq_len(max(1L, length(words) - size + 1L)), function(start) {
    end <- min(start + size - 1L, length(words))
    paste(words[seq.int(start, end)], collapse = " ")
  }, character(1))
}

exact_pdf_page <- function(target, candidates) {
  matches <- which(grepl(target, candidates, fixed = TRUE))
  names(candidates)[utils::head(matches, 1L)]
}

fuzzy_pdf_page <- function(target, candidates, max.distance = .15) {
  words <- strsplit(target, "\\s+")[[1]]
  phrases <- unique(word_windows(words, min(length(words), 8L)))
  # Phrase coverage tolerates missing text around floats or citations.
  hits <- integer(length(candidates))
  for (phrase in phrases) hits <- hits + grepl(phrase, candidates, fixed = TRUE)
  best <- which.max(hits)
  if (hits[best] / length(phrases) >= .3) return(names(candidates)[best])

  # Otherwise find the closest window with as many words as the excerpt.
  distance <- vapply(candidates, function(text) {
    windows <- word_windows(strsplit(text, "\\s+")[[1]], length(words))
    edits <- as.vector(utils::adist(target, windows))
    min(edits / pmax(nchar(target), nchar(windows)))
  }, numeric(1))
  best <- which.min(distance)
  if (distance[best] <= max.distance) names(candidates)[best] else character()
}

match_pdf_pages <- function(target, candidates, max.distance = .15) {
  if (is.na(target) || !nzchar(target) || !length(candidates)) return(character())
  exact <- exact_pdf_page(target, candidates)
  if (length(exact)) exact else fuzzy_pdf_page(target, candidates, max.distance)
}

get_pdf_pagenumber <- function(string, pdf_text, max.distance = .15) {
  match_pdf_pages(clean_string(string), pdf_page_candidates(pdf_text), max.distance)
}

# Keep whole words when taking an anchor from either end of an excerpt.
page_anchor <- function(words, size, from_end = FALSE) {
  if (from_end) words <- rev(words)
  end <- which(cumsum(nchar(words) + 1L) - 1L >= size)[1]
  if (!is.na(end)) words <- words[seq_len(end)]
  if (from_end) words <- rev(words)
  paste(words, collapse = " ")
}

combine_page_matches <- function(start, end) {
  if (!length(start)) return(end)
  if (!length(end)) return(start)

  first <- sub("--.*$", "", start)
  last <- sub("^.*--", "", end)
  if (identical(first, last)) return(first)

  bounds <- suppressWarnings(as.integer(c(first, last)))
  if (!anyNA(bounds) && bounds[1] > bounds[2]) return(character())
  paste0(first, "--", last)
}

match_excerpt_pages <- function(string, pdf_text, search_length, split_string) {
  target <- clean_string(string)
  if (is.na(target) || !nzchar(target)) return(character())
  candidates <- pdf_page_candidates(pdf_text)
  if (!length(candidates)) return(character())

  exact <- exact_pdf_page(target, candidates)
  if (length(exact)) return(exact)
  if (!split_string && nchar(string) <= search_length) {
    return(fuzzy_pdf_page(target, candidates))
  }

  words <- strsplit(target, "\\s+")[[1]]
  start <- match_pdf_pages(page_anchor(words, search_length), candidates)
  end <- match_pdf_pages(page_anchor(words, search_length, from_end = TRUE), candidates)
  bounds <- combine_page_matches(start, end)

  # Repeated text can put both anchors on one page even when the full excerpt
  # matches a two-page window.
  if (length(bounds) && grepl("--", bounds, fixed = TRUE)) return(bounds)
  full <- fuzzy_pdf_page(target, candidates)
  if (!length(bounds) || any(grepl("--", full, fixed = TRUE))) full else bounds
}
