#' Extract Tagged Text from a Manuscript
#'
#' Retrieve and format an excerpt from a manuscript loaded with [read_manuscript()].
#' @param id Character ID of the manuscript tag.
#' @param manuscript A `revise_manuscript` or `revise_corpus` in which to look
#'   for `id`. If `NULL`, use the manuscripts registered in `envir`.
#' @param quote Logical. Format the excerpt as a Markdown block quote?
#' @param evaluate Logical. Evaluate inline R if present? Set to `FALSE` to
#'   insert the excerpt without running code.
#' @param split_string Logical. Also try matching the start and end of the
#'   excerpt separately when looking for its PDF pages? This is tried
#'   automatically if the excerpt is longer than `search_length` and an exact
#'   match for the full excerpt is not found.
#' @param search_length Approximate length in characters of the start and end
#'   excerpts used for PDF matching. Shorten if floats interrupt the passage.
#' @param include_pgnum Logical. Append a LaTeX page reference for a PDF
#'   revision letter if the manuscript has an associated PDF?
#' @param revise_errors Logical. When `FALSE`, duplicate IDs or a missing ID in
#'   a single manuscript give a warning instead of an error.
#' @param envir Environment for manuscript lookup and inline R evaluation.
#' @param trust_manuscript Logical. Allow inline R to run when `evaluate = TRUE`?
#'   Set to `TRUE` only after reviewing the manuscript's code. Defaults to
#'   `getOption("revise_trust_manuscript", FALSE)`.
#' @return A character string containing the formatted excerpt. Returns `NULL`
#'   if no manuscript is registered, the manuscript has no tagged sections, or
#'   no manuscript in a corpus contains the ID. For an unknown ID in a single
#'   manuscript, `revise_errors = FALSE` returns a diagnostic string.
#' @details Page matching uses the text extracted from the PDF. If it cannot
#'   locate the excerpt, it warns and appends `Pg. ?`. Use a PDF rendered from
#'   the current manuscript. Set `include_pgnum = FALSE` for Word or plain-text
#'   revision letters when the manuscript also has a PDF attached.
#' @seealso [read_manuscript()], [revise_letter_pdf()]
#' @export

get_revision <- function(
  id,
  manuscript = NULL,
  quote = TRUE,
  evaluate = TRUE,
  split_string = FALSE,
  search_length = 300,
  include_pgnum = TRUE,
  revise_errors = getOption("revise_errors"),
  envir = parent.frame(1L),
  trust_manuscript = getOption("revise_trust_manuscript", FALSE)
) {
  if (is.null(manuscript)) {
    manuscript <- get0(".revise_manuscripts", envir = envir)
    if (is.null(manuscript)) {
      warning("Argument 'manuscript' is NULL, and no manuscript exists in the environment.")
      return(NULL)
    }
  }
  if (inherits(manuscript, "revise_corpus")) {
    ids <- lapply(manuscript, function(x) names(x$sections))
    check_dup_sections(unlist(ids, use.names = FALSE), revise_errors)
    matches <- vapply(ids, function(x) id %in% x, logical(1))
    if (!any(matches)) return(NULL)
    manuscript <- manuscript[[which(matches)[1]]]
  } else {
    check_dup_sections(names(manuscript$sections), revise_errors)
  }
  if (!length(manuscript$sections)) return(NULL)
  string <- manuscript$sections[[id]]

  if (is.null(string)) {
    msg <- missing_section_message(id, names(manuscript$sections))
    if (revise_errors) stop(msg)
    warning(msg)
    return(paste0("**", msg, "**"))
  }

  if (evaluate) {
    string <- evaluate_inline(
      string,
      envir = envir,
      trust_manuscript = trust_manuscript
    )
  }

  string <- sanitize_pandoc_tag_artifacts(string)

  if (!is.null(manuscript$PDF) && include_pgnum) {
    page <- match_excerpt_pages(string, manuscript$PDF, search_length, split_string)
    if (!length(page)) {
      warning(
        "Couldn't match the extracted text to the target PDF: ",
        id,
        ". Have you knit the manuscript since making recent changes?"
      )
      page <- "?"
    }
    string <- paste0(string, "\n\n\\begin{flushright}Pg. ", page, "\\end{flushright}")
  }

  string <- header_to_bold(string)

  for (refs in manuscript$refs[c("tables", "figures")]) {
    for (i in seq_along(refs$text)) {
      string <- gsub(refs$text[i], refs$ref[i], string, fixed = TRUE)
    }
  }

  if (quote) {
    string <- gsub("\\n", "\\\n>", string)
    string <- paste0(">", string)
  }

  string
}

missing_section_message <- function(id, section_names) {
  msg <- paste0('Couldn\'t find a section in the manuscript tagged as "', id, '".')
  quoted_id <- paste0('"', id, '"')
  if (quoted_id %in% section_names) {
    msg <- paste0(
      msg, " The manuscript tag includes surrounding double quotes. ",
      "Use {#", id, "} instead of {#", quoted_id, "}."
    )
  }
  similar <- agrep(id, as.character(section_names), value = TRUE)
  if (length(similar)) {
    msg <- paste0(msg, " Did you mean: ", paste(similar, collapse = " | "), "?")
  }
  msg
}

sanitize_pandoc_tag_artifacts <- function(string) {
  if (grepl("]{#", string, fixed = TRUE)) {
    tags <- section_tags(string)
    # Drop matched opening brackets as well; leave ordinary Markdown intact.
    if (nrow(tags)) {
      chars <- strsplit(string, "", fixed = TRUE)[[1]]
      string <- paste(chars[-tags$open], collapse = "")
    }
  }
  # Overlapping tag styles can leave partial wrappers in the extracted text.
  string <- gsub("\\]?\\{#[-A-Za-z0-9_:.]+\\}|<span\\b[^>]*>|</span>",
                 "", string, perl = TRUE)
  string <- gsub("(?m)^:{3,}[ \t]*(?:\\{[^\n]*\\})?[ \t]*$", "", string, perl = TRUE)
  gsub("\n{3,}", "\n\n", string)
}
