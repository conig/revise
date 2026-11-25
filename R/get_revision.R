#' Extract Tagged Text from a Manuscript
#'
#' Retrieves and formats a specified section from a revise_manuscript object based on its ID.
#' @param id the id as a character string from a html or markdown tag
#' @param manuscript Optional; a revise_manuscript object in which to search
#' for `id`. Defaults to `NULL`, in which case the manuscript object
#' is found in the environment.
#' @param quote should the output string be quoted by inserting '>' before the string?
#' @param evaluate logical. Should inline rchunks be executed?
#' @param split_string Also match the start and end of the excerpt separately.
#'   This is done automatically for excerpts longer than `search_length`.
#' @param search_length Approximate length in characters of each start or end
#'   excerpt used for matching. Shorten if floats interrupt the passage.
#' @param include_pgnum logical. include PDF page number? Requires a PDF to be provided to the manuscript object.
#' @param revise_errors logical. If FALSE, failure to match manuscript sections will result in warnings rather than errors.
#' @param envir The environment in which to find a loaded manuscript and evaluate
#'   its inline R code.
#' @param trust_manuscript logical. If `TRUE`, allow inline `r` code in
#' the manuscript to be evaluated when `evaluate = TRUE`. Defaults to
#' `getOption("revise_trust_manuscript", FALSE)`.
#' @return A character vector of length 1 containing the text extracted from the manuscript that was tagged with `id`, including any modifications as specified by the arguments.
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
  if (is.null(manuscript$sections)) return(NULL)
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
  # Overlapping tag styles can leave partial wrappers in the extracted text.
  string <- gsub("\\]?\\{#[-A-Za-z0-9_:.]+\\}|<span\\b[^>]*>|</span>",
                 "", string, perl = TRUE)
  string <- gsub("(?m)^:{3,}[ \t]*(?:\\{[^\n]*\\})?[ \t]*$", "", string, perl = TRUE)
  gsub("\n{3,}", "\n\n", string)
}
