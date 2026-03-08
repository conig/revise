#' Extract Tagged Text from a Manuscript
#'
#' Retrieves and formats a specified section from a revise_manuscript object based on its ID.
#' @param id the id as a character string from a html or markdown tag
#' @param manuscript Optional; a revise_manuscript object in which to search
#' for `id`. Defaults to `NULL`, in which case the manuscript object
#' is found in the environment.
#' @param quote should the output string be quoted by inserting '>' before the string?
#' @param evaluate logical. Should inline rchunks be executed?
#' @param split_string should only the start and end of the string be searched for?
#' @param search_length numeric. Searches for the first n and n characters in a string. Shorten if difficult to find passages split by floats.
#' @param include_pgnum logical. include PDF page number? Requires a PDF to be provided to the manuscript object.
#' @param revise_errors logical. If FALSE, failure to match manuscript sections will result in warnings rather than errors.
#' @param envir The environment in which to find the manuscript.
#' @param trust_manuscript logical. If `TRUE`, allow inline `r` code in
#' the manuscript to be evaluated when `evaluate = TRUE`. Defaults to
#' `getOption("revise_trust_manuscript") which when unset is assumed to be FALSE`.
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
    if (exists(".revise_manuscripts", where = envir)) {
      manuscript <- get(".revise_manuscripts", envir = envir)
    } else {
      warning(
        "Argument 'manuscript' is NULL, and no manuscript exists in the environment."
      )
      return(NULL)
    }
  }
  cl <- match.call()
  cl[["manuscript"]] <- manuscript
  if (inherits(manuscript, "revise_corpus")) {
    cl[[1L]] <- str2lang("revise:::get_revision.revise_corpus")
  } else {
    cl[[1L]] <- str2lang("revise:::get_revision.default")
  }
  eval.parent(cl)
}

get_revision.revise_corpus <- function(
  id,
  manuscript = NULL,
  quote = TRUE,
  evaluate = TRUE,
  split_string = FALSE,
  search_length = 300,
  include_pgnum = TRUE,
  revise_errors = getOption("revise_errors"),
  envir,
  trust_manuscript = getOption("revise_trust_manuscript", FALSE)
) {
  all_sect_names <- unlist(lapply(manuscript, function(x) {
    names(x[["sections"]])
  }))
  if (!isTRUE(length(all_sect_names) > 0)) {
    return(NULL)
  }
  tryCatch(
    check_dup_sections(all_sect_names, revise_errors = revise_errors),
    warning = function(w) {
      if (
        !isTRUE(sapply(manuscript, function(x) {
          any(duplicated(names(x[["sections"]])))
        }))
      ) {
        w <- paste0(
          w,
          "You have loaded multiple manuscripts, and these manuscripts contain identical section names."
        )
      }
      warning(gsub("simpleWarning: ", "", w, fixed = TRUE), call. = FALSE)
    }
  )
  # Here you could check for similar names; if so, turn that into a function
  if (!id %in% all_sect_names) {
    return(NULL)
  }
  # If it does exist, select the first instance
  cl <- match.call()
  cl[["manuscript"]] <- manuscript[[which(sapply(manuscript, function(x) {
    any(names(x[["sections"]]) == id)
  }))[1]]]
  cl[[1L]] <- str2lang("revise:::get_revision.default")
  eval.parent(cl)
}

#' Build a cleaned anchor string for long-section page matching
#'
#' Converts extracted manuscript text into a cleaned, word-aligned prefix or
#' suffix anchor. This avoids matching fragments that begin or end inside
#' markup, citations, or punctuation that disappear in the rendered PDF.
#' @param string Extracted manuscript section.
#' @param search_length Approximate target anchor length in characters.
#' @param side Which side of the section to use: `"start"` or `"end"`.
#' @return Cleaned anchor string, or `character(0)` when no anchor can be built.
build_page_anchor <- function(string, search_length, side = c("start", "end")) {
  side <- match.arg(side)
  cleaned <- clean_string(string)

  if (!nzchar(cleaned)) {
    return(character(0))
  }

  words <- strsplit(cleaned, "\\s+")[[1]]
  if (length(words) == 0) {
    return(character(0))
  }

  if (side == "end") {
    words <- rev(words)
  }

  anchor <- character()
  current_length <- 0
  for (word in words) {
    anchor <- c(anchor, word)
    current_length <- nchar(paste(anchor, collapse = " "))
    if (current_length >= search_length) {
      break
    }
  }

  if (side == "end") {
    anchor <- rev(anchor)
  }

  paste(anchor, collapse = " ")
}

#' Split a page label into its bounds
#'
#' Converts page labels such as `"5"` or `"5--6"` into their component page
#' bounds so callers can compare or merge page estimates.
#' @param page_id Character page label.
#' @return Character vector of page bounds, or `character(0)` for missing input.
extract_page_bounds <- function(page_id) {
  if (page_match_is_missing(page_id)) {
    return(character(0))
  }

  strsplit(page_id[1], "--", fixed = TRUE)[[1]]
}

#' Combine start and end page estimates into one label
#'
#' Merges page matches derived from the start and end anchors of a long section.
#' If both anchors resolve, the result spans from the earliest start page to the
#' latest end page using `--` as the range separator.
#' @param start_match Page label inferred from the start anchor.
#' @param end_match Page label inferred from the end anchor.
#' @return Single page label, page range label, or `character(0)` when unusable.
combine_page_matches <- function(start_match, end_match) {
  if (page_match_is_missing(start_match) && page_match_is_missing(end_match)) {
    return(character(0))
  }
  if (page_match_is_missing(start_match)) {
    return(end_match[1])
  }
  if (page_match_is_missing(end_match)) {
    return(start_match[1])
  }

  start_bounds <- extract_page_bounds(start_match)
  end_bounds <- extract_page_bounds(end_match)
  start_page <- start_bounds[1]
  end_page <- end_bounds[length(end_bounds)]

  if (!nzchar(start_page) || !nzchar(end_page)) {
    return(character(0))
  }

  if (
    !is.na(suppressWarnings(as.integer(start_page))) &&
    !is.na(suppressWarnings(as.integer(end_page))) &&
    as.integer(start_page) > as.integer(end_page)
  ) {
    return(character(0))
  }

  if (identical(start_page, end_page)) start_page else paste0(start_page, "--", end_page)
}

#' Normalize page match output to a single label
#'
#' Collapses page-match outputs into one canonical label so downstream rendering
#' never receives blank strings or comma-separated vectors of page IDs.
#' @param page_id Character vector of page labels.
#' @return Single normalized page label, or `character(0)` if no usable label exists.
normalize_page_match <- function(page_id) {
  if (page_match_is_missing(page_id)) {
    return(character(0))
  }

  page_id <- trimws(page_id)
  page_id <- page_id[nzchar(page_id)]

  if (length(page_id) == 0) {
    return(character(0))
  }
  if (length(page_id) == 1) {
    return(page_id)
  }

  combine_page_matches(page_id[1], page_id[length(page_id)])
}

get_revision.default <- function(
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
    if (
      ".revise_manuscripts" %in%
        objects(envir = parent.frame(1), all.names = TRUE)
    ) {
      manuscript <- get(".revise_manuscripts", envir = parent.frame(1))
    } else {
      warning(
        "Argument 'manuscript' is NULL, and no manuscript exists in the environment."
      )
      return(NULL)
    }
  }
  if (is.null(manuscript[["sections"]])) {
    return(NULL)
  }
  check_dup_sections(names(manuscript$sections), revise_errors = revise_errors)
  string <- manuscript$sections[[id]]

  if (is.null(string)) {
    section_names <- names(manuscript$sections)
    similar_id <- agrep(id, section_names, value = TRUE)
    similar_id <- paste(similar_id, collapse = " | ")
    quoted_id <- paste0("\"", id, "\"")
    has_double_quoted_match <- quoted_id %in% section_names
    message <- paste0(
      "Couldn't find a section in the manuscript tagged as \"",
      id,
      "\"."
    )
    if (has_double_quoted_match) {
      message <- paste0(
        message,
        " It looks like the manuscript tag includes surrounding double quotes (",
        quoted_id,
        "). For pandoc div IDs, use unquoted tags like {#tag instead of {#\"tag}",
        id,
        "}."
      )
    }
    if (nchar(similar_id) > 0) {
      message <- paste0(message, " Did you mean: ", similar_id, "?")
    }

    if (!revise_errors) {
      warning(message)
      return(paste0("**", message, "**"))
    } else {
      stop(message)
    }
  }

  if (evaluate) {
    string <- evaluate_inline(
      string,
      envir = envir,
      trust_manuscript = trust_manuscript
    )
  }

  string <- sanitize_pandoc_tag_artifacts(string)

  if (!is.null(manuscript$PDF) & include_pgnum) {
    pnum <- character(0)

    if ((nchar(string) > search_length) | split_string) {
      start_string <- build_page_anchor(string, search_length, side = "start")
      end_string <- build_page_anchor(string, search_length, side = "end")
      pnum.full <- character(0)

      pnum.start <- if (!page_match_is_missing(start_string)) {
        get_pdf_pagenumber(start_string, pdf_text = manuscript$PDF)
      } else {
        character(0)
      }
      pnum.end <- if (!page_match_is_missing(end_string)) {
        get_pdf_pagenumber(end_string, pdf_text = manuscript$PDF)
      } else {
        character(0)
      }

      pnum <- combine_page_matches(pnum.start, pnum.end)

      pnum.full <- normalize_page_match(
        get_pdf_pagenumber(string, pdf_text = manuscript$PDF)
      )

      if (
        page_match_is_missing(pnum) ||
        (!page_match_is_missing(pnum.full) &&
         length(extract_page_bounds(pnum.full)) > length(extract_page_bounds(pnum)))
      ) {
        pnum <- pnum.full
      }
    } else {
      pnum <- get_pdf_pagenumber(string, pdf_text = manuscript$PDF)
    }

    pnum <- normalize_page_match(pnum)

    if (page_match_is_missing(pnum)) {
      warning(
        "Couldn't match the extracted text to the target PDF: ",
        id,
        ". Have you knit the manuscript since making recent changes?"
      )
      pnum <- "?"
    }

    string = paste0(
      string,
      "\n\n\\begin{flushright}Pg. ",
      pnum,
      "\\end{flushright}"
    )
  }

  string <- header_to_bold(string)

  for (i in seq_along(manuscript$refs$tables[, 1])) {
    # replace \\@ref(tab:)
    string <-
      gsub(
        manuscript$refs$tables$text[i],
        manuscript$refs$tables$ref[i],
        string,
        fixed = TRUE
      )
  }

  for (i in seq_along(manuscript$refs$figures[, 1])) {
    # replace \\@ref(fig:)
    string <-
      gsub(
        manuscript$refs$figures$text[i],
        manuscript$refs$figures$ref[i],
        string,
        fixed = TRUE
      )
  }

  if (quote) {
    string <- gsub("\\n", "\\\n>", string)
    string <- paste0(">", string)
  }

  string
}

sanitize_pandoc_tag_artifacts <- function(string) {
  if (!is.character(string) || length(string) != 1L || is.na(string)) {
    return(string)
  }

  out <- string

  # Remove leaked markdown id fragments like ]{#tag} from malformed tags.
  out <- gsub("\\]\\{#[-A-Za-z0-9_:.]+\\}", "", out, perl = TRUE)
  out <- gsub("\\{#[-A-Za-z0-9_:.]+\\}", "", out, perl = TRUE)

  # Remove span wrappers when overlap with markdown id tags leaves fragments.
  out <- gsub("<span\\b[^>]*>", "", out, perl = TRUE)
  out <- gsub("</span>", "", out, fixed = TRUE)

  # Remove standalone fenced-div markers if they leak through extraction.
  out <- gsub("(?m)^:{3,}\\s*\\{[^\\n]*\\}\\s*$", "", out, perl = TRUE)
  out <- gsub("(?m)^:{3,}\\s*$", "", out, perl = TRUE)

  gsub("\\n{3,}", "\n\n", out, perl = TRUE)
}
