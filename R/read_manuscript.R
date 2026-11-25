#' Read a Tagged Manuscript
#'
#' Read tagged excerpts from a Markdown, R Markdown, or Word (.docx) manuscript.
#' @param path Character path to the manuscript.
#' @param PDF Use `FALSE` (the default) to skip PDF page lookup, `TRUE` to
#'   look for a PDF alongside the manuscript with the same base name, or a
#'   character path to the matching PDF.
#' @param to_envir Logical. Register the manuscript in `envir` for later
#'   lookup? Defaults to `getOption("revise_use_envir", TRUE)`.
#' @param envir Environment in which to register the manuscript.
#' @return Invisibly returns a list of class `revise_manuscript` with these
#'   elements:
#'
#' * `sections`: Tagged excerpts, named by tag ID.
#' * `PDF`: Extracted page text, or `NULL` when no PDF was supplied.
#' * `refs`: Figure and table references, or `NULL` for Word manuscripts.
#' * `rmd`: Raw Markdown source, or `NULL` for Word manuscripts.
#' * `mtime`, `checksum`, and `filename`: The source file's modification time,
#'   checksum, and base name.
#' @details In Markdown, use `[excerpt]{#id}`, an HTML span with an `id`
#'   attribute, or a fenced div such as `::: {#id}` for longer excerpts. In
#'   Word, select the excerpt and add a comment containing `Revise::id`.
#'
#'   By default, the manuscript is registered as `.revise_manuscripts` in
#'   `envir`, so [get_revision()] can find it without a `manuscript` argument.
#'   Rereading a file with the same base name replaces its registered copy.
#'   Set `to_envir = FALSE` to use the returned object without registering it.
#' @examples
#' path <- tempfile(fileext = ".md")
#' writeLines("[A revised passage.]{#example}", path)
#' manuscript <- read_manuscript(path, to_envir = FALSE)
#' get_revision("example", manuscript)
#' @seealso [get_revision()], [list_manuscripts()], [remove_manuscript()]
#' @export
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom tools file_ext
read_manuscript <- function(path, PDF = FALSE, to_envir = getOption("revise_use_envir", TRUE), envir = parent.frame(1)){
  doc_checksum <- tools::md5sum(path)
  if(!is_docx(path)){
    rmd <- paste0(readLines(path, encoding = "UTF8"), collapse = "\n")
    sections <- c(extract_sections(rmd),
                extract_sections(rmd, is_span = TRUE),
                extract_fenced_sections(rmd))
    refs <- extract_refs(rmd)
  }else{
   sections <- read_docx(path)
   rmd <- NULL
   refs <- NULL
  }


  check_dup_sections(names(sections), revise_errors = FALSE)
  if (isTRUE(PDF)) {
    if (!is.null(rmd) && isTRUE(tryCatch(rmarkdown::yaml_front_matter(path)$floatsintext,
                                         error = function(e) FALSE))) {
      warning("floatsintext is set to TRUE in target rmarkdown. This may disrupt page number matching where target strings cross pages.")
    }
    PDF <- paste0(tools::file_path_sans_ext(path), ".pdf")
  }
  PDF <- if (is.character(PDF)) process_pdf(PDF)

  # Prepare output object
  manuscript <- list(sections = sections,
                     PDF = PDF,
                     refs = refs,
                     rmd = rmd,
                     mtime = file.info(path)$mtime,
                     checksum = doc_checksum,
                     filename = basename(path))
  class(manuscript) <- c("revise_manuscript", class(manuscript))
  # Load to environment
  if (to_envir) {
    add_to_envir(manuscript = manuscript, envir = envir)
  }
  # Return
  return(invisible(manuscript))
}

add_to_envir <- function(manuscript, envir){
  corpus <- get0(".revise_manuscripts", envir = envir, inherits = FALSE)
  if (inherits(corpus, "revise_manuscript")) {
    corpus <- stats::setNames(list(corpus), corpus$filename)
  }
  corpus[[manuscript$filename]] <- manuscript
  value <- if (length(corpus) == 1L) corpus[[1L]] else structure(corpus, class = c("revise_corpus", "list"))
  assign(".revise_manuscripts", value, envir = envir)
}

remove_from_envir <- function(manuscript, envir){
  if(!inherits(manuscript, "character")) stop("Manuscript must be the name of a manuscript in the environment.")
  if(!exists(".revise_manuscripts", envir = envir, inherits = FALSE)) stop("No manuscripts in environment.")
  .revise_manuscripts <- get(".revise_manuscripts", envir = envir)
  if(inherits(.revise_manuscripts, "revise_corpus")){
    if(!manuscript %in% names(.revise_manuscripts)) stop("Named manuscript does not exist in environment.")
    .revise_manuscripts[[manuscript]] <- NULL
    if(length(.revise_manuscripts) == 1) .revise_manuscripts <- .revise_manuscripts[[1]]
    assign(".revise_manuscripts", .revise_manuscripts, envir = envir)
  } else {
    if(!manuscript == .revise_manuscripts$filename) stop("Named manuscript does not exist in environment.")
    rm(".revise_manuscripts", envir = envir)
  }
}
