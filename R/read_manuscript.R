#' read_manuscript
#'
#' Reads in rmarkdown manuscript and an associated PDF as possible
#' @param address path to a rmarkdown file
#' @param PDF if TRUE, or path provided, a PDF will be loaded for page matching.
#' @param to_envir Logical, indicating whether or not the manuscript should
#' be assigned to an invisible environment variable (`.revise_manuscript`).
#' Defaults to `TRUE`.
#' @param envir The environment to which the manuscript should be assigned.
#' @return Invisibly returns a list of class `manuscript`, containing
#' the following elements:
#'
#' * $sections: Extracted revisions
#' * $PDF: Optionally, the PDF document
#' * $refs: References to figures and tables
#' * $rmd: The raw rmarkdown
#' * $mtime: When the rmarkdown file was created
#' @examples
#' tempfile <- tempfile("tmp", fileext = ".txt")
#' writeLines("[Maecenas mollis consectetur purus.]{#test}", con = tempfile)
#' read_manuscript(tempfile)
#' @seealso
#'  \code{\link[rmarkdown]{yaml_front_matter}}
#'  \code{\link[tools]{fileutils}}
#' @rdname read_manuscript
#' @export
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom tools file_ext
read_manuscript <- function(address, PDF = FALSE, to_envir = TRUE, envir = parent.frame(1)){
  doc_checksum <- tools::md5sum(address)
  if(!is_docx(address)){
    rmd <- paste0(readLines(address, encoding = "UTF8"), collapse = "\n")
    sections <- c(extract_sections(rmd),
                extract_sections(rmd, is_span = TRUE))
    refs <- extract_refs(address)
  }else{
   sections <- read_docx(address)
   rmd <- NULL
   refs <- NULL
  }
  

  check_dup_sections(names(sections), revise_errors = FALSE)
  if(!is.null(PDF)) {
    if (PDF == TRUE) {

      floats_in_text <-
        tryCatch({
          rmarkdown::yaml_front_matter(address)$floatsintext
        }, error = function(e) return(FALSE))

      if(is.null(floats_in_text)) floats_in_text <- FALSE

      if(floats_in_text){
        warning("floatsintext is set to TRUE in target rmarkdown. This may disrupt page number matching where target strings cross pages.")
      }

      PDF <- gsub(tools::file_ext(address), "pdf", address)
    }
    if (inherits(PDF, "character")) {
      PDF <- process_pdf(PDF)
    } else{
      PDF <- NULL
    }
  } else{
    PDF <- NULL
  }
  
  # Prepare output object
  manuscript <- list(sections = sections,
                     PDF = PDF,
                     refs = refs,
                     rmd = rmd,
                     mtime = file.info(address)$mtime,
                     checksum = doc_checksum,
                     filename = basename(address))
  class(manuscript) <- c("revise_manuscript", class(manuscript))
  # Load to environment
  if (to_envir) {
    add_to_envir(manuscript = manuscript, envir = envir)
  }
  # Return
  return(invisible(manuscript))
}

add_to_envir <- function(manuscript, envir){
  if(exists(".revise_manuscripts", envir = envir)){
    .revise_manuscripts <- get(".revise_manuscripts", envir = envir)
    # Check that this doc is not yet in .revise_manuscripts
    if(!isTRUE(checksum_exists(.revise_manuscripts, manuscript[["checksum"]]))){
      newname <- basename(manuscript[["filename"]])
      if(inherits(.revise_manuscripts, "revise_manuscript")){
        manuscript <- list(.revise_manuscripts, manuscript)
        names(manuscript) <- c(basename(.revise_manuscripts$filename),
                               newname)
        class(manuscript) <- c("revise_corpus", class(manuscript))
      } else {
        manuscript <- c(.revise_manuscripts, list(manuscript))
        names(manuscript)[length(manuscript)] <- newname
      }
    }
  }
  assign(".revise_manuscripts", manuscript, envir = envir)
}

remove_from_envir <- function(manuscript, envir){
  if(!inherits(manuscript, "character")) stop("Manuscript must be the name of a manuscript in the environment.")
  if(!exists(".revise_manuscripts", envir = envir)) stop("No manuscripts in environment.")
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
