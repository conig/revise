#' List manuscripts in environment
#'
#' Returns a character vector with the names of manuscripts
#' loaded in a given environment.
#' @param envir The environment to which the manuscript should be assigned.
#' @return A character vector, or `NULL` if no manuscripts
#' exist in the environment.
#' @examples
#' library(revise)
#'
#' temp_file <- tempfile(fileext = ".Rmd")
#' write("`r 1 + 1", file = temp_file)
#'
#' read_manuscript(temp_file)
#' list_manuscripts()
#' @rdname list_manuscripts
#' @export
list_manuscripts <- function(envir = parent.frame(1)){
  if(!isTRUE(exists(".revise_manuscripts", where = envir))){
    return(NULL)
  }
  .revise_manuscripts <- get(".revise_manuscripts", envir = envir)
  if(inherits(.revise_manuscripts, "revise_manuscript")){
    return(basename(.revise_manuscripts[["filename"]]))
  } else {
    return(names(.revise_manuscripts))
  }
}


#' Remove manuscript from environment
#'
#' Removes a manuscript from a given environment by name.
#' @param manuscript A character vector of manuscript names
#' that exist in `envir`.
#' @param envir The environment from which to remove
#' the manuscript.
#' @return Invisibly returns `TRUE` if successful, and `FALSE`
#' otherwise.
#' @examples
#' library(revise)
#'
#' temp_file <- tempfile(fileext = ".Rmd")
#' write("`r 1 + 1", file = temp_file)
#'
#' read_manuscript(temp_file)
#' list_manuscripts()
#' remove_manuscript(list_manuscripts()[1])
#' list_manuscripts()
#'
#' @rdname remove_manuscript
#' @export
remove_manuscript <- function(manuscript = NULL, envir = parent.frame(1)){
  if(!inherits(manuscript, "character")) stop("Manuscript must be a vector of names of manuscripts in the environment.")
  for(n in manuscript){
    out <- tryCatch(remove_from_envir(n, envir), error = function(e){
      message(e)
      FALSE
    })
    if(isFALSE(out)) return(invisible(FALSE))
  }
  return(invisible(TRUE))
}
