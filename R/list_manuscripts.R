#' List Manuscripts in Environment
#'
#' Returns a character vector with the names of manuscripts
#' loaded in a given environment.
#' @param envir The environment in which to check for manuscripts.
#' @return A character vector, or `NULL` if no manuscripts
#' exist in the environment.
#' @examples
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


#' Remove Manuscript from Environment
#'
#' Removes a revise_manuscript from a given environment by name.
#' @param manuscript A character vector of revise_manuscript names
#' that exist in `envir`.
#' @param envir The environment from which to remove
#' the manuscript.
#' @return Invisibly returns `TRUE` if successful, and `FALSE`
#' otherwise.
#' @examples
#' temp_file <- tempfile(fileext = ".Rmd")
#' write("`r 1 + 1", file = temp_file)
#' read_manuscript(temp_file)
#' result <- remove_manuscript(list_manuscripts()[1])
#' # See that remove_manuscript() silently returns TRUE
#' print(result)
#' # See that there are no more manuscripts in envir
#' list_manuscripts()
#'
#' @rdname remove_manuscript
#' @export
remove_manuscript <- function(manuscript = NULL, envir = parent.frame(1)){
  with_cli_try("Removing manuscript {.val {manuscript}} from environment.", {
    if(!inherits(manuscript, "character")) stop()
    for(n in manuscript){
      remove_from_envir(n, envir)
    }
  })
}
