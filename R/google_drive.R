#' drive_comments
#'
#' Extract comments from a google docs document
#' @param url URL to drive document
#' @param email drive email account. Can be set with options(gargle_oauth_email)
#' @param cache_credentials bool. Should cached credentials be used? Can be set with options(gargle_oauth_cache)
#' @return A list with a length equal to the number of comments. Each element corresponds to one comment: the element's name is the comment text, and its value is the text that was bounded by the comment.
#' @export

drive_comments <- function(url, email = NULL, cache_credentials = NULL) {
  if (!requireNamespace("googledrive", quietly = TRUE)) return(NULL)

  auth <- Filter(Negate(is.null), list(email = email, cache = cache_credentials))
  do.call(googledrive::drive_auth, auth)
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path))
  suppressMessages(googledrive::drive_download(googledrive::as_id(url), path = path))
  get_docx_comments(officer::read_docx(path), markdown = FALSE)
}
