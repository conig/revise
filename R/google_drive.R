#' Read Comments from Google Docs
#'
#' Download a Google Docs document as Word (.docx) and extract its comments.
#' Unlike [read_manuscript()], this function does not filter for `Revise::` tags.
#' @param url URL of the Google Docs document.
#' @param email Google account email, passed to [googledrive::drive_auth()].
#'   When omitted, use the authentication default.
#' @param cache_credentials Whether to use cached credentials, passed as
#'   `cache` to [googledrive::drive_auth()] when supplied.
#' @return A list named by comment text, with the selected manuscript text as
#'   each value. Returns `NULL` if the optional googledrive package is not installed.
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
