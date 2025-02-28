#' drive_comments
#'
#' Extract comments from a google docs document
#' @param url URL to drive document
#' @param email drive email account. Can be set with options(gargle_oath_email)
#' @param cache_credentials bool. Should cached credentials be used? Can be set with options(gargle_oauth_cache)
#' @export


drive_comments <- function(url, email = NULL, cache_credentials = NULL){

if(!isTRUE(requireNamespace("docxtractr", quietly = TRUE) & requireNamespace("googledrive", quietly = TRUE))){
    return(NULL)
  }

  if(!is.null(email)){
  options(gargle_oauth_email = email)
  }

  if(!is.null(cache_credentials)){
    options(gargle_oauth_cache = cache_credentials)
  }

  # Ensure authorisation is set up
  googledrive::drive_auth()

  temppath <- tempfile(fileext = ".docx")

  doc <- googledrive::as_id(url)
  doc <- suppressMessages(googledrive::drive_download(doc, path = temppath, overwrite = TRUE))

  doc <- docxtractr::read_docx(temppath)
  comments <- docxtractr::docx_extract_all_cmnts(doc, include_text = TRUE)
  comment_list <- as.list(comments$word_src)
  names(comment_list) <- comments$comment_text
  unlink(temppath)

  # Some redundant spaces are introduced
  comment_list <- lapply(comment_list, function(x){
    x <- gsub("\\s\\.",".",x)
    x <- gsub("\\]\\s\\,","],",x)
    x <- gsub("\\(\\s", "(", x)
    x <- gsub("\\s\\)", ")", x)
    x
  })

  comment_list
}
