#' @title Mask Text for Blind Review
#' @description The YAML front matter tag `mask` determines whether
#' the text is displayed or replaced by `mask_message`.
#' The YAML tag can take the values `yes` or `no`.
#' @param x Atomic character vector to be masked.
#' @param mask_message Atomic character vector to mask `x` with when
#' `mask: yes`, Default: 'masked for blind review'
#' @param hide Logical, Default: rmarkdown::metadata$mask
#' @return Character vector.
#' @examples
#' mask("this is masked", hide = TRUE)
#' @seealso
#'  \code{\link[rmarkdown]{metadata}}
#' @rdname mask
#' @export
#' @importFrom rmarkdown metadata
mask <- function(x, mask_message = "masked for blind review", hide = rmarkdown::metadata$mask){
  UseMethod("mask", x)
}

#' @method mask default
#' @export
mask.default <- function(x, mask_message = "masked for blind review", hide = rmarkdown::metadata$mask){
  if(is.null(hide)){
    message("Variable 'mask' not found in YAML front matter.")
    hide <- FALSE
  }
  if(hide){
    return(mask_message)
  } else {
    return(x)
  }
}
