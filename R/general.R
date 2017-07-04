#' Add a semicolon if needed
#' 
#' @param x A character vector
#' @param code_line Should the semicolon be added to the end of each
#'   element of \code{x}?
#' @return \code{x} maybe with added semicolons.
make_code_line <- function(x, code_line) {
  if (code_line) {
    x <- sprintf("%s;", x)
  }
  x
}
