# stan_model_part Printer ####

#' Print part (or all) of a stan model to the screen
#' 
#' @param x The component to print
#' @param ... Ignored
#' @return x (invisibly)
#' @export
print.stan_model_part <- function(x, ...) {
  cat(sprintf("%s:\n%s\n",
              class(x)[1],
              paste(as.character(x), collapse="\n")))
  invisible(x)
}

#' Indent code
#' 
#' @param x The character vector to indent
#' @param spaces The number of spaces to indent
#' @return Text indented with \code{spaces} before each element of
#'   \code{x} and after every carriage return within \code{x}.
indent <- function(x, spaces=2) {
  indentation <- sprintf(paste0("%", spaces, "s"), "")
  x <- paste0(indentation, x)
  x <- gsub("\n", paste0("\n", indentation), x)
  x
}
