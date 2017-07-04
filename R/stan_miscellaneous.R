# stan_* Miscellaneous Generators ####

#' Generate a Stan include statement
#' 
#' @param filename One or more filenames to be included
#' @return A stan_include object with an element for "filename"
#' @export
stan_include <- function(filename) {
  if (!is.character(filename)) {
    stop("filename must be a character")
  }
  ret <- list(filename=filename)
  class(ret) <- c("stan_include", "stan_model_part")
  ret
}

#' Generate a stan_comment object
#' 
#' @param comment The one-or-more lined comment.  It will be converted
#'   to a character if not already one, and it will be split on carriage
#'   returns.
#' @param multiline Should the comment be handled as a multi-line comment?
#' @return A stan_comment object with elements for "comment" and "multiline"
#' @export
stan_comment <- function(comment, multiline=FALSE) {
  if (!is.character(comment)) {
    comment <- as.character(comment)
  }
  ret <- list(comment=comment,
              multiline=multiline)
  class(ret) <- c("stan_comment", "stan_model_part")
  ret
}

#' Generate a stan_character object
#' 
#' @param text A scalar or vector of character strings.  Each element
#'   should represent one statement (often one line of code).
#' @details stan_character objects are intended to be used when none of 
#'   the other functions can generate the correct code for use.  The 
#'   primary actual use is to enable the use of the specialized 
#'   \code{as.character} function that generates semicolons at the end 
#'   of lines correctly.
#' @return A stan_character object with an element for "text".
#' @export
stan_character <- function(text) {
  if (!is.character(text)) {
    text <- as.character(text)
  }
  ret <- list(text=text)
  class(ret) <- c("stan_character", "stan_model_part")
  ret
}

# stan_* Miscellaneous Character Generators ####

#' Coerce a stan_* into a character string
#' 
#' @param x The stan_* object
#' @param ... Ignored
#' @param code_line Should the output be considered one line of code per
#'   row (adding semicolons at the end)?
#' @return A character vector (may have more than one element)
#' @export
as.character.stan_include <- function(x, ...) {
  sprintf("#include %s", x$filename)
}

#' @describeIn as.character.stan_include Generate a scalar (single or
#'   multiline) or vector (single line)
#' @export
as.character.stan_comment <- function(x, ...) {
  if (x$multiline) {
    ret <- sprintf("/*\n%s\n*/",
                   paste(x$comment, collapse="\n"))
  } else {
    ret <- sprintf("// %s", x$comment)
  }
  ret
}

#' @describeIn as.character.stan_include Generate a vector of statements
#' @export
as.character.stan_character <- function(x, ..., code_line=FALSE) {
  make_code_line(x$text, code_line=code_line)
}

# stan_* Miscellaneous functions ####

length.stan_character <- function(x) {
  length(x$text)
}

length.stan_include <- function(x) {
  length(x$filename)
}

length.stan_comment <- function(x) {
  if (x$multiline) {
    1
  } else {
    length(x$comment)
  }
}
