# stan_literal Generators ####

# Section 4.1 of the 2.16.0 manual

#' Generate an integer literal
#' 
#' @param x The integer to generate a literal for
#' @param allow_char Allow \code{x} to be a character that (hopefully) 
#'   represents an integer.
#' @param allow_modification Allow \code{x} to be modified when 
#'   converted to an integer.  (When \code{TRUE}, this allows floating
#'   point imprecision.)
#' @details The \code{as.integer} function in R does not allow the same 
#'   range of integers as Stan.  Therefore, integers may be specified as
#'   character strings that will be placed into the Stan program as is.
#' @return A stan_literal object with elements for "value" and "type".
#' @export
stan_literal_int <- function(x, allow_char=FALSE, allow_modification=TRUE) {
  # Check the input
  if (any(is.na(x))) {
    stop("x may not be NA.")
  } else if ((!(is.numeric(x) & !is.factor(x))) |
             allow_char & is.character(x)) {
    stop("x must be a number and not a factor or a character and allow_char must be TRUE.")
  } else if (length(x) != 1) {
    stop("x must be a scalar")
  }
  # Convert the input
  if (!(allow_char & is.character(x))) {
    x.new <- as.integer(x)
    if (is.na(x.new)) {
      stop("x became NA when coerced into an integer.")
    } else if (x.new != x) {
      # This is usually OK due to double->int coersion being close but
      # not exact.
      warning("x was changed when coercing it to an integer.")
    }
    x <- x.new
  }
  ret <- list(value=x,
              type="int")
  class(ret) <- c("stan_literal_int", "stan_literal", "stan_model_part")
  ret
}

#' Generate a real number literal
#' 
#' @param x The real number to generate a literal for
#' @param allow_char Allow \code{x} to be a character that (hopefully) 
#'   represents a real number.
#' @details The \code{as.double} function in R does not allow the same 
#'   range of reals as Stan.  Therefore, reals may be specified as 
#'   character strings that will be placed into the Stan program as is.
#' @return A stan_literal object with elements for "value" and "type".
#' @export
stan_literal_real <- function(x, allow_char=FALSE) {
  if ((!(is.numeric(x) & !is.factor(x))) |
      allow_char & is.character(x)) {
    stop("x must be a number and not a factor or a character and allow_char must be TRUE.")
  } else if (length(x) != 1) {
    stop("x must be a scalar")
  }
  ret <- list(value=x,
              type="real")
  class(ret) <- c("stan_literal_real", "stan_literal", "stan_model_part")
  ret
}

#' Generate a literal Stan vector
#' 
#' @param x The vector to make into a literal.  It may be numeric or 
#'   character (to be interpreted by Stan as another primitive 
#'   variable).
#' @param vector_type The type of literal vector (column or row).
#' @return A stan_literal_vector object with elements for "value" and 
#'   "type".
#' @export
stan_literal_vector <- function(x, vector_type=c("vector", "row_vector")) {
  vector_type <- match.arg(vector_type)
  if (!inherits(x, c("numeric", "integer", "character"))) {
    stop("x must be numeric, integer, or character")
  }
  if (length(x) < 1) {
    stop("x may not be empty.")
  }
  ret <- list(value=x,
              type=vector_type)
  class(ret) <- c("stan_literal_vector", "stan_literal", "stan_model_part")
  ret
}

#' Generate a literal Stan matrix
#' 
#' @param x A matrix or something that can be coerced into a matrix
#' @return A stan_literal_matrix object with an element for "value".
#' @export
stan_literal_matrix <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  ret <- list(value=x)
  class(ret) <- c("stan_literal_matrix", "stan_literal", "stan_model_part")
  ret
}

#' Generate a literal Stan array
#' 
#' @param x A matrix, array, or something that can be coerced into a matrix.
#' @param type The type of data to be stored in the array.
#' @return A stan_literal_array object with an element for "value" and "type".
#' @export
stan_literal_array <- function(x, type=c("int", "real")) {
  type <- match.arg(type)
  if (!inherits(x, c("matrix", "array"))) {
    x <- as.matrix(x)
  }
  ret <- list(value=x,
              type=type)
  class(ret) <- c("stan_literal_array", "stan_literal", "stan_model_part")
  ret
}

# stan_literal Character Generators ####

#' @describeIn as.character.stan_include A scalar character string for a
#'   stan_literal_int
#' @export
as.character.stan_literal_int <- function(x, ...) {
  as.character(x$value)
}

#' @describeIn as.character.stan_include A scalar character string for a
#'   stan_literal_real
#' @export
as.character.stan_literal_real <- function(x, ...) {
  ret <- as.character(x$value)
  mask_integer_look <- grepl("^-?[0-9]+$", ret)
  if (any(mask_integer_look)) {
    # The return value looks like an integer; add a ".0"
    ret[mask_integer_look] <- paste0(ret[mask_integer_look], ".0")
  }
  ret
}

#' @describeIn as.character.stan_include A scalar character string for a
#'   stan_literal_vector
#' @export
as.character.stan_literal_vector <- function(x, ...) {
  row_indicator <- c("'", "")[1+(x$type %in% "row_vector")]
  sprintf("[%s]%s", paste(x$value, collapse=", "), row_indicator)
}

#' @describeIn as.character.stan_include A scalar character string for a
#'   stan_literal_matrix
#' @export
as.character.stan_literal_matrix <- function(x, ...) {
  ret <-
    sapply(seq_len(nrow(x$value)),
           FUN=function(i) {
             as.character.stan_literal_vector(list(value=x$value[i,],
                                                   type="row_vector"))
           })
  sprintf("[%s]", paste(ret, collapse=",\n "))
}

#' @describeIn as.character.stan_include A scalar character string for a
#'   stan_literal_array
#' @export
as.character.stan_literal_array <- function(x, ...) {
  if (length(dim(x$value)) > 1) {
    ret <-
      sapply(seq_len(dim(x$value)[1]),
             FUN=function(i) {
               tmpx <- x
               tmp_args <- list(x$value)
               tmp_args <- append(tmp_args, i)
               tmp_args <- append(tmp_args, as.list(c(rep(TRUE, length(dim(x$value))-1))))
               tmpx$value <- do.call("[", tmp_args)
               as.character.stan_literal_array(tmpx)
             })
    ret <- sprintf("{ %s }", paste(ret, collapse=",\n  "))
  } else {
    if (x$type %in% "real") {
      # Ensure formatting as a real
      ret <- sapply(x$value,
                    FUN=function(y) {
                      as.character.stan_literal_real(list(value=y))
                    })
    } else if (x$type %in% "int") {
      # Ensure formatting as an int
      ret <- sapply(x$value,
                    FUN=function(y) {
                      as.character.stan_literal_int(list(value=y))
                    })
    } else {
      stop("Invalid array type")
    }
    ret <- sprintf("{ %s }",
                   paste(ret,
                         collapse=", "))
  }
  ret
}
