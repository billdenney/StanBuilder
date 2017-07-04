# stan_variable Generators ####

#' Generate a Stan primitive value
#' 
#' @param varname The name of the variable
#' @param type The variable type ("real" or "int")
#' @param size The length of the variable.  May be an integer or another
#'   \code{stan_primitive} varname.  If the length is a varname, the 
#'   varname will be confirmed to exist upon generating a string 
#'   representation of a full model.
#' @param range Either a vector of length 2 with a type matching 
#'   \code{type} or a list with elements named "lower" and "upper" 
#'   indicating the lower and upper bounds on the range allowed for the 
#'   value.  If a vector of length 2, \code{NA} will be considered 
#'   missing and that end of the range will not be bounded.
#' @param check_size Confirm that \code{size == 1} for a true primitive 
#'   type.  Set to \code{FALSE} when generating other types using this
#'   function.
#' @return A stan_primitive object with elements for "name", "type", and
#'   "size" and optional elements for "lower" and "upper".
#' @seealso \code{\link{stan_vector}}, \code{\link{stan_matrix}}, \code{\link{stan_array}}
#' @references Stan Development Team. Stan Modeling Language: User's 
#'   Guide and Reference Manual.  (Version 2.16.0)
#'   https://github.com/stan-dev/stan/releases/download/v2.16.0/stan-reference-2.16.0.pdf.
#'   Accessed 2 July 2017.
#' @export
stan_primitive <- function(varname, type, size=1, range=c(NA, NA), check_size=TRUE) {
  if (check_size & !identical(size, 1)) {
    stop("size must be 1 for a stan_primitive object")
  }
  ret <-
    list(name=varname,
         type=match.arg(type, choices=c("real", "int")),
         size=size)
  if (is.list(range)) {
    if (all(names(range) %in% c("lower", "upper"))) {
      ret <- append(ret, range)
    } else {
      stop("Invalid names in the range list.")
    }
  } else {
    if (all(is.na(range)) |
        is.character(range) |
        (is.numeric(range) & !is.factor(range))) {
      if (!is.na(range[1])) {
        ret$lower <- range[1]
      }
      if (!is.na(range[2])) {
        ret$upper <- range[2]
      }
    } else {
      stop("When given as a vector, range must be numeric, character, or all NA.")
    }
  }
  class(ret) <- c("stan_primitive", "stan_variable", "stan_model_part")
  ret
}

#' Generate a Stan vector
#' 
#' @param ... Arguments passed to \code{\link{stan_primitive}}
#' @param vector_type Type of vector ("vector" indicates a normal 
#'   vector)
#' @return A stan_vector object with elements for "name", "type",
#'   "vector_type", and "size" and optional elements for "lower" and
#'   "upper".
#' @seealso \code{\link{stan_primitive}}, \code{\link{stan_matrix}}, \code{\link{stan_array}}
#' @export
stan_vector <- function(...,
                        vector_type=c("vector", "simplex", "unit_vector", "ordered", "positive_ordered", "row_vector")) {
  ret <- stan_primitive(..., check_size=FALSE)
  if (!(ret$type %in% "real")) {
    stop("primitive type for a vector must be 'real'")
  }
  ret$vector_type <- match.arg(vector_type)
  class(ret) <- c("stan_vector", "stan_variable", "stan_model_part")
  ret
}

#' Generate a Stan matrix
#' 
#' @param ... Arguments passed to \code{\link{stan_primitive}}
#' @details \code{size} indicates the matrix dimensions and  must either
#'   a 2-long vector with no NA values or a scalar if the 
#'   \code{matrix_type} is square.  If the matrix type is square the 
#'   values in size must match each other.
#' @return A stan_matrix object with elements for "name", "type",
#'   "matrix_type", and "size" and optional elements for "lower" and
#'   "upper".
#' @seealso \code{\link{stan_primitive}}, \code{\link{stan_vector}}, \code{\link{stan_array}}
#' @export
stan_matrix <- function(..., matrix_type=c("matrix", "corr_matrix", "cholesky_factor_corr", "cov_matrix", "cholesky_factor_cov")) {
  square_matrix_types <- c("corr_matrix", "cholesky_factor_corr", "cov_matrix", "cholesky_factor_cov")
  matrix_type <- match.arg(matrix_type)
  ret <- stan_primitive(..., check_size=FALSE)
  if (!(ret$type %in% "real")) {
    stop("primitive type for a vector must be 'real'")
  }
  if (any(is.na(ret$size))) {
    stop("size may not be NA")
  }
  if (matrix_type %in% square_matrix_types) {
    if (length(ret$size) == 1) {
      # do nothing
    } else if (length(ret$size) == 2) {
      if (ret$size[1] == ret$size[2]) {
        ret$size <- ret$size[1]
      } else {
        stop("Both size elements must match when defining a square matrix.")
      }
    }
  } else if (length(ret$size) != 2) {
    stop("matrix size must specify rows and columns")
  }
  ret$dims 
  ret$vector_type <- match.arg(vector_type)
  class(ret) <- c("stan_matrix", "stan_variable", "stan_model_part")
  ret
}

#' Generate a Stan array
#' 
#' @param object The object ("stan_primitive", "stan_vector", or 
#'   "stan_matrix") to generate an array of.
#' @param size The size of the array.
#' @return A stan_array object with elements for "object" and "size".
#' @seealso \code{\link{stan_primitive}}, \code{\link{stan_vector}},
#'   \code{\link{stan_matrix}}
#' @export
stan_array <- function(object, size) {
  if (!inherits(object, c("stan_primitive", "stan_vector", "stan_matrix"))) {
    stop("A stan_array must be based on a stan_primitive, stan_vector, or stan_matrix object.")
  }
  ret <- list(object=object,
              size=size)
  class(ret) <- c("stan_array", "stan_variable", "stan_model_part")
  ret
}

# stan_variable Character Generators ####

#' Generate bounds given a stan_variable object that may have "lower" 
#' and "upper" bounds.
#' 
#' @param object A stan_variable object
#' @return The character representation of the bounds ready for
#'   inclusion in another character representation.
bounds_to_character <- function(object) {
  lower <- ""
  upper <- ""
  if ("lower" %in% names(object) &&
      !is.na(object$lower)) {
    lower <- sprintf("lower=%s", as.character(object$lower))
  }
  if ("upper" %in% names(object) &&
      !is.na(object$upper)) {
    upper <- sprintf("upper=%s", as.character(object$upper))
  }
  if (nchar(lower) == 0 & nchar(upper) == 0) {
    ""
  } else if (nchar(lower) & nchar(upper)) {
    sprintf("<%s,%s>", lower, upper)
  } else if (nchar(lower)) {
    sprintf("<%s>", lower)
  } else if (nchar(upper)) {
    sprintf("<%s>", upper)
  }
}

#' @describeIn as.character.stan_include A scalar character string for a
#'   stan_primitive
#' @export
as.character.stan_primitive <- function(x, ..., code_line=FALSE) {
  if (x$size != 1) {
    stop("Invalid stan_primitive object; size must be 1.")
  }
  make_code_line(
    sprintf("%s%s %s",
            x$type, bounds_to_character(x), x$name),
    code_line=code_line)
}

#' @describeIn as.character.stan_include A scalar character string for a
#'   stan_vector
#' @export
as.character.stan_vector <- function(x, ..., code_line=FALSE) {
  make_code_line(
    sprintf("%s%s[%s] %s",
            x$vector_type, bounds_to_character(x), as.character(x$size), x$name),
    code_line=code_line)
}

#' @describeIn as.character.stan_include A scalar character string for a stan_matrix
#' @export
as.character.stan_matrix <- function(x, ..., code_line=FALSE) {
  matrix_size <- paste(as.character(x$size), collapse=",")
  make_code_line(
    sprintf("%s%s[%s] %s",
            x$matrix_type, bounds_to_character(x), matrix_size, x$name),
    code_line=code_line)
}

#' @describeIn as.character.stan_include A scalar character string for a stan_array
#' @export
as.character.stan_array <- function(x, ..., code_line=FALSE) {
  # The base object for the array should never be a code_line since the
  # array size must come after it.
  object_char <- as.character(x$object, code_line=FALSE)
  array_size <- paste(as.character(x$size), collapse=",")
  make_code_line(
    sprintf("%s[%s]",
            object_char, array_size),
    code_line=code_line)
}
