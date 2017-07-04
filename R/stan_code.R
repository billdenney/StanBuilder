# stan_code Generators ####

#' Generate a Stan for loop
#' 
#' @param loop_var The looping variable name (e.g. "n")
#' @param loop_bounds A named list with elements "lower" and "upper". 
#'   The elements will be made into stan_literal_int if numeric or will 
#'   be surrounded by parentheses if characters.
#' @param contents The statements making up the loop body as a list.
#' @return A stan_for_loop object with elements for "loop_var", "lower",
#'   "upper", and "contents".
#' @export
stan_for_loop <- function(loop_var, loop_bounds, contents) {
  if (!inherits(loop_var, c("character", "stan_character")) |
      length(loop_var) != 1) {
    stop("loop_var must be a character scalar")
  }
  if (!is.list(contents)) {
    stop("contents must be a list")
  } else if (length(contents) == 0) {
    stop("contents must have a nonzero length")
  }
  if (!is.list(loop_bounds)) {
    stop("loop_bounds must be a named list with elements named 'lower' and 'upper'")
  }
  lower <- loop_bounds$lower
  upper <- loop_bounds$upper
  if (is.numeric(lower)) {
    lower <- stan_literal_int(x=lower)
  } else if (!is.character(lower)) {
    stop("loop_bounds$lower must be either be a number or a character string.")
  }
  if (is.numeric(upper)) {
    upper <- stan_literal_int(x=upper)
  } else if (!is.character(upper)) {
    stop("loop_bounds$upper must be either be a number or a character string.")
  }
  ret <- list(loop_var=loop_var,
              lower=lower,
              upper=upper,
              contents=contents)
  class(ret) <- c("stan_for_loop", "stan_code", "stan_model_part")
  ret
}

#' Generate a Stan while loop
#' 
#' @param condition The logical condition that must be true for the loop
#'   to continue (as a character scalar).
#' @param contents The statements making up the loop body as a list.
#' @return A stan_while_loop object with elements for "condition" and
#'   "contents".
#' @export
stan_while_loop <- function(condition, contents) {
  if (!is.character(condition) |
      length(condition) != 1) {
    stop("condition must be a character scalar")
  }
  if (!is.list(contents)) {
    stop("contents must be a list")
  } else if (length(contents) == 0) {
    stop("contents must have a nonzero length")
  }
  ret <- list(condition=condition,
              contents=contents)
  class(ret) <- c("stan_while_loop", "stan_code", "stan_model_part")
  ret
}

#' Generate a Stan if statement (a component of an if block)
#' 
#' @param type The type of if statement; one of "if", "else if", and 
#'   "else".
#' @param condition The condition to be satisfied (must be missing for 
#'   \code{type == "else"})
#' @param contents The contents of the statement (the code to be
#'   executed when the condition is true).
#' @seealso \code{\link{stan_if_block}}
#' @export
stan_if_statement <- function(type, condition, contents) {
  type <- match.arg(type, choices=c("if", "else if", "else"))
  if (type %in% "else") {
    if (missing(condition)) {
      condition <- NULL
    } else if (is.null(condition)) {
      # do nothing, NULL is the only acceptable condition for else
    } else {
      stop("A condition may not be given for an else statement (did you mean 'else if'?)")
    }
  } else if (!is.character(condition) |
             length(condition) != 1) {
    stop("condition must be a character scalar")
  }
  if (!is.list(contents)) {
    stop("contents must be a list")
  }
  ret <- list(type=type,
              condition=condition,
              contents=contents)
  class(ret) <- c("stan_if_statement", "stan_code", "stan_model_part")
  ret
}

#' Generator for an if block combining else if and else together
#' 
#' @param ... Block parts made of one or more "if", "else if", and
#'   "else" statements
#' @return A stan_if_block object with an element for "contents".
#' @seealso \code{\link{stan_if_statement}}
#' @export
stan_if_block <- function(...) {
  args <- list(...)
  if (length(args) == 0) {
    stop("At least one argument must be given.")
  }
  if (!all(sapply(args, FUN=function(x) inherits(x, "stan_if_statement")))) {
    stop("All arguments for a stan_if_block must be stan_if_statement objects")
  }
  if (!(args[[1]]$type %in% "if")) {
    stop("The first element of an if block must have type 'if'")
  }
  if (length(args) > 1) {
    if (any(sapply(args[2:length(args)],
                   FUN=function(x) x$type %in% "if"))) {
      stop("Only the first element of an if block may be of type 'if'")
    } else if (any(sapply(args[1:(length(args)-1)],
                          FUN=function(x) x$type %in% "else"))) {
      stop("Only the last element of an if block may be of type 'else'")
    }
    if (length(args) > 2) {
      if (!all(sapply(args[2:(length(args)-1)],
                      FUN=function(x) x$type %in% "else if"))) {
        stop("All elements of an if block between the first and last must be of type 'else if'")
      }
    }
  }
  ret <- list(contents=args)
  class(ret) <- c("stan_if_block", "stan_code", "stan_model_part")
  ret
}

# stan_code Character Generators ####

#' @describeIn as.character.stan_include Generate a vector of character strings
#' @export
as.character.stan_for_loop <- function(x, ..., code_line=TRUE) {
  lower <- x$lower
  upper <- x$upper
  if (inherits(lower, "character")) {
    lower <- sprintf("(%s)", lower)
  }
  if (inherits(upper, "character")) {
    upper <- sprintf("(%s)", upper)
  }
  c(sprintf("for (%s in %s:%s) {",
            x$loop_var,
            lower, upper),
    indent(
      sapply(x$contents,
             as.character,
             code_line=TRUE)),
    "}")
}

#' @describeIn as.character.stan_include Generate a vector of character strings
#' @export
as.character.stan_while_loop <- function(x, ..., code_line=TRUE) {
  c(sprintf("while (%s) {",
            as.character(x$condition, code_line=FALSE)),
    indent(
      sapply(x$contents,
             as.character,
             code_line=TRUE)),
    "}")
}

#' @describeIn as.character.stan_include Generate a vector of character strings
#' @export
as.character.stan_if_statement <- function(x, ..., code_line=TRUE) {
  if (x$type %in% "else") {
    preamble <- sprintf("%s {", x$type)
  } else {
    preamble <- sprintf("%s (%s) {",
                        x$type,
                        as.character(x$condition,
                                     code_line=FALSE))
  }
  c(preamble,
    indent(
      sapply(x$contents,
             as.character,
             code_line=TRUE)),
    "}")
}

#' @describeIn as.character.stan_include Generate a vector of character strings
#' @export
as.character.stan_if_block <- function(x, ..., code_line=TRUE) {
  blocks <- lapply(x$contents,
                   as.character,
                   code_line=TRUE)
  if (length(blocks) == 1) {
    blocks[[1]]
  } else if (length(blocks) > 1) {
    # Combine the braces from the end of one and the beginning of the
    # next statement.
    ret <- blocks[[1]]
    for (i in 2:length(blocks)) {
      ret[length(ret)] <- paste(ret[length(ret)], blocks[[i]][1])
      ret <- c(ret, blocks[[i]][-1])
    }
    ret
  }
}
