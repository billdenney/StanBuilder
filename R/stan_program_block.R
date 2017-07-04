# stan_block Generators ####

#' Generate a stan_block object representing one of the stan program 
#' blocks
#' 
#' @param block_type The type of block to be generated.
#' @param contents The contents of the block as an unnamed list
#' @return A stan_block object with elements for "block_type" and 
#'   "contents"
#' @references Stan Development Team. Stan Modeling Language: User's 
#'   Guide and Reference Manual.  (Version 2.16.0)
#'   https://github.com/stan-dev/stan/releases/download/v2.16.0/stan-reference-2.16.0.pdf.
#'   Accessed 2 July 2017.
#' @export
stan_block <- function(block_type=c("", "functions", "data", "transformed data",
                                    "parameters", "transformed parameters", "model", "generated quantities"),
                       contents) {
  block_type <- match.arg(block_type)
  if (block_type %in% "") {
    stop("block_type must be specified")
  }
  if (any(sapply(contents,
                 FUN=function(x)
                   inherits(x, c("stan_block", "stan_program"))))) {
    stop("Stan block contents cannot be another stan_block or a stan_program")
  }
  ret <- list(block_type=block_type,
              contents=contents)
  class(ret) <- c("stan_block", "stan_model_part")
  ret
}

# stan_block Character Generator ####

#' @describeIn as.character.stan_include Generate a vector of character strings
#' @export
as.character.stan_block <- function(x, ...) {
  c(sprintf("%s {", x$block_type),
    indent(
      sapply(x$contents,
             as.character,
             code_line=TRUE),
    "}")
}
# stan_program Generators ####

#' Generate a Stan program
#' @param blocks The blocks generating the model (may include 
#'   "stan_block", "stan_comment", "stan_include", and "stan_character" 
#'   classes).
#' @return A stan_program object with an element for "blocks".
#' @export
stan_program <- function(blocks) {
  valid_block_type <-
    sapply(blocks, inherits,
           what=c("stan_block", "stan_comment", "stan_include", "stan_character"))
  if (!all(valid_block_type)) {
    stop("Invalid class passed in blocks for element number(s): ",
         paste(which(valid_block_type), collapse=", "))
  }
  ret <- list(blocks=blocks)
  class(ret) <- c("stan_program", "stan_model_part")
  ret
}

#' Sort the contents of a stan_program to be in the correct block order.
#' 
#' @param x The stand_program object
#' @param decreasing Must be FALSE
#' @param ... Ignored.
#' @details stan_model_parts that are allowed outside of a block (such 
#'   as include or comments) are sorted as follows: A preamble is 
#'   defined as anything from the beginning of the stan_program to the 
#'   first block if there is either an include or a blank line before 
#'   the first block.  If there is no include or blank line, then the 
#'   beginning of the file is considered a comment attached to the first
#'   block.  Comments, includes, and blank lines above a block are 
#'   attached to the block for sorting. Anything after the last block is
#'   considered a post-script and is kept at the bottom of the
#'   stan_program.
#' @return A stan_program with the blocks sorted in the correct order.
sort.stan_program <- function(x, decreasing=FALSE, ...) {
  extract_preamble <- function(object) {
    lastidx <- 0
    for (i in seq_along(object)) {
      if (identical(as.character(object[[i]]), "")) {
        # A blank line ends searching
        lastidx <- i
        break
      } else if (inherits(x, "stan_include")) {
        # An include indicates that the preamble exists, but there could
        # be multiple includes or a blank line that follows.
        lastidx <- i
      }
    }
    if (lastidx) {
      list(object[1:lastidx], object[(lastidx+1):length(object)])
    } else {
      list(list(), object)
    }
  }
  extract_postscript <- function(object) {
    firstidx <- 0
    for (i in rev(seq_along(object))) {
      if (inherits(object[[i]], "stan_block")) {
        break
      }
      firstidx <- i
    }
    if (firstidx) {
      list(object[1:firstidx], object[(firstidx + 1):length(object)])
    } else {
      list(object, list())
    }
  }
  extract_blocks <- function(object) {
    ret <- list()
    accumulating <- list()
    block_names <- c()
    for (i in seq_along(object)) {
      accumulating <- append(accumulating, list(object[[i]]))
      if (inherits(object[[i]], "stan_block")) {
        ret <- append(ret, accumulating)
        if (object[[i]]$block_type %in% block_names) {
          stop("Duplicate block_type in stan_program")
        }
        block_names <- c(block_names, object[[i]]$block_type)
        accumulating <- list()
      }
    }
    if (length(accumulating)) {
      stop("Unknown information after the last block.")
    }
    names(ret) <- block_names
    ret
  }
  x_pre_obj <- extract_preamble(x$blocks)
  x_obj_post <- extract_postscript(x_pre_obj[[2]])
  ret <- x_pre_obj[[1]]
  postscript <- x_obj_post[[2]]
  x_obj <- extract_blocks(x_obj_post[[1]])
  blockorder <- c("functions", "data", "transformed data",
                  "parameters", "transformed parameters",
                  "model", "generated quantities")
  mask_unknown_name <- !(names(x_obj) %in% blockorder)
  if (any(mask_unknown_name)) {
    stop("Unexpected block name(s) in stan_program: ",
         paste(names(x_obj)[mask_unknown_name], collapse=", "))
  }
  for (n in blockorder) {
    if (n %in% names(x_obj)) {
      ret <- append(ret, x_obj[n])
    }
  }
  ret <- append(ret, x_obj_post[2])
  ret <- unlist(ret, recursive=FALSE, use.names=FALSE)
  if (is.null(ret)) {
    ret <- list()
  }
  ret <- list(blocks=ret)
  class(ret) <- c("stan_program", "stan_model_part")
  ret
}

# stan_program Character Generator ####

#' @describeIn as.character.stan_include Generate a vector of character strings
#' @export
as.character.stan_program <- function(x, ...) {
  sapply(sort.stan_program(x),
         as.character,
         code_line=TRUE)
}
