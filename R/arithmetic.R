# Arithmetic ####

Ops.stan_program <- function(e1, e2) {
  if (.Generic == "+") {
    if (inherits(e2, "stan_program")) {
      e1$blocks <- append(e1$blocks, e2$blocks)
    } else if (inherits(e2, c("stan_block", "stan_comment", "stan_include", "character"))) {
      e1$blocks <- append(e1$blocks, list(e2))
    } else if (inherits(e2, "list")) {
      e1$blocks <- append(e1$blocks, e2)
    } else {
      stop("Cannot add e2 to the stan_program")
    }
    e1
  } else {
    stop(sprintf("%s is not implemented for %s", .Generic, class(e1)[1]))
  }
}

Ops.stan_block <- function(e1, e2) {
  if (.Generic == "+") {
    if (inherits(e2, "stan_model_part", "character")) {
      e1$contents <- append(e1$contents, list(e2))
    } else if (inherits(e2, "list")) {
      e1$contents <- append(e1$contents, e2)
    } else {
      stop("Cannot add e2 to the stan_block")
    }
    e1
  } else {
    stop(sprintf("%s is not implemented for %s", .Generic, class(e1)[1]))
  }
}
