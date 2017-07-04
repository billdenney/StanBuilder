# stan_sampling Generators ####

#' Generate a Stan sampling statement
#' 
#' @param varname The variable name for the left-hand side of the 
#'   sampling.
#' @param distribution_name The probability distribution name for the 
#'   sampling.
#' @param ... Additional arguments to the distribution.  These are 
#'   typically the measure of central tendency and variability.  They 
#'   must be convertible to characters for final output using 
#'   \code{as.character}.
#' @param lower,upper A lower and upper bound for truncation of the 
#'   sampling.  When calling a specific sampling function, the sampling 
#'   function can be modified by (or wrapped in a call to
#'   \code{truncate}) to access the same effect.
#' @details For interpretation and descriptions of each of the 
#'   parameters, please see the Stan manual.
#' @return A stan_sampling object with elements for "varname", 
#'   "distribution_name", and "args".
#' @seealso \code{\link{truncate}}
#' @export
stan_sampling <- function(varname, distribution_name, ..., lower=NA, upper=NA) {
  if (!is.character(varname) ||
      length(varname) != 1) {
    stop("varname must be a character scalar")
  } else if (!is.character(distribution_name) ||
             length(distribution_name) != 1) {
    stop("distribution_name mut be a character scalar")
  }
  lower <- NA
  upper <- NA
  args <- list(...)
  if ("lower" %in% names(args)) {
    lower <- args$lower
    args$lower <- NULL
  }
  if ("upper" %in% names(args)) {
    upper <- args$upper
    args$upper <- NULL
  }
  ret <- list(varname=varname,
              distribution_name=distribution_name,
              lower=lower,
              upper=upper,
              args=args)
  class(ret) <- c("stan_sampling", "stan_model_part")
  ret
}

#' Add truncation to a stan_sampling object
#' 
#' @param object A stan_sampling object to truncate (or remove trucation
#'   from)
#' @param lower,upper The lower and upper bound of the truncation (set
#'   to NA to remove the bound)
#' @return A stan_sampling object with updated truncation
#' @seealso \code{\link{stan_sampling}}
#' @export
truncate <- function(object, lower, upper) {
  if (!missing(lower)) {
    object$lower <- lower
  }
  if (!missing(upper)) {
    object$upper <- upper
  }
  object
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_bernoulli <- function(varname, theta) {
  stan_sampling(varname=varname, distribution_name="bernoulli", theta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_bernoulli_logit <- function(varname, alpha) {
  stan_sampling(varname=varname, distribution_name="bernoulli_logit", alpha)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_binomial <- function(varname, N, theta) {
  stan_sampling(varname=varname, distribution_name="binomial", N, theta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_binomial_logit <- function(varname, N, alpha) {
  stan_sampling(varname=varname, distribution_name="binomial_logit", N, alpha)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_beta_binomial <- function(varname, alpha, beta) {
  stan_sampling(varname=varname, distribution_name="beta_binomial", alpha, beta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_hypergeometric <- function(varname, a, b) {
  stan_sampling(varname=varname, distribution_name="hypergeometric", a, b)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_categorical <- function(varname, theta) {
  stan_sampling(varname=varname, distribution_name="categorical", theta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_categorical_logit <- function(varname, beta) {
  stan_sampling(varname=varname, distribution_name="categorical_logit", beta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_ordered_logistic <- function(varname, eta, c) {
  stan_sampling(varname=varname, distribution_name="ordered_logistic", eta, c)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_negative_binomial <- function(varname, alpha, beta) {
  stan_sampling(varname=varname, distribution_name="neg_binomial", alpha, beta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_negative_binomial_2 <- function(varname, mu, phi) {
  stan_sampling(varname=varname, distribution_name="neg_binomial_2", mu, phi)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_negative_binomial_2_log <- function(varname, eta, phi) {
  stan_sampling(varname=varname, distribution_name="neg_binomial_2_log", eta, phi)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_poisson <- function(varname, lambda) {
  stan_sampling(varname=varname, distribution_name="poisson", lambda)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_poisson_log <- function(varname, alpha) {
  stan_sampling(varname=varname, distribution_name="poisson_log", alpha)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_discrete_multivariate_multinomial <- function(varname, theta) {
  stan_sampling(varname=varname, distribution_name="multinomial", theta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_normal <- function(varname, mu, sigma) {
  stan_sampling(varname=varname, distribution_name="normal", mu, sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_exp_mod_normal <- function(varname, mu, sigma, lambda) {
  stan_sampling(varname=varname, distribution_name="exp_mod_normal", mu, sigma, lambda)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_skew_normal <- function(varname, xi, omega, alpha) {
  stan_sampling(varname=varname, distribution_name="skew_normal", xi, omega, alpha)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_student_t <- function(varname, nu, mu, sigma) {
  stan_sampling(varname=varname, distribution_name="student_t", nu, mu, sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_cauchy <- function(varname, mu, sigma) {
  stan_sampling(varname=varname, distribution_name="cauchy", mu, sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_double_exponential <- function(varname, mu, sigma) {
  stan_sampling(varname=varname, distribution_name="double_exponential", mu, sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_logisitic <- function(varname, mu, sigma) {
  stan_sampling(varname=varname, distribution_name="logisitic", mu, sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_gumbel <- function(varname, mu, beta) {
  stan_sampling(varname=varname, distribution_name="gumbel", mu, beta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_lognormal <- function(varname, mu, sigma) {
  stan_sampling(varname=varname, distribution_name="lognormal", mu, sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_inverse_chi_square <- function(varname, nu) {
  stan_sampling(varname=varname, distribution_name="inv_chi_square", nu)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_scaled_inverse_chi_square <- function(varname, nu, sigma) {
  stan_sampling(varname=varname, distribution_name="scaled_inv_chi_square", nu, sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_exponential <- function(varname, beta) {
  stan_sampling(varname=varname, distribution_name="exponential", beta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_gamma <- function(varname, alpha, beta) {
  stan_sampling(varname=varname, distribution_name="gamma", alpha, beta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_inverse_gamma <- function(varname, alpha, beta) {
  stan_sampling(varname=varname, distribution_name="inv_gamma", alpha, beta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_weibull <- function(varname, alpha, sigma) {
  stan_sampling(varname=varname, distribution_name="weibull", alpha, sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_frechet <- function(varname, alpha, sigma) {
  stan_sampling(varname=varname, distribution_name="frechet", alpha, sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_rayleigh <- function(varname, sigma) {
  stan_sampling(varname=varname, distribution_name="rayleigh", sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_wiener <- function(varname, tau, beta, delta) {
  stan_sampling(varname=varname, distribution_name="wiener", tau, beta, delta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_pareto <- function(varname, y_min, alpha) {
  stan_sampling(varname=varname, distribution_name="pareto", y_min, alpha)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_pareto_type_2 <- function(varname, mu, lambda, alpha) {
  stan_sampling(varname=varname, distribution_name="pareto_type_2", mu, lambda, alpha)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_beta <- function(varname, alpha, beta) {
  stan_sampling(varname=varname, distribution_name="beta", alpha, beta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_von_mises <- function(varname, mu, kappa) {
  stan_sampling(varname=varname, distribution_name="von_mises", mu, kappa)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_uniform <- function(varname, alpha, beta) {
  stan_sampling(varname=varname, distribution_name="uniform", alpha, beta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_multivariate_multi_normal <- function(varname, mu, Sigma) {
  stan_sampling(varname=varname, distribution_name="multi_normal", mu, Sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_multivariate_multi_normal_precision <- function(varname, mu, Omega) {
  stan_sampling(varname=varname, distribution_name="multi_normal_prec", mu, Omega)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_multivariate_multi_normal_cholesky <- function(varname, mu, L) {
  stan_sampling(varname=varname, distribution_name="multi_normal_prec", mu, L)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_multivariate_multi_gaussian_process <- function(varname, Sigma, w) {
  stan_sampling(varname=varname, distribution_name="multi_gp", Sigma, w)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_multivariate_multi_gaussian_process_cholesky <- function(varname, L, w) {
  stan_sampling(varname=varname, distribution_name="multi_gp_cholesky", L, w)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_multivariate_multi_student_t <- function(varname, nu, mu, Sigma) {
  stan_sampling(varname=varname, distribution_name="multi_student_t", nu, mu, Sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_multivariate_gaussian_dynamic_linear_model <- function(varname, F, G, V, W, m0, C0) {
  stan_sampling(varname=varname, distribution_name="gaussian_dlm_obs", F, G, V, W, m0, C0)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_continuous_dirichlet <- function(varname, alpha) {
  stan_sampling(varname=varname, distribution_name="dirichlet", alpha)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_correlation_lkj <- function(varname, eta) {
  stan_sampling(varname=varname, distribution_name="lkj_corr", eta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_correlation_lkj_cholesky <- function(varname, eta) {
  stan_sampling(varname=varname, distribution_name="lkj_corr_cholesky", eta)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_covariance_wishart <- function(varname, nu, Sigma) {
  stan_sampling(varname=varname, distribution_name="wishart", nu, Sigma)
}

#' @describeIn stan_sampling
#' @export
stan_sampling_covariance_inverse_wishart <- function(varname, nu, Sigma) {
  stan_sampling(varname=varname, distribution_name="inv_wishart", nu, Sigma)
}

# stan_sampling Character Generators ####

#' Generate the character string for truncation of a sampling statement.
#' 
#' @param x An object with optional names of "lower" and "upper" 
#'   corresponding to the lower and upper truncation bounds.
#' @details \code{NA} will be treated as a missing bound. \code{Inf} in 
#'   the correct direction will also be treated as a missing bound 
#'   (\code{-Inf} for "lower" or \code{Inf} for "upper").
#' @return A character string scalar suitable for inclusion at the end of a
#'   Stan sampling statement.
truncation_to_character <- function(x) {
  lower <- ""
  upper <- ""
  if ("lower" %in% names(x) &&
      !(is.na(x$lower) |
        (is.infinite(x$lower) & x < 0))) {
    if (is.infinite(x$lower)) {
      stop("The lower bound for truncation may not be positive infinity")
    }
    lower <- as.character(x$lower)
  }
  if ("upper" %in% names(x) &&
      !(is.na(x$upper) |
        (is.infinite(x$upper) & x > 0))) {
    if (is.infinite(x$upper)) {
      stop("The upper bound for truncation may not be negative infinity")
    }
    upper <- as.character(x$upper)
  }
  if (nchar(lower) | nchar(upper)) {
    sprintf(" T[%s, %s]", lower, upper)
  } else {
    ""
  }
}

#' @describeIn as.character.stan_include Generate a scalar character string
#' @export
as.character.stan_sampling <- function(x, ..., code_line=TRUE) {
  args <- sapply(x$args, FUN=as.character)
  args <- paste(args, collapse=", ")
  make_code_line(
    sprintf("%s ~ %s(%s)%s",
            x$varname, x$distribution_name, args,
            truncation_to_character(x)),
    code_line=code_line)
}
