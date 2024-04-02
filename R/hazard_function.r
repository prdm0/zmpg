#' Numerical hazard function
#'
#' This function calculates the hazard function based on a given probability density function (pdf).
#' The hazard function represents the instantaneous rate at which an event occurs at a specific time,
#' given that the event has not occurred before that time.
#'
#' @param pdf The probability density function used to model the survival time.
#' @return A function that calculates the hazard function at a given time.
#' @details The hazard function is calculated as the ratio of the probability density function
#' to the survival function. The hazard function is useful for understanding the instantaneous
#' rate of occurrence of an event at a specific time. The hazard function is also used to model
#' the instantaneous rate of occurrence of an event in survival analysis. Just like
#' the function [survival_function()], the [hazard_function()] function is
#' vectorized in \eqn{t}, which allows the calculation of the hazard function
#' for a vector of times. The hazard function is defined by:
#'
#' \eqn{h(t) = \frac{f_T(t)}{S_T(t)},}
#' where \eqn{f_T(t)} is the probability density function and \eqn{S_T(t)} is the
#' survival function at time \eqn{t}.
#'
#' @seealso [survival_function()].
#'
#'
#' @references NADARAJAH, Saralees; KOTZ, Samuel. The beta exponential distribution. Reliability engineering & system safety, v. 91, n. 6, p. 689-697, 2006.
#'
#' @examples
#' # Saraless Nadarajah and Samnuel Kotz (2006)
#' beta_exponential <- function(x, a, b, lambda){
#'  lambda / beta(a, b) * exp(-b * lambda * x) * (1 - exp(-lambda * x))^(a - 1)
#' }
#'
#' hazard_beta_exponential <- hazard_function(beta_exponential)
#'
#' hazard_beta_exponential(
#'   t = seq(0.001, 1.5, length.out = 50L),
#'   a = 1.5,
#'   b = 1.8,
#'   lambda = 1.5
#' )
#'
#' @importFrom assertthat assert_that
#'
#' @export
hazard_function <- function(pdf) {
  survival_func = survival_function(pdf)
  f <- function(t, t0 = 0, ...) {
    assertthat::assert_that(
      t > 0,
      msg = "t > 0. In this tool, the density function is used to model survival time."
    )
    #exp(log(pdf(t, ...)) - log(survival_func(t, t0, ...)))
    pdf(t, ...) / survival_func(t, t0, ...)
  }

  f_vec <- Vectorize(FUN = f, vectorize.args = "t")
  f_class <- function(t, ...) {
    result <- f_vec(t, ...)
    attr(result, "time") <- t
    class(result) <- "hazard_function"
    result
  }
  f_class
}
