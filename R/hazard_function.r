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
#' @examples
#' # Define a probability density function
#' exponential_pdf <- function(t, lambda) {
#'   lambda * exp(-lambda * t)
#' }
#'
#' # Create a hazard function based on the exponential_pdf
#' exponential_hazard <- hazard_function(exponential_pdf)
#'
#' # Calculate the hazard at time 1
#' exponential_hazard(1, lambda = 0.5)
#'
#' @importFrom assertthat assert_that
#'
#' @export
hazard_function <- function(pdf) {
  f <- function(t, ...) {
    assertthat::assert_that(
      t > 0,
      msg = "t > 0. In this tool, the density function is used to model survival time."
    )
    pdf(t, ...) / survival_function(pdf)(t, ...)
  }

  f_vec <- Vectorize(FUN = f, vectorize.args = "t")
  f_class <- function(time, ...) {
    result <- f_vec(time, ...)
    attr(result, "time") <- time
    class(result) <- "hazard_function"
    result
  }
  f_class
}
