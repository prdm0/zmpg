#' cdf_function
#'
#' This function creates a cumulative distribution function (CDF) based on a given probability density function (PDF).
#' The CDF is defined as the integral of the PDF from 0 to a given value of t.
#'
#' @param pdf The probability density function.
#' @details [cdf_function()] returns a cumulative distribution function (CDF)
#' obtained numerically from a probability density function (PDF).
#' The idea of this function is that the user does not need to integrate a probability
#' density function and can easily obtain the cumulative distribution function for their
#' experiments, simply by passing the probability density function as an argument,
#' thus significantly reducing the amount of code.
#'
#' From a computational performance standpoint, in some situations, it might make sense
#' to implement the analytical form of the CDF. However, the numerical approach is
#' more general and can be applied to any probability density function without much effort.
#'
#' @return The cumulative distribution function in terms of time t and the
#' parameters that index the probability density function passed as an argument
#' to pdf. The function, returned by [cdf_function()], is vectorized in relation
#' to `t`.
#'
#' When using the [cdf_function()], utilizing the concept of closure, a function
#' is returned in terms of `t` and the parameters that index the PDF passed as
#' an argument to pdf. When using the function returned by [cdf_function()], the
#' cumulative probabilities in relation to time `t`, i.e., \eqn{P(T \leq t)} are returned.
#' In addition, the function carries the time attribute which is the time `t`
#' used to calculate the cumulative probability and can be used to plot the
#' cumulative distribution function. The class of the object returned by using
#' the function that is returned by [cdf_function()] is `cdf_function`.
#'
#' @examples
#' # Create a CDF based on a PDF
#' cdf <- cdf_function(pdf = dweibull)
#'
#' # Evaluate the CDF at a specific value of t
#' cdf(t = 1, shape = 2, scale = 1)
#'
#' # Evaluate the CDF for a vector of values
#' cdf(t = 1L:30L, shape = 2, scale = 1)
#' @importFrom assertthat assert_that
#' @seealso [survival_function()] and [hazard_function()].
#' @export
cdf_function <- function(pdf) {
  f <- function(t, ...) {
    if (t == 0) {
      t <- .Machine$double.xmin
    }
    assertthat::assert_that(
      t >= 0,
      msg = "t must be non-negative. In this tool, the density function is used to model survival time."
    )
    1 - survival_function(pdf)(t, t0 = 0, ...)
  }
  f_vec <- Vectorize(FUN = f, vectorize.args = "t")
  f_class <- function(time, ...) {
    result <- f_vec(time, ...)
    attr(result, "time") <- time
    class(result) <- "cdf_function"
    result
  }
  f_class
}
