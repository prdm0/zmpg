#' Numerical survival function
#'
#' Constructs the numerical survival function from a probability density function.
#'
#' @param pdf The probability density function.
#'
#' @return Returns the numerical value of survival at time \eqn{t}.
#'
#' @details The function [survival_function()] returns a function in terms of
#' \eqn{t} and the additional parameters of the probability density function
#' that is passed as an argument to [pdf]. The returned function calculates
#' survival numerically, making the implementation of survival functions quick.
#' The returned function is vectorized in \eqn{t}, that is, a vector of time
#' instances can be passed as an argument.
#'
#' The survival function returned by [survival_function()] also has a `t0`
#' argument that defaults to `t0 = 0`. The `t0` argument is the initial
#' time for the calculation of the survival function. The `t0` argument is
#' useful for calculating the survival function from a start time different
#' from zero. In most cases, you should not change the default value of `t0`.
#'
#' @examples
#' survival_weibull <- survival_function(dweibull)
#' survival_weibull(0:10, shape = 2, scale = 1)
#'
#' @seealso [hazard_function()].
#' @export
#' @importFrom stats integrate
survival_function <- function(pdf) {
  f <- function(t, t0 = 0, ...) {
    assertthat::assert_that(
      t >= 0 && t0 >= 0 && t >= t0,
      msg = "t >= 0, t0 >= 0 and t >= t0"
    )

    if(t == 0) {
      return(1)
    }

    r <- 1 - integrate(
      f = \(t) pdf(t, ...),
      lower = t0,
      upper = t
    )$value
    if(r < 0) {
      return(0)
    } else {
      return(r)
    }

  }
  f_vec <- Vectorize(FUN = f, vectorize.args = "t")
  f_class <- function(time, ...) {
    result <- f_vec(time, ...)
    attr(result, "time") <- time
    class(result) <- "survival_function"
    result
  }
  f_class
}
