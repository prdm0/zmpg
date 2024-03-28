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
#' The function returned by [survival_function()] also has the argument
#' `asymptotic_quantile = 30`, which defaults to `30`. This argument is responsible
#' for evaluating the survival function at the 30th quantile, providing a good approximation
#' for the cure fraction. As this is a numerical evaluation, and depending on the
#' complexity of the base probability density function, it may be that `asymptotic_quantile = 30`
#' produces an error, requiring the value of `asymptotic_quantile` to be changed.
#'
#' @references NADARAJAH, Saralees; KOTZ, Samuel. The beta exponential distribution. Reliability engineering & system safety, v. 91, n. 6, p. 689-697, 2006.
#'
#' @examples
#' survival_weibull <- survival_function(dweibull)
#' survival_weibull(0:10, shape = 2, scale = 1, asymptotic_quantile = 30)
#'
#' # The user can also define any density
#' # Saraless Nadarajah and Samnuel Kotz (2006)
#' beta_exponential <- function(x, a, b, lambda){
#'  lambda / beta(a, b) * exp(-b * lambda * x) * (1 - exp(-lambda * x))^(a - 1)
#' }
#' survival_beta_exponential <- survival_function(beta_exponential)
#' survival_beta_exponential(
#'   t = seq(0.01, 1.5, length.out = 20L),
#'   a = 1.5,
#'   b = 1.8,
#'   lambda = 1.5
#' )
#'
#' # Sobrevivencia do modelo de fragilidade descreta ZMPG
#' fragility_model <- pdf_fragility_zmpg(pdf = dweibull)
#' fragility_survival <- survival_function(fragility_model)
#' fragility_survival(
#'   t = seq(0.001, 4, length.out = 50L),
#'   shape = 1.3,
#'   scale = 1.2,
#'   mu = 3.7,
#'   rho = 1.5,
#'   phi = 2.5,
#'   asymptotic_quantile = 30
#'  ) |> plot()
#'
#' @seealso [hazard_function()].
#' @importFrom stats integrate
#' @export
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

  # # Wrapper function to add class and attributes to the result
  f_class <- function(t, t0 = 0, ..., asymptotic_quantile = 30) {
    result <- f_vec(t = t, t0 = 0, ...)
    attr(result, "time") <- t
    attr(result, "cure_fraction") <- f_vec(t = asymptotic_quantile, t0 = t0, ...)
    class(result) <- "survival_function"
    result
  }
  f_class
}
