#' Generalized Poisson Distribution
#'
#' @description
#' Probability function of a random variable with Generalized Poisson distribution.
#'
#' @param y Vector of integer values, with \eqn{y \geq 0}.
#' @param mu The mean parameter, with \eqn{\mu > 0}.
#' @param phi The dispersion parameter, with \eqn{\phi \geq 0}.
#'
#' @return Vector of probability values of \eqn{Y = y}.
#' @examples
#' pg(0L:100L, mu = 0.5, phi = 0.7) |> sum()
#'
#'
#' @seealso [dgeom()], [dpois()] and [dbinom()].
#' @export
dgp <- function(y, mu, phi){
  alpha <- ((1 + phi * y)^(y - 1))/factorial(y)
  g <- (mu * exp(-mu * phi * (1 + mu * phi)^(-1)))/(1 + mu * phi)
  f <- exp(mu * (1 + mu * phi)^(-1))
  (alpha * g^y)/f
}
