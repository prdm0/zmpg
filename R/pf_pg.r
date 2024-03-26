#' Probability function of a random variable with Generalized Poisson distribution
#'
#' @param y Vector of integer values, with \eqn{y \geq 0}.
#' @param mu The mean parameter, with \eqn{\mu > 0}.
#' @param phi The dispersion parameter, with \eqn{\phi \geq 0}.
#'
#' @return Probability value of \eqn{Y = y}.
#'
#' @details The probability function of a random variable \eqn{Y} with a Generalized Poisson distribution is expressed by:
#' \deqn{\pi(y; \mu, \phi) = \frac{\frac{(1 + \phi y)^{y-1}}{y!} \left[\frac{\mu e^{-\mu\phi(1 + \mu\phi)^{-1}}}{1 + \mu\phi}\right]^y}{e^{\mu(1 + \mu\phi)^{-1}}},\,\, y = 0, 1, 2, \ldots,}
#' \eqn{\mu>0} and \eqn{\phi \geq 0}.
#'
#' @examples
#' pf_pg(0:100, 3, 0.5) |> sum()

#'
#' @importFrom assertthat assert_that
#' @importFrom gmp factorialZ
#'
#' @export
pf_pg <- function(y, mu, phi) {
  # Mean: mu
  # Variance: mu * (1 + mu * phi)^2
  f <- function(y, mu, phi) {
    assertthat::assert_that(
      y >= 0L && mu > 0 && phi >= 0,
      msg = "y, phi must be non-negative and mu must be positive"
    )

    if (Sys.info()["machine"] == "x86_64" && y > 170L) {
      y_factorial <- log(as.numeric(factorialZ(y)))
    } else if (Sys.info()["machine"] == "i386" && y > 34L) {
      y_factorial <- log(as.numeric(factorialZ(y)))
    } else {
      y_factorial <- factorial(y)
    }

    theta <- mu * phi
    log_a <- (y - 1) * log(1 + phi * y) - log(y_factorial)
    log_b <- y * (log(mu) - theta * (1 + theta)^(-1) - log(1 + theta))
    log_c <- mu * (1 + theta)^(-1)
    exp(log_a) * exp(log_b) / exp(log_c)
  }
  result <- Vectorize(FUN = f, vectorize.args = "y")
  result(y, mu, phi)
}
pf_pg(0L:100L, mu = 1, phi = 1) |> sum()
