#' Probability generating function of the Geometric Poisson distribution
#'
#' Probability generating function of the Geometric Poisson distribution
#' proposed by Ambagaspitiya and Balakrishnan (1994).
#'
#' @param t A numeric vector representing the input values for \eqn{t \geq 0}.
#' @param mu A numeric value representing the input value for \eqn{\mu > 0}.
#' @param phi A numeric value representing the input value for \eqn{\phi \geq 0}.
#'
#' @return A numeric vector representing the calculated values of fgp_pg.
#'
#' @details The function [fgp_pg] calculates the probability generating by:
#'
#' \eqn{
#'  G(t) = e^{-\frac{1}{\phi}\left[W\left(-\frac{\mu\phi}{1 + \mu\phi}t e^{-\frac{\mu\phi}{1 + \mu\phi}}\right) + \frac{\mu\phi}{1 + \mu\phi}\right]},
#' }
#' with \eqn{0 \leq t \leq 1}, where \eqn{W} is the Lambert function
#' (CORLESS et al., 1996), such that, \eqn{W(x)e^{W(x)} = x}.
#'
#' @references AMBAGASPITIYA, R. S.; BALAKRISHNAN, N. On the compound generalized
#' poisson distributions. ASTIN Bulletin: the Journal of the IAA, Cambridge
#' University Press, v. 24, n. 2, p. 255–263, 1994.
#' @references CORLESS, R. M.; GONNET, G. H.; HARE, D. E.; JEFFREY, D. J.; KNUTH,
#' D. E. On the lambert w function. Advances in Computational mathematics,
#' Springer, v. 5, p. 329–359, 1996.
#' @examples
#' fgp_pg(c(0.1, 0.2, 0.3), 2, 0.5)
#'
#' @seealso [LambertW::W]
#'
#' @importFrom assertthat assert_that
#' @importFrom LambertW W
#' @export
fgp_pg <- function(t, mu, phi) {
  # assert_that(
  #   all(t >= 0 & t <= 1 & mu > 0 & phi >= 0),
  #   msg = "t must be between 0 and 1, mu must be positive and phi must be non-negative"
  # )

  delta <- (mu * phi) / (1 + mu * phi)
  exp((-1 / phi) * (LambertW::W(-delta * exp(-delta) * t) + delta))
}
