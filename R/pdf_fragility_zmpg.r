#' Zero-Modified Generalized Poisson Discrete Frailty Model
#'
#' This function constructs the probability density function of the discrete
#' frailty model of a continuous random variable \eqn{T} through a base density.
#'
#' @param pdf The base probability density function.
#' @return Calculates the improper density of a random variable \eqn{T} that
#' follows the forward discrete frailty model \eqn{f}.
#' @details The probability density function of a random variable \eqn{T}
#' requires the base survival function and hazard function, i.e.,
#' \eqn{S_0(t)} and \eqn{h_0(t)}, respectively. However, it will only be
#' necessary to inform the base density function as an argument for pdf.
#' Internally, the function [pdf_fragility_zmpg()] will calculate
#' \eqn{S_0(t)} and \eqn{h_0(t)}. The probability density function of \eqn{T}
#' is said to be the discrete frailty model. \eqn{T} is a continuous random
#' variable, where the density (frailty model) was obtained through a discrete
#' random variable \eqn{Z} that follows a Zero-Modified Generalized Poisson - ZMPG
#' distribution, with \eqn{Z \sim ZMPG(\mu, \phi, \rho)}. The probability
#' density function of the frailty model is given by:
#'
#' \eqn{
#'
#'  f_T(t) = -\frac{\rho h_0(t) e^{-\frac{1}{\phi}\left[W\left(-\frac{\mu\phi}{1 + \mu\phi}S_0(t) e^{-\frac{\mu\phi}{1 + \mu\phi}}\right) + \frac{\mu\phi}{1 + \mu\phi}\right]}}{\phi}\frac{W\left(-\frac{\mu\phi}{1 + \mu\phi}S_0(t)e^{-\frac{\mu\phi}{1 + \mu\phi}}\right)}{1 + W\left(-\frac{\mu\phi}{1 + \mu\phi}S_0(t)e^{-\frac{\mu\phi}{1 + \mu\phi}}\right)},
#'
#' }
#'
#' with \eqn{t>0}, \eqn{\mu>0}, \eqn{\phi \geq 0} and \eqn{\rho \geq 0}.
#'
#' @seealso [pf_pg()]
#' @examples
#' zmpg_weibull <- pdf_fragility_zmpg(pdf = dweibull)
#' zmpg_weibull(t = 1, mu = 1, phi = 1, rho = 0.5, shape = 0.5, scale = 1.2)
#' @export
pdf_fragility_zmpg <- function(pdf) {
  # Mean: rho * mu
  # Variance: rho * mu * (1 + mu * phi)^2 + rho * (1 - rho) * mu^2
  f <- function(t, mu, phi, rho, ...) {
    delta <- mu * phi / (1 + mu * phi)

    kapa <-
      survival_function(pdf)(t = t, t0 = 0, ...) |>
      fgp_pg(t = _, mu = mu, phi = phi)

    w <- -phi * log(kapa) - delta

    r <- (-rho * hazard_function(pdf)(t = t, ...) * kapa) / phi * w / (1 + w)
    if(r < 0){
      return(0)
    } else {
      return(r)
    }
  }
  f_vec <- Vectorize(FUN = f, vectorize.args = "t")
  f_class <- function(t, ...) {
    result <- f_vec(t, ...)
    attr(result, "time") <- t
    class(result) <- "pdf_fragility_zmpg"
    result
  }
  f_class
}
