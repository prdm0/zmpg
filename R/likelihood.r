#' Calculate the log-likelihood of a probability density function (pdf).
#'
#' This function calculates the log-likelihood of a given probability density function (pdf)
#' using the negative sum of the logarithm of the pdf evaluated at each value of t.
#'
#' @param pdf A probability density function.
#' @return The log-likelihood function.
#' @export
log_likelihood <- function(pdf) {
  function(t, ...) {
    -sum(log(pdf_fragility_zmpg(pdf)(t = t, ...)))
  }
}
