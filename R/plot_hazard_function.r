#' Plot of the hazard function
#'
#' Constructs the plot of the hazard function of an object of the
#' `hazard_function` class.
#'
#' @param x A numeric vector representing the dataset.
#' @param ... Additional arguments to be passed to the ggplot function.
#'
#' @return A ggplot object displaying the hazard function plot.
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
#'   t = seq(0.001, 1.5, length.out = 100L),
#'   a = 1.5,
#'   b = 1.8,
#'   lambda = 1.5
#' ) |> plot()
#'
#' @importFrom ggplot2 aes geom_line labs theme element_text aes_string xlim ylim
#'
#' @references NADARAJAH, Saralees; KOTZ, Samuel. The beta exponential distribution. Reliability engineering & system safety, v. 91, n. 6, p. 689-697, 2006.
#' @export
plot.hazard_function <- function(x, ...) {
  data <- data.frame(x = attr(x, "time"), y = x)
  ggplot(data, aes_string(x = "x", y = "y")) +
    geom_line() +
    labs(
      title = "Hazard function",
      x = "t",
      y = "h(t)"
    ) +
    xlim(min(data$x), max(data$x)) +
    ylim(min(data$y), max(data$y)) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
}
