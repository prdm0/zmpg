#' Plot of the probability density function (PDF) of ZMPG frailty.
#'
#' Constructs a plot of the probability density function (PDF) of ZMPG frailty.
#'
#' @param x A numeric vector representing the dataset.
#' @param ... Additional arguments to be passed to the ggplot function.
#'
#' @return A ggplot object displaying the PDF plot.
#'
#' @examples
#' zmpg_weibull <- pdf_fragility_zmpg(pdf = dweibull)
#' zmpg_weibull(
#'   t = seq(0.001, 0.5, length.out = 250),
#'   mu = 0.7,
#'   phi = 0.6,
#'   rho = 0.1,
#'   shape = 0.5,
#'   scale = 1.2
#' ) |> plot()
#' @importFrom ggplot2 ggplot aes geom_line labs theme element_text aes_string xlim ylim
#'
#' @export
plot.pdf_fragility_zmpg <- function(x, ...) {
  data <- data.frame(x = attr(x, "time"), y = x)
  ggplot(data, aes_string(x = "x", y = "y")) +
    geom_line() +
    labs(
      title = "Probability density function",
      subtitle = "Zero-Modified Poisson Generalized - ZMPG",
      x = "t",
      y = "f(t)"
    ) +
    xlim(min(data$x), max(data$x)) +
    ylim(min(data$y), max(data$y)) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
}
