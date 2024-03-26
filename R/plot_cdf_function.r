#' Plot Cumulative Distribution Function
#'
#' This function plots the cumulative distribution function (CDF) of a given dataset.
#'
#' @param x A numeric vector representing the dataset.
#' @param ... Additional arguments to be passed to the ggplot function.
#'
#' @return A ggplot object displaying the CDF plot.
#'
#' @examples
#' cdf_weibull <- cdf_function(dweibull)
#' # plot.cdf_function(cdf_weibull(0:10, shape = 2, scale = 1))
#'
#' @importFrom ggplot2 aes geom_line labs theme element_text aes_string
#'
#' @export
plot.cdf_function <- function(x, ...) {
  data <- data.frame(x = attr(x, "time"), y = x)
  ggplot(data, aes_string(x = "x", y = "y")) +
    geom_line() +
    labs(
      title = "Cumulative distribution function",
      x = "t",
      y = "F(t)"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
}
