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
#' hazard_weibull <- hazard_function(dweibull)
#' plot.hazard_function(hazard_weibull(1:10, shape = 2, scale = 1))
#'
#' @importFrom ggplot2 aes geom_line labs theme element_text aes_string
#'
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
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
}
