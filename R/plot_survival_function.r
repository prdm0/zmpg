#' Plot of the survival function
#'
#' Constructs the plot of the survival function of an object of the
#' `survival_function` class.
#'
#' @param x A numeric vector representing the dataset.
#' @param ... Additional arguments to be passed to the ggplot function.
#'
#' @return A ggplot object displaying the survival function plot.
#'
#' @examples
#' survival_weibull <- survival_function(dweibull)
#' plot.survival_function(survival_weibull(0:10, shape = 2, scale = 1))
#'
#' @importFrom ggplot2 aes geom_line labs theme element_text aes_string
#'
#' @export
plot.survival_function <- function(x, ...) {
  data <- data.frame(x = attr(x, "time"), y = x)
  ggplot(data, aes_string(x = "x", y = "y")) +
    geom_line() +
    labs(title = "Survival function", x = "t", y = "S(t)") +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
}
