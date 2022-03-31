#' Generate a line chart
#'
#' This function generates a ggplot2 object, so it's easily
#' expanded with labels, themes, and other elements.
#'
#'
#'
#' @param data data.frame
#' @param xvar variable
#' @param yvar variable
#'
#' @return
#' @export
#'
#' @examples
#'
#'x <- c("A","B","C","D")
#'y <- c(5, 8, 12,15)
#' my_data <- data.frame(x = x, y = y)
#' ksnet_line(my_data, x, y)
#'
#' @importFrom rlang .data
ksnet_line <- function(data, xvar, yvar, color = color_ksnet()[1]) {

  gg <-
    ggplot2::ggplot(data) +
    ggplot2::aes(x = {{xvar}}, y = {{yvar}}, group = 1) +
    ggplot2::geom_line(color = color, size = 2) +
    theme_ksnet()

  return(gg)

}
