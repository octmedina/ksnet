

#' Generate a scatterplot
#'
#' Explore the distribution of your data with a scatterplot This function generates
#' a ggplot2 object, so it's easily expanded with labels, themes, and other elements.
#'
#'
#'
#' @param data data.frame
#' @param xvar variable
#' @param yvar variable
#'
#' @return
#' ggplot
#' @export
#'
#' @examples
#'
#'x <- rnorm(100)
#'y <-  rnorm(100, x, 1)
#' my_data <- data.frame(x = x, y = y)
#' ksnet_scatter(my_data, x, y)

#' @importFrom rlang .data
ksnet_scatter <- function(data, xvar, yvar, color = color_ksnet()[1]) {

    gg <-
        ggplot2::ggplot(data) +
        ggplot2::aes(x = {{xvar}}, y = {{yvar}}) +
        ggplot2::geom_point(color = color) +
        theme_ksnet()

    return(gg)

}
