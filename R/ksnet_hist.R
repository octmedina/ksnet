

#' Generate a histogram with ksnet styling
#'
#' Explore the distribution of your data with a histogram. This function generates
#' a ggplot2 object, so it's easily expanded.
#'
#'
#'
#' @param data data.frame
#' @param xvar variable
#'
#' @return
#' @export
#'
#' @examples
#'
#' my_data <- data.frame(x = c(1,2,3,4,5))
#' ksnet_hist(my_data, x)

#' @importFrom rlang .data
ksnet_hist <- function(data, xvar, fill = color_ksnet("ksnet_classic")[1]) {

  gg <-
    ggplot2::ggplot(data) +
    ggplot2::aes(x = {{xvar}}) +
    ggplot2::geom_histogram(fill = fill, color = 'white') +
    theme_ksnet()

  return(gg)

}
