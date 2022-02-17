

#' Generate a bar chart
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
#' ksnet_bar(my_data, x, y)

#' @importFrom rlang .data
ksnet_bar <- function(data, xvar, yvar, fill = color_ksnet("ksnet_classic")[1]) {

    gg <-
        ggplot2::ggplot(data) +
        ggplot2::aes(x = {{xvar}}, y = {{yvar}}) +
        ggplot2::geom_col(fill = fill, color = 'white') +
        theme_ksnet()

    return(gg)

}

