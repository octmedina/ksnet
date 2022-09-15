
#' Get a pallete of the same color with different transparencies
#'
#' Sometimes a variable has more categories than colors in `ksnet` functions.
#' To deal with this situation this function returns a vector of n colors
#' with progressive transparency.
#'
#' @param color a hex color, for example, #00b2a9
#' @param n the number of new colors. Not necessary if values are specified.
#' @param values Optional. Name of the labels.
#' @param rev Optional. Reverse order.
#' @param min_alpha Minimal transparency. Default is 0.3.
#' @param max_alpha Maximum transparency. Default is 1.
#'
#' @return
#' A vector of colors with progressive transparency. If values are supplied
#' it is returned a named vector.
#'
#' @export
#'
#' @examples
#' df <- data.frame( x = factor(letters[1:6]),
#'                   y = round( runif(6),2 ) )
#'
#' color <- ksnet::paletas_ksnet$classic[5] # "#056F6A"
#'
#' (col_x <- color_pallette_alpha( color, values = levels(df$x) ))
#'
#' library(ggplot2)
#' ggplot( df, aes( x, y, fill = x ) )+
#'     geom_col()+
#'     scale_fill_manual( values = col_x )
#'
#' # reverse color order:
#' (col_x_rev <- color_pallette_alpha( color, values = levels(df$x),rev = TRUE ))
#'
#' ggplot( df, aes( x, y, fill = x ) )+
#'     geom_col()+
#'     scale_fill_manual( values = col_x_rev )
#'
#' ## Using 'n' parameter
#' color_pallette_alpha( color, n = 10 )

color_pallette_alpha <- function(color, n, values = NULL, rev = FALSE,
                                 min_alpha = 0.3, max_alpha = 1){

    if( !is.null(values) ) n <- length(values)

    col_rgb <- as.numeric( col2rgb( col = color )) # color a RGB
    alphas <- seq(min_alpha, max_alpha, length.out = n) # seq de transparencias
    # vector de colores idénticos pero cambiando su transparencia
    out <- purrr::map_chr(alphas,
                          ~ rgb(col_rgb[1]/255,col_rgb[2]/255,col_rgb[3]/255, alpha = .x) )

    if( !is.null(values) ){
        names(out) <- values
        if( rev ) names(out) <- rev( names(out) )
    }
    if( rev ) out <- rev(out)

    return(out)
}


