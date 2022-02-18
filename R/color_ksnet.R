

#
# La lista de paletas de KSNET
#


# Lista de paletas
#' @export
paletas_ksnet <- list(
  classic = c("#00b2a9", "#BFBFBF", "#FFC000", "#f46572", "#056F6A"),
  bright = c("#00b2a9", "#f46572", "#ffc000", "#A5A5A5", "#056F6A"),
  verde = c('#002d2a', '#004540', '#005d57', '#007770', '#00928a', '#00aea5', '#34cac0', '#5be5db', '#86ffff'),
  gris = c('#0e0e0e', '#242424', '#3b3b3b', '#535353', '#6d6d6d', '#878787', '#a3a3a3', '#bfbfbf', '#dcdcdc'),
  amarillo = c('#3d0c00', '#502700', '#673e00', '#815600', '#9d6d00', '#bb8600', '#d99f00', '#f7b900', '#ffdd36'),
  rojo = c('#570000', '#780017', '#9a082e', '#b42941', '#ce4254', '#e85a69', '#fb7783', '#ff9ba4', '#ffbdc5'),
  verde_amarillo = c('#00b2a9', '#5ab49a', '#7fb68b', '#9ab87c', '#b2ba6c', '#c7bc5b', '#dbbd48', '#edbf30', '#ffc000'),
  rojo_amarillo = c('#ffc000', '#ffb626', '#feab37', '#fda044', '#fc954f', '#fa8a59', '#f87e62', '#f6726a', '#f46572')
)


# color_ksnet function
#' Aplicar un color KSNET a las figuras
#'
#' @return
#' @export
#'
#' @examples
#'
color_ksnet <- function(palette = "classic", reverse = FALSE, n, type = c("discrete", "continuous"), ...) {

    type <- match.arg(type)

    pal <- paletas_ksnet[[palette]]
    if (is.null(pal))
        stop("No hay ninguna paleta con ese nombre.")
    if (reverse) {
        pal <- rev(pal)
    }
    if (missing(n)) {
        n <- length(pal)
    }

    if (type == "discrete" && n > length(pal)) {
        stop(paste0("Esta paleta solo tiene ", length(pal), "colores, que son menos de los que necesitas."))
    }

    out <- switch(type,
                  continuous = grDevices::colorRampPalette(pal)(n),
                  discrete = pal[1:n]
    )
    structure(out, class = "palette", name = palette)
}

#Imprimir la paleta

# print.palette funcion
#' Imprime la paleta
#'
#' @return
#' @export
#'
#'
#'
print.palette <- function(x, ...) {

  dt <- data.frame(a = 1:length(x), b = rep(5, length(x)))
  pl <- ggplot2::ggplot(dt, aes(a,b,fill = factor(a)))+
    ggplot2::geom_col()+
    ggplot2::scale_fill_manual(values = x)+
    ggplot2::guides(fill = "none")+
    ggplot2::labs(title = attr(x, "name"))+
    ggplot2::coord_fixed(ratio = 1)+
    ggplot2::theme_void()+
    ggplot2::theme(plot.title = ggplot2::element_text( hjust = .5,
                                                       vjust = 2))
  print(pl)
}


#' Paleta discreta de colores de KSNET (color)
#'
#' @param palette El nombre de la paleta ("classic", "bright")
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Argumentos adicionales
#'
#' @export
#' @example
#' x <- c(1,2,3,4,5)
#' y <-  rnorm(5, x, 0.5)
#' mis_datos <- data.frame(x = x, y = y)
#' mis_datos %>%
#' ggplot(aes(x = x, y = y, color = y)) +
#' geom_point()
#' scale_color_ksnet_discrete() +
#' theme_ksnet()
#'
scale_color_ksnet_discrete <- function(palette = "classic", reverse = FALSE, ...) {

    pal <- color_ksnet(palette = palette, reverse = reverse, type = "discrete")
    scale_color_manual(values = pal)

}


#' Paleta continua de colores de KSNET (color)
#'
#' @param palette Nombre de la paleta ("verde", "gris", "amarillo", "rojo",
#' "verde_amarillo", "rojo_amarillo")
#' @param reverse Booleano para revertir la paleta (TRUE o FALSE)
#' @param ... Argumentos adicionales
#'
#'@export
#'
scale_color_ksnet_continuous <- function(palette = "verde", reverse = FALSE, ...) {

    pal <- color_ksnet(palette = palette, reverse = reverse, type = "continuous")

    ggplot2::scale_color_gradientn(colors = pal, ...)

}


#' Paleta discreta de colores de KSNET (fill)
#'
#' @param palette El nombre de la paleta ("classic", "bright")
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Argumentos adicionales
#'
#' @export
#' @example
#' x <- c(1,2,3,4,5)
#' y <-  rnorm(5, x, 0.5)
#' mis_datos <- data.frame(x = x, y = y)
#' mis_datos %>%
#' ggplot(aes(x = x, y = y, color = y)) +
#' geom_point()
#' scale_color_ksnet_discrete() +
#' theme_ksnet()
#'
scale_fill_ksnet_discrete <- function(palette = "classic", reverse = FALSE, ...) {

    pal <- color_ksnet(palette = palette, reverse = reverse, type = "discrete")
    scale_fill_manual(values = pal)

}

#' Paleta continua de colores de KSNET (fill)
#'
#' @param palette Nombre de la paleta ("verde", "gris", "amarillo", "rojo",
#' "verde_amarillo", "rojo_amarillo")
#' @param reverse Booleano para revertir la paleta (TRUE o FALSE)
#' @param ... Argumentos adicionales
#'
#'@export
#'
scale_fill_ksnet_continuous <- function(palette = "verde", reverse = FALSE, ...) {

    pal <- color_ksnet(palette = palette, reverse = reverse, type = "continuous")

    ggplot2::scale_fill_gradientn(colors = pal, ...)

}

