
#' Exportar los gráficos
#'
#' Define los parámetros de \code{ggplot2::ggsave}.
#'
#' @param p Un gráfico ggplo2.
#' @param name Character. La ruta y nombre del gráfico.
#' @param ext Character. La extensión. Por defecto '.png'.
#' @param width Numeric. Por defecto 8000.
#' @param height Numeric. Por defecto 4500.
#' @param unit Character. Por defecto "px".
#' @param dpi Numeric. Por defecti 720.
#' @param type Character. Por defecto "cairo".
#' @param ... Otros argumentos que reciba la función \code{ggplot2::ggsave}.
#'
#' @return Exporta el gráfico.
#' @export
#'
#' @examples
#'
#' p <- ggplot2::ggplot( data = mtcars, ggplot2::aes( disp, mpg ) ) + ggplot2::geom_point()
#'
#' ggSave( p, 'Graficos/migrafico' )
#'
ggSave <- function( p, name, ext = '.png', width = 8000, height = 4500, unit = 'px', dpi = 720,
                    type = 'cairo', ...
                    ){
    require(Cairo)

  ggplot2::ggsave(
            plot = p,
            filename = paste0(name,ext),
            width = width,
            height = height,
            units = unit,
            dpi = dpi,
            type = type,
            ... )

}


