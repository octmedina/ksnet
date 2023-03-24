
#' Theme KSNET fot ggplots
#'
#' This is one of the available ksnet themes for plots. Its parameters
#' are just those specified within \code{theme} function. When an argument
#' is not accesible with this function, it can be combined with another
#' specific \code{theme}.
#'
#' @param legend_position character vector. Default is right.
#' @param is_x_date when x axis represents a date vector set to TRUE to adjust vjust.
#' @param caption_ksnet Logical. Set to TRUE when using \code{ksnet::caption_ksnet}. See Examples.
#' @param x_vjust numeric. Default 4.
#' @param remove_grid_major_y logical. Default TRUE.
#' @param remove_grid_major_x logical. Default TRUE.
#' @param title_size numeric. Default 14.5.
#' @param subtitle_size numeric. Default 12.5.
#' @param x_size numeric. Axis title size. Default 12.5.
#' @param y_size numeric. Axis title size. Default 12.5.
#' @param axis_x_size numeric. Axis text size. Default 12.
#' @param axis_y_size numeric. Axis text size. Default 12.
#' @param font_family default is NULL. But is is necessary to have it installed. See Details.
#'
#' @details
#' At the beginning of your script, after having installed Public Sans font, you should
#' write the following code to load the font:
#' windowsFonts('Public Sans'='Public Sans')
#'
#' Another alternative is to used the package \{showtext}.
#'
#' @return
#' An object of class gg theme to add for an existing ggplot object.
#'
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars,aes(mpg))+
#'   geom_density()+
#'   labs( caption = 'Fuente: Elaboración propia' )+
#'   theme_ksnet_light()
#'
#' ## gráfico divulgativo con la etiqueta KSNET
#' ggplot(mtcars,aes(mpg))+
#'   geom_density()+
#'   labs( caption = ksnet::caption_ksnet( 'Fuente: Elaboración propia' ))+
#'   theme_ksnet_light( caption_ksnet = TRUE,
#'                      remove_grid_major_x = FALSE)
#'
#'
theme_ksnet_light <- function(legend_position='right',
                              is_x_date = FALSE,
                              caption_ksnet = FALSE,
                              x_vjust=4,
                              remove_grid_major_y = TRUE,
                              remove_grid_major_x = TRUE,
                              title_size=14.5,
                              subtitle_size=12.5,
                              x_size=12.5,
                              y_size=12.5,
                              axis_x_size=12,
                              axis_y_size=12,

                              plot_font= NULL
){
#
#     sysfonts::font_add("PTSans", "Fonts/PTSans.ttf")
#
#     showtext::showtext_auto()

    tema <- theme_minimal()+
        theme(
            plot.title = element_text(size=title_size,face='bold',family = plot_font) ,
            plot.subtitle = element_text(size = subtitle_size, family = plot_font),

            axis.title.y = element_text(size=x_size),
            axis.title.x = element_text(size=y_size),

            axis.text.x = element_text(size=axis_x_size,vjust=x_vjust),
            axis.text.y = element_text(size=axis_y_size),

            plot.caption = element_text(size= 8.5, hjust=0,colour = rgb(0,0,0,.65),
                                        family = plot_font),
            strip.text = element_text(size=13),
            legend.position =legend_position, legend.text = element_text(size=13),

            title=element_text( family = plot_font),
            text=element_text( family = plot_font),

            panel.grid.minor = element_blank()

        )
    if( remove_grid_major_y ){
        tema <- tema + theme( panel.grid.major.y = element_blank() )
    }
    if( remove_grid_major_x ){
        tema <- tema + theme( panel.grid.major.x = element_blank() )
    }
    if( is_x_date ){
        tema <- tema + theme( axis.text.x = element_text(size=axis_x_size,
                                                         vjust=1) )
    }
    if( caption_ksnet ){
        tema <- tema + theme( plot.caption = element_text(size= c(8.5,9) , hjust=c(0,1),
                                                          colour = rgb(0,0,0,.65),
                                                          family = plot_font) )
    }

    return(tema)
}

# library(ggplot2)
#
# ggplot( mtcars, aes(disp,mpg) )+geom_point()+theme_ksnet_light()
#
# ggplot( mtcars, aes(disp,mpg) )+geom_point()+
#     theme_ksnet_light(plot_font = 'Calibri_light')















