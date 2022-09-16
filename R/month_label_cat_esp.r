#' Add date labels in catalan or spanish to ggplot2
#'
#' Converting English default date labels to calatan or spanish ones in ggplots.
#'#'
#' @param breaks This argument is specified inside \code{scale_x_date}. See Examples.
#' @param language Either 'catalan' or 'spanish'. By default catalan is selected.
#' @param with_days Logical. If TRUE days are shown in the labels. By default is FALSE.
#' @param abbr Logical. If TRUE full month labels are displayed. By default is FALSE.
#'
#' @return
#' A character vector or a ScaleContinuousDate object for a ggplot.
#'
#' @export
#'
#' @examples
#' library(dply);library(ggplot2)
#' ## monthly sequence:
#' df1 <- data.frame(
#'     x = seq.Date(lubridate::as_date('2020-01-01'),lubridate::as_date('2022-05-01'),
#'                  by = '1 month'),
#'     y = rnorm(29) )
#' ## plot
#' plot_1 <- ggplot(df1, aes(x,y))+ geom_line()
#'
#' ## catalan monthly labels
#' plot_1 +
#'     scale_x_date(breaks = '3 month', # specify step width
#'                  labels = month_label_cat_esp )
#'
#' ##  catalan monthly labels without abbreviate
#' plot_1 +
#'     scale_x_date(breaks = '3 month',
#'                  labels = ~ month_label_cat_esp(.x, abbr = FALSE ) )
#'
#' ## daily sequence:
#' df2 <- tibble(
#'     'z' = seq.Date(lubridate::as_date('2021-12-01'),lubridate::as_date('2022-01-15'),
#'                                   by = '1 day'),
#'     y= runif(length(z)) )
#'
#' ## spanish daily labels
#' ggplot(df2, aes(z,y))+
#'     geom_line()+
#'     scale_x_date(breaks = '3 days', # specify step width
#'                  labels = ~ month_label_cat_esp(.x,language = 'spanish',
#'                                                 with_days = TRUE) )
#'
#' ## extract a character vector of labels
#'
#' ksnet::month_label_cat_esp( df2$z, language = 'spanish' )
#'


month_label_cat_esp <- function(breaks, language = c('catalan','spanish'),
                                with_days = FALSE, abbr = TRUE ){

    match.arg(language,c('catalan','spanish'),several.ok = F )

    if( length(language) > 1 ) language <- 'catalan'

    df <- data.frame(fecha = breaks) %>%
        dplyr::mutate(
            year = lubridate::year(fecha),
            mes = as.character( lubridate::month(fecha) ),
            dia = lubridate::day(fecha))

    if( language == 'catalan' ){
        if( abbr ) df$mes <- ksnet::list_meses$cat_abb[ df$mes ]
        else df$mes <- ksnet::list_meses$cat_full[ df$mes ]
    } else if( language == 'spanish' ){
        if( abbr ) df$mes <- ksnet::list_meses$esp_abb[ df$mes ]
        else df$mes <- ksnet::list_meses$esp_full[ df$mes ]
    }

    df <- df %>%
        dplyr::group_by(year) %>%
        dplyr::mutate(is_min_mes = fecha == min(fecha)) %>%
        dplyr::ungroup()

    if( with_days ){
        df <- df %>%
            dplyr::group_by(year, mes) %>%
            dplyr::mutate( is_first_day = fecha == min(fecha) ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(fecha_label = case_when(

                is_min_mes == T & is_first_day == T ~ paste0(dia,' ',mes,'\n', year),
                is_min_mes == F & is_first_day == T ~ paste0(dia,'\n',mes),
                is_min_mes == F & is_first_day == F ~ paste0(dia)
            ))

    } else {
        df <- df %>%
            dplyr::mutate(fecha_label = ifelse(
                is_min_mes == TRUE, paste0(mes,'\n', year), mes) )
    }
    dplyr::pull(df,fecha_label)
}

