#' Add Month labels in Catalan to ggplot2
#'
#' Converting English default month labels to calatan ones in ggplots.
#'
#'
#' @param breaks this argument is specified inside \code{scale_x_date}. See Examples.
#'
#' @return
#' A ScaleContinuousDate object for a ggplot.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'    x = seq.Date(lubridate::as_date('2020-01-01'),lubridate::as_date('2022-05-01'),
#'                 by = '1 month'),
#'    y = rnorm(29) )
#'
#' ggplot(df, aes(x,y))+
#'  geom_line()+
#'  scale_x_date(breaks = '1 month', # specify step width
#'               labels = catalan_month_label ) # add the function to label param

catalan_month_label <- function(breaks){
    data.frame(fecha=breaks) %>%
        dplyr::mutate( year = lubridate::year(fecha),
                mes = lubridate::month(fecha),
                mes = dplyr::case_when(
                    mes == 1 ~ 'Gen', mes == 5 ~ 'Mai', mes == 9 ~ 'Set',
                    mes == 2 ~ 'Feb', mes == 6 ~ 'Jun', mes == 10 ~ 'Oct',
                    mes == 3 ~ 'Mar', mes == 7 ~ 'Jul', mes == 11 ~ 'Nov',
                    mes == 4 ~ 'Abr', mes == 8 ~ 'Set', mes == 12 ~ 'Des')) %>%
        dplyr::group_by( year ) %>%
        dplyr::mutate( is_min = fecha == min(fecha) ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate( fecha_label = ifelse( is_min == TRUE, paste0(mes,'\n',year),mes  ) ) %>%
        dplyr::pull(fecha_label)
}

