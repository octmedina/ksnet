#' Count and Share
#'
#' Tras realizar un group_by es habitual calcular el porcentaje.
#' Con esta función se obtiene por defecto.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr count
#' @importFrom dplyr mutate
#'
#' @param df Un tibble o dataframe.
#' @param ... Nombre de las variables a utilizar.
#'
#' @return Un nuevo tibble con el conteo y el porcentaje
#' @export
#'
#' @examples
#'
#' mtcars %>%
#'  count_and_share(cyl, vs)
count_and_share <- function(df,...){
    df %>%
        count(...) %>%
        mutate( p = n/sum(n) )
}
