
#' Set KSNET caption
#'
#' Set at the bottom-right of the plot KSNET.
#'
#'
#' @param ... a character vector with other caption information such as Source or Notes. See Details.
#'
#' @return
#'  A character vector of length two.
#' @export
#'
#' @details
#' Currently, since the double caption is a bit tricky, only works when come across with \code{theme_ksnet_light}
#' when more information is supplied.#'
#' @examples
#'
#' library(ggplot2)
#' ggplot(mtcars,aes(mpg))+
#'   geom_density()+
#'   labs( caption = caption_ksnet() )
#'
#' # If more caption information if supplied, must go with theme_ksnet_light
#' ggplot(mtcars,aes(mpg))+
#'   geom_density()+
#'   labs( caption = caption_ksnet('Fuente: ECV') )+
#'   theme_ksnet_light()
#'

caption_ksnet <- function(...){
    c(paste0(...),'KSNET')
}

