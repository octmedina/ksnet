



#' Bold or italicize an expression (html)
#'
#' Vector changes from character to html character. Designed to be used in shiny or html outputs.
#'
#' @param x A character vector of length 1.
#' @param which A character vector of length 1. Options: 'bold', 'italic' or 'both'.
#' @param ... Other parameters passed onto \code{glue::glue} function.
#'
#' @return
#' An html character vector.
#' @export
#'
#' @examples
#' bold_italic('Hola')
#' bold_italic(c('Hola'),'italic')
#' bold_italic('Hola','both')

bold_italic <- function(x, which = c('bold','italic','both'),... ){

    if( length(which) != 1 ) which <- 'bold'

    match.arg(which, c('bold','italic','both'))

    switch (which,
            'bold' = gt::html(glue::glue('<b>{x}</b>',...)),
            'italic' = gt::html(glue::glue('<i>{x}</i>',...)),
            'both' = gt::html(glue::glue('<b><i>{x}</i></b>',...))
        )
}

