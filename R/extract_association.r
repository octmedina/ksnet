
#' Extract specifics tests from `ksnet::get_association`
#'
#' This function is a helper to work with `ksnet::get_association`. After obtaining
#' the results, for example, we could be interesting in keep in one data.frame only those
#' related to either ANOVA or ANOVA and chi 2.
#'
#' @param obj a dataframe or a list.
#' @param which One of 'ANOVA','corr','t_test','chisq_test'.
#'
#' @return
#' A data.frame
#'
#' @export
#'
#' @examples
#'
#' mtcars$cyl <- factor(mtcars$cyl)
#'
#' mtcars_asociacion <- ksnet::get_get_association(mtcars)
#'
#' extract_association( mtcars_asociacion, 'ANOVA' )
#'
#'
#'
extract_association <- function( obj, which = c('ANOVA','corr','t_test','chisq_test') ){

    match.arg( which, c('ANOVA','corr','t_test','chisq_test'), several.ok = T )

    if( is.data.frame(obj) ){

        out <- dplyr::filter(obj, method %in% which )

    } else if( is.list(obj) ){

        out <- purrr::map_df( obj, ~ dplyr::filter(.x, method %in% which ) )

    }

    out <- dplyr::arrange( janitor::remove_empty(out,'cols'), method )

    return(out)
}


