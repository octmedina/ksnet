
#' Tidy test of proportion
#'
#' Carry out a chi-squared contingency table test for two vectors and export it in a tidy
#' format with \code{broom}. It is also calculated the Cramer V.
#'
#' @param x A vector.
#' @param y A vector.
#' @param ... Optional. Parameters of \code{prop.test} function.
#' @param name_x Optional. Name of the x variable (character).
#' @param name_y Optional. Name of the y variable (character).
#'
#' @return A data.frame with the results.
#' @export
#'
#' @examples
#' association_2dummy(mtcars$am,mtcars$vs)
#'
association_2dummy <- function(x,y,...,name_x=NULL,name_y=NULL){

    is_2_dummies <- is_2dummies(x = x, y = y)

    stopifnot( 'Variables must be dummies' = is_2_dummies )

    if(is.null(name_x)){
        name_x <- deparse( substitute(x) )
        if( grepl('$',name_x) ) name_x <- strsplit(name_x,'\\$')[[1]][2]

    }
    if(is.null(name_y)){
        name_y <- deparse( substitute(y) )
        if( grepl('$',name_x) ) name_y <- strsplit(name_y,'\\$')[[1]][2]
    }

    table_x_y <- table(x,y)
    cramer_v <- questionr::cramer.v(table_x_y)

    out <-  broom::tidy( suppressWarnings( chisq.test( x,y,... ) ) ) %>%
        mutate( is_sign = p.value <= 0.05, .after = p.value) %>%
        mutate( 'var1' = name_x,'var2' = name_y, .before = 1,
                'pair' ='2fac','method'='chisq_test') %>%
        relocate( pair,method, .before = 1 ) %>%
        rename( 'dfreedom'=parameter ) %>%
        mutate( 'cramer_v'=cramer_v )


    return(out)
}
