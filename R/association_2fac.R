#' Tidy test of chi-squared contingency table test
#'
#' Carry out a chi-squared contingency table test for two vectors and export it in a tidy
#' format with \code{broom}. It is also calculated the Cramer V.
#'
#' @param x A vector.
#' @param y A vector.
#' @param ... Optional. Parameters of \code{chisq.test} function.
#' @param name_x Optional. Name of the x variable (character).
#' @param name_y Optional. Name of the y variable (character).
#'
#' @return A data.frame with the results.
#' @export
#'
#' @examples
#' association_2fac( as.factor(mtcars$cyl), as.factor(mtcars$am) )
association_2fac <- function(x,y,...,name_x=NULL,name_y=NULL){

    is_2_fac <- is_2fac(x,y)
    stopifnot( 'Variables must be factors' = is_2_fac )

    if(is.null(name_x)){
        name_x <- deparse( substitute(x) )
        if( grepl('$',name_x) ) name_x <- strsplit(name_x,'\\$')[[1]][2]

    }
    if(is.null(name_y)){
        name_y <- deparse( substitute(y) )
        if( grepl('$',name_x) ) name_y <- strsplit(name_y,'\\$')[[1]][2]
    }

    cramer_v <- questionr::cramer.v(table(x,y))

    out <-  broom::tidy( suppressWarnings( chisq.test( x,y,... ) ) ) %>%
        mutate( is_sign = p.value <= 0.05, .after = p.value) %>%
        mutate( 'var1' = name_x,'var2' = name_y, 
                across(c(var1,var2),~gsub(')','',.x)),
                .before = 1,
                'pair' ='2fac','method'='chisq_test') %>%
        relocate( pair,method, .before = 1 ) %>%
        rename( 'dfreedom'=parameter ) %>%
        relocate( dfreedom,.after = statistic ) %>%
        mutate( 'cramer_v'=cramer_v )

    return(out)

}



