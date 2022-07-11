#' Tidy analysis of variance test (ANOVA)
#'
#' Carry out a ANOVA test for two vectors and export it in a tidy
#' format with \code{broom}.
#'
#' @param x A vector.
#' @param y  A vector.
#' @param ... Optional. Parameters of \code{aov} function.
#' @param name_x Optional. Name of the x variable (character).
#' @param name_y Optional. Name of the y variable (character).
#'
#' @return A data.frame with the results.
#' @export
#'
#' @examples
#' association_num_fac(mtcars$mpg, as.factor(mtcars$cyl))

association_num_fac <- function(x,y,...,name_x=NULL,name_y=NULL){

    is_numfac <- is_num_fac(x,y)
    stopifnot( 'One variable must be numeric and the other one dummy' = is_numfac )

    if(is.null(name_x)){
        name_x <- deparse( substitute(x) )
        if( grepl('$',name_x) ) name_x <- strsplit(name_x,'\\$')[[1]][2]

    }
    if(is.null(name_y)){
        name_y <- deparse( substitute(y) )
        if( grepl('$',name_x) ) name_y <- strsplit(name_y,'\\$')[[1]][2]
    }

    which_is_num <- which(c(is.numeric(x), is.numeric(y)))

    if( which_is_num == 1)  out <-  broom::tidy( aov( x ~ y,... ) )[1,]
    if( which_is_num == 2)  out <-  broom::tidy( aov( y ~ x,... ) )[1,]

    fac_var <- c(name_x,name_y)[-which_is_num]

    out <-  out %>%
        mutate( 'var1' = name_x,'var2' = name_y, .before = 1,
                'pair' ='num_fac',
                'method'='ANOVA') %>%
        relocate( pair,method, .before = 1 ) %>%
        rename( 'dfreedom'=df ) %>%
        mutate( is_sign = p.value <= 0.05, .after = p.value) %>%
        mutate( 'is_factor' = fac_var, .after = var2 ) %>%
        select( -c(term,sumsq,meansq) )

    return(out)

}
