#' Tidy t-test
#'
#' Carry out a t-test for two vectors and export it in a tidy
#' format with \code{broom}.
#'
#' @param x A vector.
#' @param y A vector.
#' @param ... Optional. Parameters of \code{t.test} function.
#' @param name_x Optional. Name of the x variable (character).
#' @param name_y Optional. Name of the y variable (character).
#'
#' @return A data.frame with the results.
#' @export
#'
#' @examples
#' association_num_dummy( mtcars$mpg, mtcars$vs  )
#' association_num_dummy( mtcars$vs, mtcars$mpg  ) # order is not important
association_num_dummy <- function(x,y,...,name_x=NULL,name_y=NULL){

    is_numdummy <- is_num_dummy(x,y)
    stopifnot( 'One variable must be numeric and the other one dummy' = is_numdummy )

    if(is.null(name_x)){
        name_x <- deparse( substitute(x) )
        if( grepl('$',name_x) ) name_x <- strsplit(name_x,'\\$')[[1]][2]

    }
    if(is.null(name_y)){
        name_y <- deparse( substitute(y) )
        if( grepl('$',name_x) ) name_y <- strsplit(name_y,'\\$')[[1]][2]
    }

    which_is_dummy <- which(c(is_dummy(x), is_dummy(y)))

    if( which_is_dummy == 2)  out <-  broom::tidy( t.test( x ~ y,... ) )
    if( which_is_dummy == 1)  out <-  broom::tidy( t.test( y ~ x,... ) )

    dummy_var <- c(name_x,name_y)[which_is_dummy]

    out <-  out %>%
        mutate( is_sign = p.value <= 0.05, .after = p.value) %>%
        mutate( 'var1' = name_x,'var2' = name_y, .before = 1,
                'pair' ='num_dummy', 'method'='t_test') %>%
        relocate( pair,method, .before = 1 ) %>%
        rename( 'dfreedom'=parameter ) %>%
        mutate( 'is_dummy' = dummy_var, .after = var2 )

    return(out)

}
