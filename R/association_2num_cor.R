#' Tidy test of correlation
#' Carry out a correlation test for two vectors and export it in a tidy
#' format with \code{broom}.
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
#' association_2num_cor(mtcars$mpg, mtcars$disp)

association_2num_cor <- function(x,y,...,name_x=NULL,name_y=NULL){

    is_2_num <- is_2num(x = x, y = y)

    stopifnot( 'Variables must be numeric' = is_2_num )

    if(is.null(name_x)){
        name_x <- deparse( substitute(x) )
        if( grepl('$',name_x) ) name_x <- strsplit(name_x,'\\$')[[1]][2]

    }
    if(is.null(name_y)){
        name_y <- deparse( substitute(y) )
        if( grepl('$',name_x) ) name_y <- strsplit(name_y,'\\$')[[1]][2]
    }

    cor_test <- broom::tidy( cor.test(x,y,...) ) %>%
        mutate( is_sign = p.value <= 0.05, .after = p.value,
                'method'='correlation',
                'pair' ='2num'
        ) %>%
        relocate( pair, method, .before = 1 ) %>%
        mutate( 'var1'=name_x, 'var2'=  name_y, .before = estimate) %>%
        rename('dfreedom' = parameter, 'cor' = estimate) %>%
        relocate( conf.low, conf.high, .after = cor ) %>% 
         relocate( dfreedom, .after = statistic )
    # names(prop_test)[c(2:3,7)] <- c(name_x,name_y,'dfreedom')

    return(cor_test)
}

