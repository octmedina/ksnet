#' Extract descriptive statistics between two factors
#'
#' @param x A vector.
#' @param y A vector.
#' @param peso NOT IMPLEMENTED.
#' @param name_x Optional. Name of the x variable (character).
#' @param name_y  Optional. Name of the Y variable (character).
#' @param round_p  A numeric of length 1. Number of decimals of proportions.
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' # DATA FOR THE EXAMPLES #
#'
#' library(dplyr)
#' df <- mtcars %>%
#'  filter( carb != 6 & carb != 8 ) %>%
#'  mutate( across(c(cyl,vs,am,gear, carb), as.factor) )
#'
#' df$vs <- as.factor(df$vs)
#' summarise_fac_fac( df$cyl, df$vs, peso = abs(runif(30,0.5,3)) )

summarise_fac_fac <- function(x,y, peso=NULL,name_x=NULL,name_y=NULL, round_p=3){

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

    data_temp <- data.frame('x'=x,'y'=y,'pond'=1)

    if(!is.null(peso)) data_temp$pond <- peso

    data_temp <- data_temp %>%
        group_by(x) %>%
        ksnet::count_and_share(y , wt = pond) %>%
        ungroup() %>%

        mutate( 'Error' = sqrt( (p*(1-p))/n ),
                'p_min'= p - qt(0.025,df = n,lower.tail = F)*Error ,
                'p_max'= p + qt(0.025,df = n,lower.tail = F)*Error
        ) %>%

        mutate( across(where(is.numeric),round,round_p),
                n = round(n) ) %>%
        rename( 'Desv.Est.'='Error' )

    names(data_temp)[1:3] <- c(name_x,name_y,'N')

    attr(data_temp,'type') <- 'summarise_fac_fac'

    return(data_temp)
}

