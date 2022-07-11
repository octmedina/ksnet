#' Extract descriptive statistics grouping by a factor
#'
#'
#'
#' @param x A vector.
#' @param y A vector.
#' @param peso Optional. A vector of weights.
#' @param name_x Optional. Name of the x variable (character).
#' @param name_y Optional. Name of the y variable (character).
#' @param round_stats A numeric of length 1. Number of decimals of stats.
#' @param round_p A numeric of length 1. Number of decimals of proportions.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#'
#' # DATA FOR THE EXAMPLES #
#'
#' library(dplyr)
#' df <- mtcars %>%
#'  filter( carb != 6 & carb != 8 ) %>%
#'  mutate( across(c(cyl,vs,am,gear, carb), as.factor) )
#'
#' summarise_fac_num(df$mpg, df$cyl,runif(30,0.5,1.5))
#' summarise_fac_num(mtcars$mpg, as.factor(mtcars$cyl))
#' summarise_fac_num(mtcars$mpg, mtcars$am)


summarise_fac_num <- function(x,y, peso=NULL,name_x=NULL,name_y=NULL, round_stats=3,round_p=3){
    is_numfac <- is_num_fac(x,y)
    is_numdummy <- is_num_dummy(x,y)
    is_correct <- any(is_numfac,is_numdummy)
    stopifnot( 'One variable must be numeric and the other one factor' = is_correct )

    if(is.null(name_x)){
        name_x <- deparse( substitute(x) )
        if( grepl('$',name_x) ) name_x <- strsplit(name_x,'\\$')[[1]][2]

    }
    if(is.null(name_y)){
        name_y <- deparse( substitute(y) )
        if( grepl('$',name_x) ) name_y <- strsplit(name_y,'\\$')[[1]][2]
    }

    which_is_num <- which(c(is.numeric(x), is.numeric(y)))

    if( length(which_is_num) == 2 ){
        which_is_dummy <- which(c(is_dummy(x), is_dummy(y)))
        which_is_num <- which_is_num[ -which_is_dummy ]
    }

    var_num <- c(name_x,name_y)[which_is_num]
    var_fac <- c(name_x,name_y)[-which_is_num]

    data_temp <- data.frame('x'=x,'y'=y)
    names(data_temp) <- c(name_x,name_y)
    data_temp <- data_temp[,c(var_fac,var_num)]

    if(!is.null(peso)) data_temp$pond <- peso
    else data_temp$pond <- 1

    var_fac_sym <- sym(var_fac)

    data_temp <- data_temp %>%
        rename('y' =2)

    data_count_temp <- data_temp %>%
        ksnet::count_and_share(!!var_fac_sym, wt = pond) %>%
        rename( 'N' = n ) %>%
        mutate( N = round(N),
               'p' = round(N/sum(N),round_p) ) %>%
        ungroup()


    data_summ_temp <- data_temp %>%
        group_by(!!var_fac_sym) %>%
        summarise(
                   'Min'= min(y,na.rm=T),
                   'pt_25'= Hmisc::wtd.quantile(y,pond,.25,na.rm=T),
                   'Mediana'=Hmisc::wtd.quantile(y,pond,.5,na.rm=T),
                   'Media'=weighted.mean(y,pond,na.rm=T),
                   'pt_75'=Hmisc::wtd.quantile(y,pond,.75,na.rm=T),
                   'Max'=max(y,na.rm=T),
                   'Desv.Est.'= sqrt(Hmisc::wtd.var(y,pond,na.rm=T)),
                   .groups = 'drop'
        ) %>%
        mutate( across(where(is.numeric),round,round_stats) ) %>%
        mutate( 'var_num' = var_num, .after = 1 )

    data_temp <- left_join( data_summ_temp, data_count_temp ) %>%
        relocate(c(N,p), .after = var_num )

    attr(data_temp,'type') <- 'summarise_fac_num'

    return(data_temp)
}
