

#' Get (weighted) summary statistics of numeric variables
#'
#' This function computes the summary statistics of one or more variables.
#' It can receives a grouped data.frame.
#'
#' @param df A data.frame.
#' @param var Character. Variable/s name.
#' @param peso Character.A numeric vector of weights. Default is NULL.
#'
#' @return
#' A dataframe.
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' summary_statsNum(mtcars, 'disp')
#'
#' mtcars %>%
#'  group_by(cyl) %>%
#'  summary_statsNum('mpg')
#'
#'  mtcars %>%
#'      mutate( 'peso' = runif(32) ) %>%
#'      group_by( cyl ) %>%
#'      summary_statsNum(c('mpg','disp'), peso = 'peso')

summary_statsNum <- function(df,var,peso=NULL){


    if( length(var) > 1 ){

        purrr::map_dfr( var, function(x){ summary_statsNum( df = df, var = x, peso = peso ) } )

    } else{

        var_sym <- sym(var)

        if(is.null(peso)){
            df %>%
                dplyr::summarise(
                    'var' = var,
                    'N' = skimr::n_complete(!!var_sym),
                    'Missing' = skimr::n_missing(!!var_sym),
                    'Min'=min(!!var_sym,na.rm = T),
                    'Q25'=quantile(!!var_sym,0.25,na.rm = TRUE),
                    'Media'=mean(!!var_sym,na.rm = T),
                    'Mediana'=median(!!var_sym,na.rm = T),
                    'Q75'=quantile(!!var_sym,0.75,na.rm = TRUE),
                    'Max'=max(!!var_sym,na.rm = T),
                    'sd' = sd(!!var_sym,na.rm = T),
                    'hist' = skimr::inline_hist(!!var_sym),
                    .groups = 'drop'
                ) %>%
                mutate( across(where(is.numeric), \(x)round(x,3)) )
        } else {

            peso_sym <- sym(peso)

            df %>%
                dplyr::summarise(
                    'var' = var,
                    'N' = skimr::n_complete(!!var_sym),
                    'Missing' = skimr::n_missing(!!var_sym),
                    'Min'= min(!!var_sym,na.rm = T),
                    'Q25.W'= DescTools::Quantile(!!var_sym,!!peso_sym,0.25,na.rm = T),
                    'Media.W'= weighted.mean(!!var_sym,!!peso_sym,na.rm = T),
                    'Mediana.W'= DescTools::Quantile(!!var_sym,!!peso_sym,0.5,na.rm = T),
                    'Q75.W'= DescTools::Quantile(!!var_sym,!!peso_sym,0.75,na.rm = T),
                    'Max' = max(!!var_sym,na.rm = T),
                    'sd.W' = DescTools::SD(!!var_sym,!!peso_sym,na.rm = T),
                    'hist' = skimr::inline_hist(!!var_sym),
                    .groups = 'drop'
                ) %>%
                mutate( across(where(is.numeric), \(x)round(x,3)) )
        }
    }

}



#' Get (weighted) summary statistics of factor variables
#'
#' This function computes the summary statistics of one or more variables.
#' It can receives a grouped data.frame.
#'
#' @param df A data.frame.
#' @param var Character. Variable/s name.
#' @param peso Character.A numeric vector of weights. Default is NULL.
#'
#' @return
#' A dataframe.
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' mtcars %>%
#'  group_by( cyl ) %>%
#'  summary_statsFac( c('vs') )
#'
#'  mtcars %>%
#'      mutate( 'peso' = runif(32) ) %>%
#'      group_by( cyl ) %>%
#'      summary_statsFac( 'vs', peso = 'peso' )
#'
#'  mtcars %>%
#'      group_by( cyl, am ) %>%
#'      summary_statsFac( c('vs') )
#'
#'  mtcars %>%
#'      group_by( cyl ) %>%
#'      summary_statsFac( c('am','vs') )

summary_statsFac <- function(df,var,peso=NULL){

    var_sym <- syms(var)

    if(is.null(peso)){

        out <- df %>%
            ksnet::count_and_share( !!!var_sym ) %>%
            mutate( across(where(is.numeric), \(x)round(x,4)) )

    } else {

        peso_sym <- sym(peso)

        out <- df %>%
            ksnet::count_and_share( !!!var_sym, wt = !!peso_sym ) %>%
            mutate( across(where(is.numeric), \(x)round(x,4)) )
    }

    if( is.grouped_df(out) ){

        names(out)[ncol(out)] <- paste0('p[',
                                        paste0( group_vars(out),collapse = ','),
                                        ']')

    }
    return(ungroup(out))
}




