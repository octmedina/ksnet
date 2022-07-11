#' Tidy summarise results
#'
#' Detect pair type and carry out the appropiate summarise function. It can be passed two vectors (x and y) or a dataframe.
#'
#' If the input is a dataframe, when `pairs_to_check` is not specified all possible combinations between variables
#' are taken into account. If it is a character vector of variable names, only these pair combinations will be used.
#' Besides, it can be a dataframe with two columns specifying the desired pairs. Finally, it can be a
#' named list in which each name is a variable name, and each element is a character vector with the names
#' of the variables to combine with the element name. See Examples.
#'
#' @param x A vector.
#' @param y A vector.
#' @param peso  Optional. A vector of weights.
#' @param round_stats A numeric of length 1. Number of decimals of stats.
#' @param round_p  A numeric of length 1. Number of decimals of proportions.
#' @param name_x Optional. Name of the x variable (character).
#' @param name_y Optional. Name of the y variable (character).
#' @param df A data.frame with the required data.
#' @param pairs_to_check NULL (default), a character vector, a dataframe of two columns or a named list.
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' df <- mtcars %>%
#'  filter( carb != 6 & carb != 8 ) %>%
#'  mutate( across(c(cyl,vs,am,gear, carb), as.factor) )
#'
#' get_summarise(df$mpg, df$cyl)
#' get_summarise(mtcars$mpg, as.factor(mtcars$cyl))
#' get_summarise(mtcars$mpg, mtcars$am)
#' get_summarise(df$cyl, df$vs, runif(32,0.5,1.5) )
#

get_summarise <- function(x,y, peso, round_stats, round_p, name_x,name_y,
                          df, pairs_to_check ){
    UseMethod('get_summarise')
}


#' @rdname get_summarise
#' @export
get_summarise.default <- function(x,y,peso=NULL, round_stats=3, round_p=3, name_x=NULL,name_y=NULL){

    if(is.null(name_x)){
        name_x <- deparse( substitute(x) )
        if( grepl('$',name_x) ) name_x <- strsplit(name_x,'\\$')[[1]][2]

    }
    if(is.null(name_y)){
        name_y <- deparse( substitute(y) )
        if( grepl('$',name_x) ) name_y <- strsplit(name_y,'\\$')[[1]][2]
    }

    which_pair <- detect_pair_type(x = x, y = y)

    switch( which_pair,
            '2dummies' = summarise_fac_fac(x=x,y=y, round_p=round_p,name_x = name_x, name_y = name_y, peso = peso),
            # '2num' = association_2num_cor(x=x,y=y, name_x = name_x, name_y = name_y),
            '2fac' = summarise_fac_fac(x=x,y=y, round_p=round_p,name_x = name_x, name_y = name_y, peso = peso),
            'num_dummy' = summarise_fac_num(x=x,y=y, round_stats=round_stats,round_p=round_p,
                                            name_x = name_x, name_y = name_y),
            'num_fac' = summarise_fac_num(x=x,y=y, round_stats=round_stats,round_p=round_p,
                                          name_x = name_x, name_y = name_y, peso = peso)
    )

}


#' @rdname get_summarise
#' @export
get_summarise.data.frame <- function(df,pairs_to_check=NULL, peso = NULL,  round_stats=3, round_p=3){

    if( is.null(pairs_to_check) ){
        vars_pair <- as.data.frame(combn(names(df),2))
        vars_pair <- data.frame( 'var1' = as.character(vars_pair[1,] ),
                                 'var2' = as.character(vars_pair[2,] ))
        vars_pair$pair <- paste0(vars_pair$var1,'_',vars_pair$var2)

    } else{

        if( is.character(pairs_to_check) ){

            vars_pair <- as.data.frame(combn(pairs_to_check,2))
            vars_pair <- data.frame( 'var1' = as.character(vars_pair[1,] ),
                                     'var2' = as.character(vars_pair[2,] ))
            vars_pair$pair <- paste0(vars_pair$var1,'_',vars_pair$var2)

        } else if( is.data.frame(pairs_to_check) ){

            stopifnot( 'Data.frame must have only 2 columns' = ncol(pairs_to_check) == 2 )
            stopifnot( 'Data.frame variables must be characters' = is.character(pairs_to_check[[1]]) & is.character(pairs_to_check[[2]]) )

            vars_pair <- pairs_to_check
            names(vars_pair) <- c('var1','var2')
            vars_pair$pair <- paste0(vars_pair[[1]],'_',vars_pair[[2]])

        } else if( is.list(pairs_to_check) ){

            names_list <- names(pairs_to_check)

            var1 <- character()
            var2 <- character()

            for (i in seq_len(length(pairs_to_check) )) {

                var1_temp <- names_list[i]

                for (j in 1:length(pairs_to_check[[i]])) {

                    var2_temp <- pairs_to_check[[i]][j]

                    var1 <- c(var1,var1_temp)
                    var2 <- c(var2,var2_temp)


                }

            }
            vars_pair <- data.frame('var1'=var1,'var2'=var2)
            vars_pair$pair <- paste0(vars_pair$var1,'_',vars_pair$var2)

        }
    }

    out <- purrr::map2(
        vars_pair$var1,
        vars_pair$var2,

        function(var1,var2){

            get_summarise.default( x = df[[var1]], y = df[[var2]], peso = peso,
                                   round_stats=round_stats, round_p=round_p,
                                   name_x = var1,name_y = var2 )

        }
    )
    names(out) <- vars_pair$pair
    return(out)

}
