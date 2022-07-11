#' Tidy association result
#'
#' Detect pair type and carry out the appropiate association test. It can be passed two vectors (x and y) or a dataframe.
#'
#' If the input is a dataframe, when `pairs_to_check` is not specified all possible combinations between variables
#' are taken into account. If it is a character vector of variable names, only these pair combinations will be used.
#' Besides, it can be a dataframe with two columns specifying the desired pairs. Finally, it can be a
#' named list in which each name is a variable name, and each element is a character vector with the names
#' of the variables to combine with the element name. See Examples.
#'
#' @param x A vector.
#' @param y A vector.
#' @param name_x Optional. Name of the x variable (character).
#' @param name_y Optional. Name of the y variable (character).
#' @param df A data.frame with the required data.
#' @param pairs_to_check NULL (default), a character vector, a dataframe of two columns or a named list.
#' @param return_df If TRUE, when is passed a dataframe, is returned a dataframe as well instead of a named list.
#'
#' @return A dataframe or a named list with the results.
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
#' ## VECTORS ##
#' get_association(df$mpg, df$disp)
#' get_association(df$mpg, df$am)
#'
#'
#' ## DATA.FRAMES ##
#'
#' ## all columns combination
#' get_association(df[,c(1,2,3,9)], return_df = T)
#'
#' ## only this variables combination
#' get_association(df, pairs_to_check = c('cyl','disp','gear'),return_df = F )
#'
#'
#' ## only these pairs of combinations
#' df_pairs <- data.frame( 'x'=c('cyl','disp','gear'),
#'                         'y'= c('gear','vs','mpg'))
#'
#' get_association( df, pairs_to_check = df_pairs, return_df = T )
#'
#' ## for each variable the desired combinations:
#' list_pairs <- list( 'mpg' = c('cyl','disp','vs'),
#'                     'cyl' = c('disp','vs','am')
#'  )
#'
#' get_association( df, pairs_to_check = list_pairs, return_df = T )


get_association <- function(x,y,name_x=NULL,name_y=NULL,
                            df,pairs_to_check,return_df ){
    UseMethod('get_association')
}


#' @rdname get_association
#' @export

get_association.default <- function(x,y,name_x=NULL,name_y=NULL){

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
            '2dummies' = ksnet::association_2dummy(x=x,y=y, name_x = name_x, name_y = name_y),
            '2num' = ksnet::association_2num_cor(x=x,y=y, name_x = name_x, name_y = name_y),
            '2fac' = ksnet::association_2fac(x=x,y=y, name_x = name_x, name_y = name_y),
            'num_dummy' = ksnet::association_num_dummy(x=x,y=y, name_x = name_x, name_y = name_y),
            'num_fac' = ksnet::association_num_fac(x=x,y=y, name_x = name_x, name_y = name_y)
    )
}

#' @rdname get_association
#' @export
get_association.data.frame <- function(df,pairs_to_check=NULL, return_df=FALSE){

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

            ksnet::get_association.default(x = df[[var1]], y = df[[var2]], name_x = var1,name_y = var2)

        }
    )
    names(out) <- vars_pair$pair

    if( return_df ) out <- dplyr::bind_rows(out)

    return(out)

}
