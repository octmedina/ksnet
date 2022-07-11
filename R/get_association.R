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
#'
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



