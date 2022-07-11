#' Pair of numeric variables
#'
#' Detect if two vetors are both numerics.
#'
#' @param x A vector.
#' @param y A vector.
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' is_2num(mtcars$mpg, mtcars$disp)

is_2num <- function(x,y){
    is_2_num <- is.numeric(x) & is.numeric(y)

    if(is_2_num){
        attr(is_2_num,'type') <- '2num'
    }

    return(is_2_num)
}
