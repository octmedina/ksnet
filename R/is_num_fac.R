#' Pair of one numeric variable and one factor
#'
#' Detect if one vector is numeric and the another one a factor (or character).
#'
#' @param x A vector.
#' @param y A vector.
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' is_num_dummy(mtcars$mpg, as.factor(mtcars$vs))

is_num_fac <- function(x,y){

    is_num_fac1 <- is.numeric(x) & is.character(y)
    is_num_fac2 <- is.numeric(x) & is.factor(y)

    is_num_fac <- any(is_num_fac1,is_num_fac2)

    if( is_num_fac ){

        attr(is_num_fac,'type') <- 'num_fac'
        return(is_num_fac)
    }

    is_num_fac1 <- is.numeric(y) & is.character(x)
    is_num_fac2 <- is.numeric(y) & is.factor(x)

    is_num_fac <- any(is_num_fac1,is_num_fac2)

    if( is_num_fac ){
        attr(is_num_fac,'type') <- 'num_fac'
    }

    return(is_num_fac)

}
