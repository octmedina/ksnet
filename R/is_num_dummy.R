#' Pair of one numeric variable and one dummy
#'
#' Detect if one vector is numeric and the another one a dummy.
#'
#' @param x A vector.
#' @param y A vector.
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' is_num_dummy(mtcars$mpg, mtcars$vs)
#'
is_num_dummy <- function(x,y){

    is_numdummy <- is.numeric(x) & is_dummy(y)

    if( is_numdummy ){
        attr(is_numdummy,'type') <- 'num_dummy'
        return(is_numdummy)
    }

    is_numdummy <- is.numeric(y) & is_dummy(x)

    if( is_numdummy ){
        attr(is_numdummy,'type') <- 'num_dummy'
    }

    return(is_numdummy)

}
