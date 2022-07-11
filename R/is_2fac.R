#' Pair of factor variables
#'
#' Detect if two vetors are both factors (or characters).
#'
#' @param x A vector.
#' @param y A vector.
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' is_2fac(as.factor(mtcars$cyl), as.factor(mtcars$gear))
#'
#' is_2fac(as.character(mtcars$cyl), as.character(mtcars$gear))

is_2fac <- function(x,y){

    is_2fac <- is.character(x) & is.character(y)

    if(is_2fac){
        attr(is_2fac,'type') <- '2fac'
        return(is_2fac)
    }

    is_2fac <- is.factor(x) & is.factor(y)

    if(is_2fac){
        attr(is_2fac,'type') <- '2fac'
    }

    return(is_2fac)
}
