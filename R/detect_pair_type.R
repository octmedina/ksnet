
#' Type of pair of variables
#'
#' Detect the type of a pair of variabes.
#'
#' @param x A vector.
#' @param y A vector.
#'
#' @return A character vector of length 1. Values can be '2dummies','2num',
#' '2fac','num_dummy' or 'num_fac'.
#'
#' @export
#'
#' @examples
#'
#' detect_pair_type(as.factor(mtcars$carb),as.factor(mtcars$cyl)) # '2fac'
#' detect_pair_type(mtcars$am,  mtcars$vs) # '2dummies
#' detect_pair_type(mtcars$mpg, mtcars$disp) # '2num'
#' detect_pair_type(mtcars$mpg, mtcars$vs) # 'num_dummy'
#' detect_pair_type(mtcars$mpg, as.factor(mtcars$cyl)) # 'num_fac'

detect_pair_type <- function(x,y){

    out <- is_2dummies(x=x,y=y)
    if(out) return(attr(out,'type'))

    out <- is_num_dummy(x=x,y=y)
    if(out) return(attr(out,'type'))

    out <- is_2num(x=x,y=y)
    if(out) return(attr(out,'type'))

    out <- is_2fac(x=x,y=y)
    if(out) return(attr(out,'type'))

    out <- is_num_fac(x=x,y=y)
    if(out) return(attr(out,'type'))

}

