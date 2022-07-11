#' Pair of dummy variables
#'
#' Detect if two vetors are both dummies.
#'
#' @param x A vector.
#' @param y A vector.
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' is_2dummies(mtcars$vs, mtcars$am)

is_2dummies <- function(x,y){

    out <- length(unique(x)) == 2 & length(unique(y)) == 2

    if(out){
        attr(out,'type') <- '2dummies'
    }

    return(out)
}
