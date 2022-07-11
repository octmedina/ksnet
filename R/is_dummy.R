
#' Dummy vectors
#'
#' Detect if a vetor only has 2 unique values, regardless of being numeric, factor or logical.
#'
#' @param x A vector.
#'
#' @return A logical value.
#' @export
#'
#' @examples
#'
#' is_dummy(mtcars$vs)

is_dummy <- function(x){
    length(unique(x)) == 2
}
