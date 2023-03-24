
#' Iterative Join
#'
#' The \code{*_join} functions (\code{left_join}, \code{inner_join}, etc.) only allow a
#' join between 2 datasets, but sometimes we deal with several datasets
#' that we have to join using the same identifiers.
#'
#' This function receives as input a list where each element is one of the
#' datasets, and specifying the type of join and the variable/s of join,
#' it performs the process iteratively.
#'
#' @param obj A list where each element is a dataset.
#' @param fun The join function (\code{left_join}, \code{inner_join}, etc.).
#' @param by A character vector of variables to join by.
#' @param ... Other parameters passed onto methods.
#'
#' @return
#' A tibble.
#'
#' @export
#'
#' @examples
#'
#' # Fake Data
#' library(dplyr)
#' data1 <- data.frame( 'id' = sample(1:5, 10, replace = T ),'x1' = rnorm(10) )
#' data2 <- data.frame( 'id' = sample(1:5, 10, replace = T ),'x2' = rnorm(10) )
#' data3 <- data.frame( 'id' = sample(1:5, 10, replace = T ),'x3' = rnorm(10) )
#' data4 <- data.frame( 'id' = sample(1:5, 10, replace = T ),'x4' = rnorm(10) )
#'
#' list_data <- list( data1, data2, data3, data4 )
#'
#' iterative_join(list_data, left_join, 'id')
#'
#' iterative_join(list_data, inner_join, 'id')
#'
#' iterative_join(list_data, full_join, 'id')

iterative_join <- function(obj,fun, by = NULL, ...){

    if( class(obj) != 'list' ) stop( 'Object must be a list' )

    out <- obj[[1]]

    for (i in 2:length(obj) ) {
        out <- fun( out, obj[[i]], by = by, ... )
     }
    return(out)
}



