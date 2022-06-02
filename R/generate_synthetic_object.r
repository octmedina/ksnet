
#' Generate synthetic vectors from their sampling distributions
#'
#' One way of creating synthetic data is replicating sampling distribution
#' of the variable. This function can be applied to dataframes, numeric or
#' character/factor vectors and obtain an object of equal length with synthetic values.
#'
#' @param obj A dataframe, numeric vector or character/factor vector.
#' @param seed Specify seed when replication is desired.
#' @param n_news Length of the new vectors. By default, same length as input.
#'
#' @return
#' An object of equal dimensions (if `n_news` is not specified) with synthetic values.
#'
#' @export
#'
#' @examples
#' generate_synthetic_object(mtcars)
#'
#' generate_synthetic_object(mtcars$mpg)
#'
#' generate_synthetic_object(as.factor(mtcars$cyl))


generate_synthetic_object <- function(obj, seed = NULL, n_news = NULL){
    UseMethod('generate_synthetic_object')
}



#' @rdname generate_synthetic_object
#' @export

generate_synthetic_object.default <- function(obj, seed = NULL, n_news = NULL ){
    # for character and factor vectors
    if(is.null(n_news)) n_news <- length(obj)

    set.seed(seed)
    new_values <- sample( obj, n_news, TRUE )

    if( is.factor(obj) ){
        new_values <- factor(new_values, levels = levels(obj) )
    }

    return(new_values)
}


#' @rdname generate_synthetic_object
#' @export

generate_synthetic_object.numeric <- function(obj, seed = NULL, n_news = NULL){

    if(is.null(n_news)) n_news <- length(obj)

    set.seed(seed)
    ind <- sample(obj,size = round(0.1*length(obj)))
    n_decimals <- max(decimalplaces(ind))

    dd <- density(obj, from = min(obj), to = max(obj))

    set.seed(seed)
    new_values <- approx(x = cumsum(dd$y)/sum(dd$y),
                         y = dd$x,
                         xout = runif(n_news))$y
    new_values <- round( new_values, n_decimals )

    return(new_values)
}


#' @rdname generate_synthetic_object
#' @export
generate_synthetic_object.data.frame <- function(obj, seed = NULL, n_news = NULL){
    synthetic_obj <- vector('list', ncol(obj))
    names(synthetic_obj) <- names(obj)

    for (i in seq_len(ncol(obj))) {

        var_temp <- obj[[i]]

        synthetic_obj[[i]] <- generate_synthetic_object( obj = var_temp, seed = seed )

    }
    synthetic_obj <- dplyr::bind_cols(synthetic_obj)

    return(synthetic_obj)
}


















