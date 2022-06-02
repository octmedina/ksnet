

#' Imput missing values by variable samplingd distribution
#'
#' This function take advantage of `generate_synthetic_object` to impute
#' missing data. Read `help(generate_synthetic_object)` for more information.
#'
#'
#' @param obj A dataframe, numeric vector or character/factor vector.
#' @param seed Specify seed when replication is desired.
#'
#' @return
#' The same object without NA values as they have been imputed.
#'
#' @export
#'
#' @examples
#'
#' impute_by_sampling_distribution(c(mtcars$mpg,NA,NA,NA,NA,NA))
#'
#' data_temp <- data.frame(
#'     x = c(mtcars$mpg,NA,NA,NA,NA,NA),
#'     y = c(mtcars$cyl,NA,NA,NA,NA,NA))
#'
#' as.data.frame(impute_by_sampling_distribution(data_temp) )
#'
#' dplyr::mutate(data_temp, x_impute = impute_by_sampling_distribution(x))


impute_by_sampling_distribution <- function(obj, set_seed=NULL){

    if(anyNA(obj)==FALSE) stop('Object has no NA value')

    if( is.vector(obj) ){
        new_obj <- obj

        ind_isNA <- is.na(obj)
        n_isNA <- length(obj[ind_isNA])
        x_complete <- obj[!ind_isNA]

        values_impute <- generate_synthetic_object(x_complete, seed=set_seed, n_news = n_isNA)

        new_obj[ind_isNA] <- values_impute

    } else if( is.data.frame(obj) ){

        new_obj <- purrr::map_df(obj, impute_by_sampling_distribution, set_seed = set_seed)

    }
    return(new_obj)
}
