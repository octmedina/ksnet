

#' Get (weighted) summary statistics
#'
#' This function computes the summary statistics of `x`.
#'
#' @param df A dataframe.
#' @param var Variable name.
#' @param peso A numeric vector of weights. Default is NULL.
#'
#' @return
#' A dataframe.
#' @export
#'
#' @examples
#'
#' summary_statistics(mtcars, disp)
#'
#' mtcars %>%
#'  group_by(cyl) %>%
#'  summary_statistics(mpg)
summary_statistics <- function(df,var,peso=NULL){

    require(dplyr)

    var_name <- as_label(enquo(var))

    if(is.null(peso)){
        df %>%
            dplyr::summarise(
                'var' = var_name,
                'N' = n(),
                'Min'=min({{var}},na.rm = T),
                'Q25'=quantile({{var}},0.25),
                'Media'=mean({{var}},na.rm = T),
                'Mediana'=median({{var}},na.rm = T),
                'Q75'=quantile({{var}},0.75),
                'Max'=max({{var}},na.rm = T),
                'sd' = sd({{var}},na.rm = T),
                .groups = 'drop'
            )
    } else {

        stopifnot( 'The length of the vector of weights is not correct' = length(peso) == nrow(df) )

        df %>%
            dplyr::summarise(
                'var' = var_name,
                'N' = n(),
                'Min'= min({{var}},na.rm = T),
                'Q25'= DescTools::Quantile({{var}},peso,0.25,na.rm = T),
                'Media'= weighted.mean({{var}},peso,na.rm = T),
                'Mediana'= DescTools::Quantile({{var}},peso,0.5,na.rm = T),
                'Q75'= DescTools::Quantile({{var}},peso,0.75,na.rm = T),
                'Max' = max({{var}},na.rm = T),
                'sd' = DescTools::SD({{var}},peso,na.rm = T),
                .groups = 'drop'
            )
    }

}

