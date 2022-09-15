

#' Estimate the differences in means between each level of a factor
#'
#' Carry out a multiple comparison test. By default Holm method is used.
#'
#' @param df A data.frame
#' @param pairs_to_check A named list. Each element a character vector with factor names. Each element name is the name of the numeric variable.
#' @param pesos Optional. A vector of weights.
#' @param ... Other arguments passed for \code{modelbased::estimate_contrasts}
#'
#' @return
#' A list.
#'
#' @export
#'
#' @examples
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#' df$gear <- as.factor(df$gear)
#' comparison <- list('mpg'  = c('cyl','gear'),
#'                    'disp' = c('cyl','gear') )
#'
#' association_multiple_dif_test( df,pairs_to_check = gear )

association_multiple_dif_test <- function( df, pairs_to_check, pesos = NULL,... ){

  stopifnot('pairs_to_check must be a list' = is.list(pairs_to_check) )

  if( is.null(pesos) ) pesos <- rep(1,nrow(df))

  purrr::map(
    seq_len( length( pairs_to_check ) ), function(i){

      vd_temp <- names(pairs_to_check)[i]
      vars_temp <- pairs_to_check[[i]]

      purrr::map(vars_temp, function(VI){
        mod_temp <- lm( as.formula( paste0( vd_temp,'~',VI ) ), data = df, weights = pesos )

        as_tibble(modelbased::estimate_contrasts(mod_temp, contrast = VI, ...  ) ) %>%
          mutate( 'num' = vd_temp, 'fac' = VI, .before = 1 ) %>%
          mutate( is_sign = p <= 0.05 ) %>%
          rename( 'dif' = Difference, 'p.value' =  p )

       }) %>%
        purrr::set_names(vars_temp)
  }) %>%
    purrr::set_names( names(pairs_to_check) )
}


