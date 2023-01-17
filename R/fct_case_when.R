

#' Obtain a factor after a \code{case_when}
#'
#' The output of the \code{case_when} function is a character, which means that when
#' you want to work with factors you have to apply the \code{factor} function afterwards.
#'
#' With this function the usual \code{case_when} procedure is followed but a factor is
#' obtained with the levels ordered according to the appearance in
#' the \code{case_when} input.
#'
#' @param ... Input of a \code{case_when}.
#'
#' @return A factor vector.
#' @export
#'
#' @examples
#'
#' # Comparing the outputs of the two functions:
#'
#' library(dplyr)
#'
#' as_tibble(mtcars) %>%
#'     select( cyl ) %>%
#'     mutate(
#'      cyl_case_when = case_when(
#'          cyl == 4 ~ 'Bajo',
#'          cyl == 6 ~ 'Medio',
#'          cyl == 8 ~ 'Alto' ),
#'
#'      cyl_fct_case_when = fct_case_whe
#'          cyl == 4 ~ 'Bajo'
#'          cyl == 6 ~ 'Medio'
#'          cyl == 8 ~ 'Alto' )
#'      )

#'
fct_case_when <- function(...) {
    args <- as.list(match.call())
    levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
    levels <- levels[!is.na(levels)]
    factor(dplyr::case_when(...), levels=levels)
}



