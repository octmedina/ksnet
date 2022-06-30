#' Generate samples from a dataset
#'
#' Generate samples of a dataset.
#'
#' @param data a dataframe
#' @param n_samples
#' @param p Numeric between 0 and 1. Percentage of observations to keep in each sample.
#' @param var_to_stratify If desired, name of variable to straty the sampling. Default NULL.
#' @param pond A numeric vector of sample weights. Default NULL.
#' @param seed Numeric.
#'
#' @return
#' A list with dataframes.
#'
#' @export
#'
#' @examples
#'
#' ecv <- read_csv('ecv.csv')
#'
#' # Make 10 samples, 90% of observations in each one and stratify by CCAA
#' ecv_samples <- make_samples(ecv, 10, .9, 'CCAA')
#'
#' # Taking into account sample weights
#' ecv_samples <- make_samples(ecv, 10, .9, 'CCAA', ecv$factor_hogar)
#'
make_samples <- function(data, n_samples, p, var_to_stratify=NULL, pond = NULL, seed=12 ){

    if( p == 0 | p == 1 ) stop('p cannot be either 0 or 1')

    if( !is.null(var_to_stratify) ){

        data$peso_temp <- data[[var_to_stratify]]

        if( is.numeric(data$peso_temp) ) data$peso_temp <- rsample::make_strata( data$peso_temp )

        if( !is.null(pond) ){

            data$peso_sample <- pond

            data <- data %>%
                add_count( peso_temp, wt = peso_sample, name = 'peso_temp' ) %>%
                mutate( peso_temp = peso_temp/nrow(.) )

        } else {
            data <- data %>%
                add_count( peso_temp, name = 'peso_temp' ) %>%
                mutate( peso_temp = peso_temp/nrow(.) )
        }
    }

    list_samples <- vector('list',n_samples)

    for (i in seq_len(n_samples)) {

        seed_temp <- as.numeric(paste0(seed,i))
        set.seed(seed_temp)

        if( !is.null(var_to_stratify) ){

            sample_temp <- data %>%
                slice_sample( prop = p, replace = FALSE, weight_by = peso_temp  )

        } else {
            sample_temp <-  data %>%
                slice_sample( prop = p, replace = FALSE  )
        }

        list_samples[[i]] <- sample_temp

    }
    names(list_samples) <- paste0('sample_',seq_len(n_samples))

    return(list_samples)
}


