#' @param df
#' @param pairs_to_check
#' @param return_df
#'
#' @rdname get_association
#' @export

get_association.data.frame <- function(df,pairs_to_check=NULL, return_df=FALSE){

    if( is.null(pairs_to_check) ){
        vars_pair <- as.data.frame(combn(names(df),2))
        vars_pair <- data.frame( 'var1' = as.character(vars_pair[1,] ),
                                 'var2' = as.character(vars_pair[2,] ))
        vars_pair$pair <- paste0(vars_pair$var1,'_',vars_pair$var2)

    } else{

        if( is.character(pairs_to_check) ){

            vars_pair <- as.data.frame(combn(pairs_to_check,2))
            vars_pair <- data.frame( 'var1' = as.character(vars_pair[1,] ),
                                     'var2' = as.character(vars_pair[2,] ))
            vars_pair$pair <- paste0(vars_pair$var1,'_',vars_pair$var2)

        } else if( is.data.frame(pairs_to_check) ){

            stopifnot( 'Data.frame must have only 2 columns' = ncol(pairs_to_check) == 2 )
            stopifnot( 'Data.frame variables must be characters' = is.character(pairs_to_check[[1]]) & is.character(pairs_to_check[[2]]) )

            vars_pair <- pairs_to_check
            names(vars_pair) <- c('var1','var2')
            vars_pair$pair <- paste0(vars_pair[[1]],'_',vars_pair[[2]])

        } else if( is.list(pairs_to_check) ){

            names_list <- names(pairs_to_check)

            var1 <- character()
            var2 <- character()

            for (i in seq_len(length(pairs_to_check) )) {

                var1_temp <- names_list[i]

                for (j in 1:length(pairs_to_check[[i]])) {

                    var2_temp <- pairs_to_check[[i]][j]

                    var1 <- c(var1,var1_temp)
                    var2 <- c(var2,var2_temp)


                }

            }
            vars_pair <- data.frame('var1'=var1,'var2'=var2)
            vars_pair$pair <- paste0(vars_pair$var1,'_',vars_pair$var2)

        }
    }

    out <- purrr::map2(
        vars_pair$var1,
        vars_pair$var2,

        function(var1,var2){

            ksnet::get_association.default(x = df[[var1]], y = df[[var2]], name_x = var1,name_y = var2)

        }
    )
    names(out) <- vars_pair$pair

    if( return_df ) out <- dplyr::bind_rows(out)

    return(out)

}
