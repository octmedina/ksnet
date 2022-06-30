
#' Search for interactions in linear models
#'
#' Method imported from Machine Learning techniques but with inferential statistics in mind.
#' Detect wich pair of interaction if statistically significant 'regardless' input data.
#'
#' @param data A dataframe.
#' @param vd Name of the dependent variable (character).
#' @param vars_interactions A character vector with the names of variables to interact betwean pairs.
#' @param controls A character vector with the names of variable controls (if desired) or 'all'. Default is NULL. See details for more information.
#' @param n_muestras Number of samples to take from the data. If 1 is speficied, it takes the the entire dataset.
#' @param p_muestras Proportion of observations to keep in each sample. Only if n_muestras greater than 1.
#' @param var_to_stratify Name of the variable to stratify when creating samples. Default is NULL.
#' @param peso Numeric vector with sampling weights. If NULL is used a vector of ones.
#' @param get_summary Logical. It should be returned the coefficient of every sample or just the average? Default if TRUE.
#'
#' @return
#' A list with two elements: 'outcome' is a dataframe with the results, and 'formulas' showing the formulas used.
#'
#' @details
#'
#' When specifying 'all' inf 'controls', all variables present in vars_interactions will be used in every linear model.
#'
#' @export
#'
#' @examples
#' res_temp <- lm_search_interactions( data = mtcars,
#'                                     vd = 'mpg',
#'                                     vars_interactions = c('cyl','disp','vs'),
#'                                     controls = 'all',
#'                                     n_muestras = 3,
#'                                     p_muestras = .9,
#'                                     get_summary = F)
#' res_temp$outcome
#' res_temp$formulas

lm_search_interactions <- function(data,vd,
                                   vars_interactions,
                                   controls = NULL,
                                   n_muestras = 10,
                                   p_muestras = .9,
                                   var_to_stratify = NULL,
                                   peso = NULL,
                                   get_summary=TRUE
){

    ## ajustar los pesos
    if( is.null(peso) ) data$pond <- 1
    else data$pond <- peso

    ## extraer N muestras aleatorias con el p_muestras % de los datos en cada una
    if( n_muestras > 1 ){ # cuando se especifica 1 entonces se utiliza la matriz completa

        if( !is.null(var_to_stratify) ){
            muestras <- make_samples(data, n_samples = n_muestras, p = p_muestras,var_to_stratify = var_to_stratify)
        } else {
            muestras <- make_samples(data, n_samples = n_muestras, p = p_muestras)
        }

    }

    ## Parejas de interacciones
    parejas <- as.data.frame( t(combn( vars_interactions,2 )) )
    names(parejas) <- c('var1','var2')
    parejas$interaccion <- paste0( parejas$var1,':', parejas$var2 )

    ## para cada interacción, se lleva a cabo el modelo

    res_interacciones <- vector('list',length = nrow(parejas)) # guardar output de cada interaccion
    info_formulas <- vector('list', length = nrow(parejas)) # guardar la formula empleada

    for (i in seq_len(nrow(parejas))) {

        ## extraemos variabels e interaccion para la formula
        var1_temp <- parejas$var1[i]
        var2_temp <- parejas$var2[i]

        formula_vars_temp <- paste0(var1_temp,'+',var2_temp)
        interaccion_temp <- parejas$interaccion[i]

        formula_temp <- paste0(vd,'~',formula_vars_temp,'+',interaccion_temp )

        # ajustamos los controles
        if( !is.null(controls) ){

            if( controls == 'all' ){ # usamos todas las variables del dataset
                controls_temp <- names(data)
                # quitamos la vd, las variables ya incluidas y la ponderacion
                controls_temp <- controls_temp[ !controls_temp %in% c(vd,var1_temp,var2_temp,'pond')]
                formula_controles <- paste0(controls_temp, collapse = '+')

            } else {
                controls_temp <- controls[ !controls %in% c(var1_temp,var2_temp)]
                formula_controles <- paste0(controls_temp, collapse = '+')
            }
            # actualizamos la formula con los controles
            formula_temp <- paste0(formula_temp,'+',formula_controles)
        }

        formula_temp <- as.formula(formula_temp) # pasar a formula
        info_formulas[[i]] <- formula_temp # guardar la formula

        res_modelos <- vector('list',length = n_muestras ) # guardar el output de los N modelos

        for ( m  in 1:n_muestras  ) { # para cada m

            if( n_muestras == 1 ){

                muestra_temp <- data # print('Se utiliza el dataset completo')
            }
            else{
                muestra_temp <- muestras[[m]] # extraemos los datos
            }

            mod_temp <- broom::tidy(lm( formula_temp, data = muestra_temp,
                                        weights = pond )) # pasamos a tidy

            df_coef <- mod_temp %>% # nos quedamos únicamente con la interaccion
                tidyr::separate(term, c('var1','var2'), sep = ':') %>%
                tidyr::drop_na(var2)

            res_modelos[[m]] <- df_coef

        }
        res_modelos_int <- dplyr::bind_rows(res_modelos) # se unen los N modelos

        # if TRUE se devuelve el promedio de coef,errores, t value y p value
        if(get_summary) res_modelos_int <- res_modelos_int %>%
            summarise(across(where(is.numeric),mean))


        # se añaden las variables utilizadas
        res_modelos_int <- res_modelos_int %>%
            mutate( 'var1' = var1_temp, 'var2' = var2_temp, .before = 1 ) %>%
            mutate( sign = p.value < 0.05 )

        # se guarda el resultado de cada pareja de interacciones
        res_interacciones[[i]] <- res_modelos_int
        names(info_formulas) <- parejas$interaccion
    }
    # unificamos resultados
    res_interacciones <- dplyr::bind_rows(res_interacciones)
    out <- list( 'outcome' = res_interacciones,
                 'formulas'= info_formulas )
    return(out)

}


