
#' Adjust STATA objects to R format
#'
#' When importing data from Stata, some vectors take 'haven_labelled' as their \code{class} with specific attributes.
#' Given that this class (with its attributes) does not exist in R this can lead to errors or unexpected behaviours.
#'
#' This function is provided for changing the class of the object. If it is a dataframe
#' is not necessary to previously select the columns, since the function detects which
#' variables need to be modified.
#'
#' The class of the new vector is guessed.
#'
#' @param obj A dataframe or a vector
#'
#' @return The same object with its changed class.
#' @export
#'
#' @details
#' When importing .dta data, coerce functions such as \code{as.numeric} or \code{as.character} work well in principle.
#' However, if this .dta is transformed to a .rds (for memory reasons) the new vectors are intractables
#' for these functions. Thus, the function is designed to deal with any type of situation.
#'
#' @examples
#' # Import data
#' ecv2020 <- haven::read.dta('ecv2020.dta')
#' # The function detects which columns must be changed
#' ecv2020 <- check_stata_format(ecv2020)
#'
#' # It can be applied to one variable
#' ecv2020$situacion_hogar <- check_stata_format( ecv2020$situacion_hogar )
#'
#' ecv2020 <- ecv2020 %>%
#'    mutate( situacion_hogar = check_stata_format( situacion_hogar )  )

#' @rdname check_stata_format
check_stata_format <- function( obj){

    UseMethod('check_stata_format')
}

#' @rdname check_stata_format
#' @export
check_stata_format.data.frame <- function(obj){

    obj <- purrr::map_dfc( obj, function(x){
        # limpiar los attributos extraños importados de spss y stata
        x <- clean_attributes(x)

        if( class(x)[1] == "haven_labelled" ){

            x <- check_stata_format( obj = x )

        }

        return(x)

    }  )

    return(obj)


}

#' @rdname check_stata_format
#' @export
check_stata_format.default <- function(obj){

    # extraemos el vector de valores nombrado con labels y checkeamos
    # si hay labels duplicados, el output es una lista con 2 elementos,
    # los valueLabels y el segundo indicando si hay duplicados o no (NULL)
    valueLabels_dup <- check_duplicate_label(obj)

    valueLabels <- valueLabels_dup$valueLabels

    # si hay duplicados, los sacamos del vector valueLabels y generamos
    # el nuevo vector de forma diferenciada
    if( length(valueLabels_dup) == 2 ){

        valueLabels <- valueLabels[ !names(valueLabels) %in% names( valueLabels_dup$dupLabels ) ]

        new_values <- names(valueLabels)
        names(new_values) <- valueLabels

        ## añadimos el duplicado pero diferenciando las etiquetas

        new_valuesDup <- paste0( names(valueLabels_dup$dupLabels), valueLabels_dup$dupLabels )
        names( new_valuesDup ) <- valueLabels_dup$dupLabels

        ## unimos
        new_values <- c(new_values, new_valuesDup)

    } else {

        new_values <- names(valueLabels)
        names(new_values) <- valueLabels

    }

    # class(obj) <- 'character'
    attr(obj,'format.stata') <- NULL # eliminamos este atributo
    attr(obj,'display_width') <- NULL # eliminamos este atributo
    attr(obj,'pregunta') <- attr(obj,'label') # dejamos la pregunta
    attr(obj,'label') <- NULL
    attr(obj,'labels') <- NULL # eliminamos las etiquetas de los factores

    obj <- new_values[ as.character(obj) ]

    names(obj) <- NULL
    names(new_values) <- NULL

    # obj <- factor(obj, levels = new_values)
    obj <- guessAndset_class( obj, new_values )

    return( obj )

}

clean_attributes <- function(x){

    attr(x,'format.stata') <- NULL
    attr(x,'format.spss') <- NULL
    attr(x,'display_width') <- NULL # eliminamos este atributo
    attr(x,'pregunta') <- attr(x,'label')
    attr(x,'label') <- NULL

    return(x)
}



check_duplicate_label <- function(x){

    valueLabels <- attr(x,'labels')

    countLabels <- table( names( valueLabels ) )

    dupLabels <- names( countLabels[ countLabels > 1 ] )# etiquetas asociadas a más de 1 value

    out <- list( 'valueLabels' = valueLabels )

    if( length(dupLabels) == 0 ){

        out$dupLabels <- NULL

    } else{
        dup_valueLabels <- valueLabels[ grepl( dupLabels, names(valueLabels ) ) ]

        out$dupLabels <- dup_valueLabels
    }
    return(out)
}


#' Adivina y modifica la clase de un vector
#'
#' Internamente recurre a \code{vroom::guess_type}.
#'
#' @param x Un vector.
#'
#' @param setlevels Los niveles de un posible factor. Por defecto NULL.
#'
#' @return Un vector con la clase 'adivinada'.
#'
#' @export
guessAndset_class <- function(x, setlevels = NULL){

    x <- as.character(x)

    guessClass <- attr( vroom::guess_type( x ), 'class' )[1]

    out <- switch (guessClass,
                   collector_logical = as.logical(x),
                   collector_double = as.numeric(x),
                   collector_character = factor(x)
    )

    if(is.factor(out) & !is.null(setlevels) ){ levels(out) <- setlevels }

    return(out)

}


# esta funcion devolvia un error cuando habia labels repetidas
# check_stata_format <- function(obj, return_labels = TRUE){
#
#     if( !is.data.frame(obj) ){
#
#         attr_labels <- attr(obj,'labels') # extraemos los valores del vector haven_labelled
#         x_labels <- names(attr_labels) # y sus etiquetas
#         names(x_labels) <- attr_labels # creamos el named vector
#
#         # si tiene etiquetas (está pensado para el loop):
#         if( !is.null(attr(obj,'labels')) & return_labels  ){
#             obj <- x_labels[ as.character(obj) ]
#             obj <- factor(obj,x_labels)
#             names(obj) <- NULL
#         }
#         attr(obj,'format.stata') <- NULL # eliminamos este atributo
#         # attr(obj,'label') <- NULL # dejamos finalmente el de la pregunta
#         attr(obj,'labels') <- NULL # eliminamos las etiquetas de los factores
#
#         return(obj)
#
#     } else if( is.data.frame(obj) ){
#
#         for (i in seq_len(ncol(obj)) ) {
#
#             var_temp <- obj[[i]]
#             # si no es un haven_labelled al menos le quitamos la etiqueta de format.stata
#             if( class(var_temp)[1] != "haven_labelled" ){
#                 attr(var_temp,'format.stata') <- NULL
#                 obj[[i]] <- var_temp
#
#             } else{
#                 obj[[i]] <- check_stata_format(obj = var_temp, return_labels = return_labels )
#
#             }
#         }
#
#         return( obj )
#
#     }
#
# }
