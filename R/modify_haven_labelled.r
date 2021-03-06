
#' Adjust STATA objects to R format
#'
#' When importing data from Stata, some vectors take 'haven_labelled' as their \code{class} with specific attributes.
#' Given that this class (with its attributes) does not exist in R this can lead to errors or unexpected behaviours.
#'
#' This function is provided for changing the class of the object. If it is a dataframe
#' is not necessary to previously select the columns, since the function detects which
#' variables need to be modified.
#'
#' @param obj A dataframe or a vector
#' @param return_labels Logical. Return variable values as integers or with its labels. Default TRUE (labels).
#' @param new_class When `return_lavels` set to FALSE, The desired class of the vector/variables. Choices: 'numeric', 'character' or 'logical'. By default it is converted to numeric.
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
#' ecv2020 <- modify_haven_labelled(ecv2020)
#'
#' # It can be applied to one variable
#' ecv2020$situacion_hogar <- modify_haven_labelled( ecv2020$situacion_hogar )
#'
#' ecv2020 <- ecv2020 %>%
#'    mutate( situacion_hogar = modify_haven_labelled( situacion_hogar )  )

modify_haven_labelled <- function(obj, return_labels = TRUE){

    if( !is.data.frame(obj) ){

        attr_labels <- attr(obj,'labels')
        x_labels <- names(attr_labels)
        names(x_labels) <- attr_labels

        attr(obj,'format.stata') <- NULL
        attr(obj,'label') <- NULL
        attr(obj,'labels') <- NULL
        class(obj) <- 'numeric'

        if( return_labels ){
            obj <- x_labels[ as.character(obj) ]
            names(obj) <- NULL
        }
        return(obj)

    } else if( is.data.frame(obj) ){

        for (i in seq_len(ncol(obj)) ) {

            var_temp <- obj[[i]]

            if( class(var_temp)[1] != "haven_labelled" ){
                obj[[i]] <- var_temp
            } else{
                obj[[i]] <- modify_haven_labelled(obj = var_temp, return_labels = return_labels )

            }
        }

        return( obj )

    }

}


# FUNCION ANTERIOR SIGUIENDO LA LOGICA DE METODOS:
#' modify_haven_labelled <- function(obj,to = c('numeric','character','logical'),
#'                                   return_labels = TRUE){
#'     UseMethod('modify_haven_labelled')
#' }
#'
#'
#' #'  rdname modify_haven_labelled
#' #'  export
#' modify_haven_labelled.default <- function(obj, to = c('numeric','character','logical'),
#'                                           return_labels){
#'     convert_class(obj, to, returnLabels = return_labels)
#' }
#'
#' #' rdname modify_haven_labelled
#' #' export
#' modify_haven_labelled.data.frame <- function(obj, to = c('numeric','character','logical'),
#'                                              return_labels ){
#'     to <- match.arg(to, c('numeric','character','logical') )
#'
#'     for (i in seq_along(obj)) {
#'         if( class(obj[[i]])[1] != "haven_labelled" ){
#'             next
#'         } else{
#'             obj[[i]] <- convert_class(obj[[i]], to, returnLabels = return_labels)
#'         }
#'
#'     }
#'     return(obj)
#' }
#'
#' #'
#' convert_class <- function(x, new_class = c('numeric','character','logical'),
#'                           returnLabels ){
#'
#'     new_class <- match.arg(new_class, c('numeric','character','logical') )
#'
#'
#'     attr_labels <- attr(x,'labels')
#'     x_labels <- names(attr_labels)
#'     names(x_labels) <- attr_labels
#'
#'     attr(x,'format.stata') <- NULL
#'     attr(x,'label') <- NULL
#'     attr(x,'labels') <- NULL
#'     class(x) <- new_class
#'
#'     if( returnLabels ){
#'         x <- x_labels[ x ]
#'         names(x) <- NULL
#'     }
#'     return(x)
#' }
