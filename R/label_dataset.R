

#' Añadir etiquetas las variables de un dataset
#'
#' Es habitual al importar datos de excel que una de las filas represente las etiquetas
#' (labels) de las variables. Con esta función de forma automatizada se añaden dichas
#' etiquetas a las columnas y se elimina la fila de las etiquetas. Para ello se
#' espefica el número de columna en el parámetro `labels_data`.
#'
#' No obstante, puede suceder que las etiquetas las tengamos en un vector externo
#' al dataset, que hemos creado o importado. En ese caso, en vez de especificar
#' un número, pasamos al vector con las etiquetas al parámetro `labels_data`.
#'
#'
#' Es necesario tener instalado el paquete \code{Hmisc}.
#'
#'
#' @param data Un tibble o data.frame.
#' @param labels_data Un número indicando la fila que contiene las etiquetas.
#'
#' @return
#' Devuelve un data.frame etiquetado y sin la fila de labels.
#'
#' @export
#'
#' @examples
#'
#' ## EJEMPLO 1 ##
#' # Tabla donde la primera fila contiene las etiquetas
#' df <- data.frame(
#'   'p1' = c('Sexo de la persona',     'Hombre','Mujer','Hombre'),
#'   'p2' = c('Edad de la persona',     55,18,22),
#'   'p3' = c('Ideologia de la persona',2,8,5)
#'   )
#'
#' df <- label_dataset(df,1)
#' View(df)
#'
#' ## EJEMPLO 2 ##
#'
#' # Las etiquetas las tenenemos en un vector fuera del data.frame
#' df2 <- data.frame(
#'    'p1' = c('Hombre','Mujer','Hombre'),
#'    'p2' = c(55,18,22),
#'    'p3' = c(2,8,5) )
#'
#' etiquetas <- c('Sexo de la persona','Edad de la persona','Ideologia de la persona')
#'
#' df2 <- label_dataset(df2,labels_data = etiquetas)
#' View(df2)


label_dataset <- function(data, labels_data=NULL){

    if( length(labels_data) == 1 & is.numeric(labels_data) ){

        id <- labels_data
        labels_data <- as.character(data[labels_data,]) # extraer labels
        data <- data[-id,] # quitar primera fila
    }

    # añadir labels a las variables:
    for (i in seq_along(labels_data)) {
        Hmisc::label( data[[i]] ) <- labels_data[i]
    }
    return(data)
}

