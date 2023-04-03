

#' Conectar con una base de datos de Access
#'
#' Primera función que hay que aplicar para importar una base de datos
#' de access. Establece la conexión con ésta.
#'
#' @param filename ruta del archivo. Character vector.
#'
#' @return La conexión. Un objeto de tipo "RODBC".
#' @export
#'
#' @examples
#'
#' library(RODBC)
#' # ruta de la base de datos
#' db <- "C:/Users/34673/Downloads/Liquidaciones2021.mdb"
#' # generamos la conexión
#' con <- access_import(db)
#' con
access_import <- function(filename){
    db <- filename
    RODBC::odbcConnectAccess2007(db)
}



#' Tablas disponibles en Access
#'
#' Ver el nombre de las tablas disponibles en la base de datos Access
#'
#'
#' @param con La conexión previamente establecida con \code{access_import}
#'
#' @return Un vector con el nombre de las tablas
#' @export
#'
#' @examples
#' library(RODBC)
#' # ruta de la base de datos
#' db <- "C:/Users/34673/Downloads/Liquidaciones2021.mdb"
#' # generamos la conexión
#' con <- access_import(db)
#'
#' # vemos qué tablas hay:
#' access_searchTables(con)
#'
access_searchTables <- function(con){
    dplyr::pull( RODBC::sqlTables(con,tableType = 'TABLE'),
                 TABLE_NAME )
}



#' Importar una tabla de Access
#'
#' Importar una tabla de una base de datos Access ya conectada.
#'
#' @param con La conexión. Objeto de tipo "RODBC".
#' @param tableName El nombre de la tabla.
#'
#' @return Una matriz de datos de tipo tibble.
#' @export
#'
#' @examples
#' library(RODBC)
#' # ruta de la base de datos
#' db <- "C:/Users/34673/Downloads/Liquidaciones2021.mdb"
#' # generamos la conexión
#' con <- access_import(db)
#'
#' # vemos qué tablas hay:
#' access_searchTables(con)
#'
#' # importamos varias bases de datos:
#' tb_CCAA <- access_selectTable(con, 'tb_CCAA')
#' tb_CCAA
#'
#' tb_Provincias <- access_selectTable(con, 'tb_Provincias')
#' tb_Provincias
#'
access_selectTable <- function (con, tableName) {
    # cuando el nombre de la tabla tiene espacios hay que ponerlo dentro de ``
    tableName <- ifelse( grepl(' ',tableName), paste0('`',tableName,'`'),tableName )
    # print(tableName)
    dplyr::as_tibble(RODBC::sqlQuery(con, paste0("SELECT * FROM ",tableName)))
}


#' Cerrar la conexión de Access
#'
#' Cerrar la conexión de Access
#'
#' @return Se cierra la conexión.
#' @export
#'
#' @examples
#' con <- access_import(db)
#' access_closeConnection()
#'
access_closeConnection <- function(){
    RODBC::odbcCloseAll()
}


