% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access.R
\name{access_import}
\alias{access_import}
\title{Conectar con una base de datos de Access}
\usage{
access_import(filename)
}
\arguments{
\item{filename}{ruta del archivo. Character vector.}
}
\value{
La conexión. Un objeto de tipo "RODBC".
}
\description{
Primera función que hay que aplicar para importar una base de datos
de access. Establece la conexión con ésta.
}
\examples{

library(RODBC)
# ruta de la base de datos
db <- "C:/Users/34673/Downloads/Liquidaciones2021.mdb"
# generamos la conexión
con <- access_import(db)
con
}
