#' Convertir un objeto table creado con la función summary() a un data.frame
#' 
#' @description
#' Esta función convierte un objeto table creado con la función summary() a un data.frame en formato largo
#' Esto nos permite trabajar con los estadísticos descriptivos de una o varias variables utilizando los métodos para data.frame de R Base o del tidyverse.
#' 
#' @param t table creado con la función summary aplicada sobre un data.frame
#' @returns Un df en formato largo con tres columnas: variable (nombre de cada columna para la cual hacemos un estadístico), estadístico y valor.
#' @examples
#' df <- data.frame(a = c(1,2,3,4), b = c(5,6,7,8))
#' sum <- summary(df)
#' tabla_resumen(sum)

tabla_resumen <- function(t){
  t <- as.data.frame(t)
  t <- t[,-1]
  colnames(t) <- c("variable", "statistic")
  t <- t |> 
    dplyr::mutate("value" = {stringr::str_extract(statistic,":(.*)") |> 
        stringr::str_remove(":") |> 
        stringr::str_trim(side = "both")},
        "statistic" = {stringr::str_remove(statistic,":(.*)") |> 
            stringr::str_trim(side = "both")})
  
  return(t)
}

