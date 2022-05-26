
#' Web Scraping of Catastro
#'
#' This function is designed to extract the information related to
#' real estates from Catastro, for example [this](https://www1.sedecatastro.gob.es/CYCBienInmueble/OVCListaBienes.aspx?rc1=9797905&rc2=VK3799F).
#'
#' The data related to the
#'
#' @param url A character vector of variable length with the url.
#' @param ID An optional character vector with the unique identification of each real state. For example, its name or address.
#' @param i An internal argument when url vector is of length greater than 1. Do not use this parameter.
#' @param verbose If TRUE, the iteration number is printed on the console. Default set to FALSE.
#'
#' @return
#'  A list with two elements: the first one with the information of the main real state (`parcela`).
#'  In the second element is the information regarding real states associated with the main one (`inmuebles`).
#' @export
#'
#' @examples
#'
#' # Real examples
#'
#' url_1 <- "https://www1.sedecatastro.gob.es/CYCBienInmueble/OVCListaBienes.aspx?rc1=9797905&rc2=VK3799F"
#' url_2 <- "https://www1.sedecatastro.gob.es/CYCBienInmueble/OVCListaBienes.aspx?rc1=2831834&rc2=VK4723B"
#'
#' address_1 <- 'CL ARMENTEROS 44'
#' address_2 <- 'CL ABTAO 30'
#'
#' # web scraping the information of the first one:
#'
#' scrape_catastro( url_1, address_1 )
#'
#' # web scraping the information of the two webpages:
#'
#' urls <- c(url_1,url_2)
#' address <- c(address_1,address_2)
#'
#' scrape_catastro( urls, address, verbose = TRUE )
#'
scrape_catastro <- function(url, ID=NA_character_, i = NULL, verbose = FALSE){

    if( all( c('rvest','dplyr') %in% (.packages()) ) == FALSE ){

        stop('rvest and dplyr packages must be loaded')
    }

    if( length(url) == 1 ){


        if( !is.null(i) & verbose ) print(paste0('Iteration: ', i))

        html_temp <- read_html(url)
        html_temp_body <- html_temp %>%
            html_element('body')

        ## extraer la información de la parcela catastral
        info_parcela_rf <- html_temp_body %>%
            html_element(css = '.panel-heading.amarillo') %>%
            html_text2() %>%
            strsplit(' ') %>%
            unlist()

        parcela <- html_temp_body %>%
            html_element(
                xpath ='//*[@id="ctl00_Contenido_HtmlTodos"]/div/div[2]/div[5]/div' ) %>%
            html_text2() %>%
            strsplit('\n\n') %>%
            unlist()

        df_parcela <- tibble( 'referencia_catastral' = info_parcela_rf[3],
                              'tipo_parcela' = parcela[1],
                              'localizacion' = parcela[2],
                              'superfice' = parcela[3] ) %>%
            mutate( 'ID' = ID, categoria = 'parcela', .before = 1 )

        ## extraer la información de los inmuebles relacionados con la parcela
        inmuebles_temp <- html_temp %>%
            html_element('body') %>%
            html_elements( css=".panel.panel-default") %>%
            html_text2()

        df_inmuebles <- purrr::map_df(inmuebles_temp, clean_info_inmuebles) %>%
            mutate( 'ID' = ID, categoria = 'inmueble', .before = 1 )

        list_return <- list('parcela'=df_parcela,'inmuebles'=df_inmuebles )

     } else {

       list_return <-  purrr::pmap(
           list( url, ID, seq_along(url) ),

           scrape_catastro, verbose = verbose
       )

    }
    class(list_return) <- c('catastro','list')

    return(list_return)

}



#' clean_info_inmuebles
clean_info_inmuebles <- function(x){

    referencia_catastral <- stringr::str_sub(x,1,20)
    # eliminamos la rf
    info <-stringr::str_sub(x,22,nchar(x))
    # separamos direcccion y de info
    info_split <- strsplit(info,'\n')[[1]]
    # dir inmueble
    dir_inmueble <- info_split[1]
    # separamos la info
    info_inmueble <- strsplit(info_split[2],' \\| ')[[1]]

    df_info <- tibble(
        'referencia_catastral' = referencia_catastral,
        'dir_inmueble' = dir_inmueble,
        'uso' = info_inmueble[1],
        'superficie' = info_inmueble[2],
        'coef_participacion' = info_inmueble[3],
        'any_construccion' = info_inmueble[4],
    )
    return(df_info)
}
