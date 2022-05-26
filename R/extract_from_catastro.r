

#' Extract data from catastro object
#'
#' After scraping data with `scrape_catastro` is obtained an object of class catastro which
#' is a list with at least two elements (`parcelas` and `inmuebles`). With this function
#' that information can be translated into a tibble: both the `parcelas` and `inmuebles`
#' as well as both
#'
#' @param list_scrape A catastro object (the output of `scrape_catastro`).
#' @param which Options: 'parcelas', 'inmuebles' or c('parcelas','inmuebles').
#'
#' @return
#'
#' A tibble with the specified information.
#'
#' @export
#'
#' @examples
#'
#' # Real example
#'
#' urls <- c("https://www1.sedecatastro.gob.es/CYCBienInmueble/OVCListaBienes.aspx?rc1=9797905&rc2=VK3799F",
#'           "https://www1.sedecatastro.gob.es/CYCBienInmueble/OVCListaBienes.aspx?rc1=2831834&rc2=VK4723B")
#'
#' address <- c('CL ARMENTEROS 44','CL ABTAO 30')
#'
#' # web scraping:
#' example_catastro <- scrape_catastro(urls,address)
#'
#' # extract information:
#'
#' extract_from_catastro(example_catastro, which = 'inmuebles')
#'
#' extract_from_catastro(example_catastro, which = 'parcelas')
#'
#' extract_from_catastro(example_catastro, c('inmuebles','parcelas'))
#'

extract_from_catastro <- function(list_scrape, which = c('parcelas','inmuebles')){

    if( class(list_scrape)[1] != 'catastro' ){
        stop('list_scrape must be an object of class catastro')
    }

    match.arg(which,  c('parcelas','inmuebles'),several.ok = TRUE )

    if( length(which) > 1 ){
       dplyr::arrange(
           dplyr::bind_rows(
               dplyr::bind_rows( purrr::map(list_scrape, 'parcela') ),
               dplyr::bind_rows( purrr::map(list_scrape, 'inmuebles') )
                         ), ID)
    }

    else if( which == 'parcelas' ){

        dplyr::bind_rows( purrr::map(list_scrape, 'parcela') )

    } else {
        dplyr::bind_rows( purrr::map(list_scrape, 'inmuebles') )
    }

}


