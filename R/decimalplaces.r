#' Number of decimas: internal
#' Internal function for generate_synthetic_object. Not exported.
#' @param x A vector
decimalplaces <- Vectorize(function(x) {
    if ((x %% 1) != 0) {
        nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
        return(0)
    }
},vectorize.args = 'x')
