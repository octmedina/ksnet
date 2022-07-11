#' @rdname get_association
#' @export

get_association.default <- function(x,y,name_x=NULL,name_y=NULL){

    if(is.null(name_x)){
        name_x <- deparse( substitute(x) )
        if( grepl('$',name_x) ) name_x <- strsplit(name_x,'\\$')[[1]][2]

    }
    if(is.null(name_y)){
        name_y <- deparse( substitute(y) )
        if( grepl('$',name_x) ) name_y <- strsplit(name_y,'\\$')[[1]][2]
    }

    which_pair <- detect_pair_type(x = x, y = y)

    switch( which_pair,
            '2dummies' = ksnet::association_2dummy(x=x,y=y, name_x = name_x, name_y = name_y),
            '2num' = ksnet::association_2num_cor(x=x,y=y, name_x = name_x, name_y = name_y),
            '2fac' = ksnet::association_2fac(x=x,y=y, name_x = name_x, name_y = name_y),
            'num_dummy' = ksnet::association_num_dummy(x=x,y=y, name_x = name_x, name_y = name_y),
            'num_fac' = ksnet::association_num_fac(x=x,y=y, name_x = name_x, name_y = name_y)
    )
}
