

#
# These are the i42 palettes
#


# List of color palettes
#' @export
paletas_ksnet <- list(
  ksnet_classic = c("#00b2a9", "#f46572", "#ffc000", "#2708a0", "#a4036f")
)


# pal42 function
#' Apply an ideas42 color scheme to your plots and figures
#'
#' @return
#' @export
#'
#' @examples
#'
color_ksnet <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- paletas_ksnet[[name]]
  if (is.null(pal))
    stop("Check spelling. Palette does not exist")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop(paste0("Roses are red, violets are blue, but this palette only has ", length(pal), "items for you."))
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#printing the palette

# print.palette function
#' Prints a palette
#'
#' @return
#' @export
#'
#'
#'
print.palette <- function(x, ...) {

  dt <- data.frame(a = 1:length(x), b = rep(5, length(x)))
  pl <- ggplot2::ggplot(dt, aes(a,b,fill = factor(a)))+
    ggplot2::geom_col()+
    ggplot2::scale_fill_manual(values = x)+
    ggplot2::guides(fill = FALSE)+
    ggplot2::labs(title = attr(x, "name"))+
    ggplot2::coord_fixed(ratio = 1)+
    ggplot2::theme_void()+
    ggplot2::theme(plot.title = ggplot2::element_text( hjust = .5,
                                                       vjust = 2))
  print(pl)
}
