% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caption_ksnet.r
\name{caption_ksnet}
\alias{caption_ksnet}
\title{Set KSNET caption}
\usage{
caption_ksnet(...)
}
\arguments{
\item{...}{a character vector with other caption information such as Source or Notes. See Details.}
}
\value{
A character vector of length two.
}
\description{
Set at the bottom-right of the plot KSNET.
}
\details{
Currently, since the double caption is a bit tricky, only works when come across with \code{theme_ksnet_light}
when more information is supplied.#'
}
\examples{

library(ggplot2)
ggplot(mtcars,aes(mpg))+
  geom_density()+
  labs( caption = caption_ksnet() )

# If more caption information if supplied, must go with theme_ksnet_light
ggplot(mtcars,aes(mpg))+
  geom_density()+
  labs( caption = caption_ksnet('Fuente: ECV') )+
  theme_ksnet_light()

}
