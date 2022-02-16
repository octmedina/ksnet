
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ksnet

<!-- badges: start -->
<!-- badges: end -->

El objetivo del paquete `ksnet` es simplificar las rutinas de trabajo
analítico de KSNET. El objetivo es incluir elementos como:

-   Gráficos sencillos para análisis exploratorio de datos usando
    `ggplot2`
-   Funciones auxiliares para la limpieza y análisis de datos
-   Plantillas para presentaciones, informes y resúmenes ejecutivos
-   Plantillas para gráficas
-   Funciones para ayudar a exportar datos y resultados en formatos
    accesibles
-   etcétera

## Instalación

Se puede instalar la versión en desarrollo desde
[GitHub](https://github.com/) con:

``` r
# install.packages("devtools")
devtools::install_github("octmedina/ksnet")
```

## Gráficos

Este es un ejemplo básico que muestra cómo resolver un problema común:
hacer una gráfica con datos. Empecemos con un scatterplot y un
histograma.

``` r
library(ksnet)

## Plotting a histogram of penguin bill length

ksnet_hist(penguins, bill_length_mm) 
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> Warning: Removed 2 rows containing non-finite values (stat_bin).
```

<img src="man/figures/README-plots1-1.png" width="100%" />

``` r
## Simple scatterplot

ksnet_scatter(penguins, bill_length_mm, flipper_length_mm) 
#> Warning: Removed 2 rows containing missing values (geom_point).
```

<img src="man/figures/README-plots2-1.png" width="100%" />

## Themes

También podemos utilizar `themes`, o plantillas de gráficas.

``` r
library(ggplot2)

ksnet_scatter(penguins, bill_length_mm, flipper_length_mm) + 
  labs(title = "This is a sample plot",
       subtitle = "And this is the subtitle") +
  theme_ksnet()
#> Warning: Removed 2 rows containing missing values (geom_point).
```

<img src="man/figures/README-themes1-1.png" width="100%" />

## Paletas de colores

Por último, hemos creado paletas de colores. Por ahora hay una:
`ksnet_classic`. Para visualizarla, basta con utilizar la función
`color_ksnet`.

``` r
color_ksnet("ksnet_classic")
#> Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
#> "none")` instead.
```

<img src="man/figures/README-color1-1.png" width="100%" />

Este es el aspecto que tiene cuando lo combinamos con una graáfica.

``` r
ggplot(penguins, aes(bill_length_mm, flipper_length_mm, color = species)) + 
  geom_point() +
  labs(title = "This is a sample plot",
       subtitle = "And this is the subtitle") +
  theme_ksnet() +
  scale_color_manual(values = color_ksnet("ksnet_classic"))
#> Warning: Removed 2 rows containing missing values (geom_point).
```

<img src="man/figures/README-color2-1.png" width="100%" />
