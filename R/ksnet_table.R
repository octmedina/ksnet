
#' Tabla al estilo de KSNET (con gt)
#'
#' @param df dataframe para hacer la tabla
#'
#'@export
#'
#'
#'@examples
#' x <- c("Barcelona", "Madrid", "Murcia")
#' y <-  c(115, 90, 145)
#' z  <- c(2019, 2020, 2019)
#' mis_datos <- data.frame(provincia = x, variable = y, fecha = z)
#'ksnet_table(mis_datos)
#'
ksnet_table <- function(df){
    df %>%
        gt::gt() %>%
        gt::tab_options(
            #table.background.color = "#BFBFBF",
            column_labels.background.color = "#00b2a9",
            column_labels.font.weight = "bold",
            column_labels.font.size = gt::px(10),
            table.font.size = gt::px(10),
            data_row.padding = gt::px(4),
            #table.width = gt::px(250)
        ) %>%
        gt::tab_style(
            style = list(
                gt::cell_fill("#BFBFBF")
                #cell_text(color = "white", weight = "bold")
            ),
            locations = gt::cells_row_groups()
        )
}
