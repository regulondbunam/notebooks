#' @title Generation of a scatter plot
#' @description This function uses the ggplot2 library to generate
#' a scatter plot based on data from a dataframe.
#' It allows specifying the columns for the x and y axes,
#' as well as the titles for the axes and the plot.
#' @param df The dataframe containing the data for the plot.
#' @param column_x The name of the column to be used on the x-axis.
#' @param column_y The name of the column to be used on the y-axis.
#' @param x_axis_title The title for the x-axis.
#' @param y_axis_title The title for the y-axis.
#' @param plot_title The title for the plot.
#'
#' @return A scatter plot generated using ggplot2.
#'
#' @examples
#' Example 1: Generate a scatter plot of the number of genes by operons
#' generate_graphic_points_genes_by_operon <- generate_graphic_points(get_general_frequency_genes,cantidad,n_operon,"genes by operon","operon frequency","Genes by operon")
#'
#' Example 2: Generate a scatter plot of the number of genes by tfs
#' generate_graphic_points_genes_by_tfs <- generate_graphic_points(get_general_frequency_genes_by_tf,	n_tf, cantidad, "Number regulated genes ", "Number of tf","Number of genes regulated by tf")
#'
#'
#'
#' @import ggplot2
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @export
generate_graphic_points <- function(df, column_x, column_y, x_axis_title, y_axis_title, plot_title) {
  graph <- ggplot(data = df, aes(x = {{column_x}}, y = {{column_y}})) +
    geom_point(color = "blue") +
    labs(x = x_axis_title, y = y_axis_title, title = plot_title) +
    theme(plot.title = element_text(hjust=0.5))
  print(graph)
}
