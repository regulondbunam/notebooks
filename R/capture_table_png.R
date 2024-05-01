#' Capture and save a PNG image of a DataTable
#'
#' This function captures and saves a PNG image of a DataTable generated from a dataframe.
#' It calculates the height of the table based on the number of rows and adds additional
#' height for the header. The resulting image is saved in the "images" directory with the
#' specified name.
#'
#' @param df The DataFrame.
#' @param name The name used to save the file.
#' @return Nothing. The function saves the PNG image.
#' @author Emanuel Pacheco Alberto
#' @examples
#' capture_table_png(get_gene_general_statistics(), "gene_statistics")
#' 
capture_table_png <- function(df, name) {
  
  # Get the name of the dataframe
  df_name <- deparse(substitute(df))
  
  # Get the number of rows in the dataframe
  num_rows <- nrow(df)
  
  # Calculate the total height of the table including header and additional padding
  total_height <- (num_rows * 35) + 47 + 8
  
  # Round the total height
  vheight <- round(total_height)
  
  # Create the file name for HTML and PNG
  file_name <- paste0("images/", name)
  
  # Define the list of dataframe names for comparison
  special_dfs <- c("get_promoters_regulations_genes", "get_tus_regulations_genes",
                   "get_sigmafactor_regulations_genes")
  
  # Check if the dataframe name is in the list of special dataframes
  if (df_name %in% special_dfs) {
    # Create the DataTable with special options
    dt_options <- list(
      dom = 't',
      ordering = FALSE,
      lengthMenu = list(c(-1)),
      columnDefs = list(
        list(targets = "_all", className = "dt-center")
      )
    )
  } else {
    # Create the DataTable with default options
    dt_options <- list(
      dom = 't',
      ordering = FALSE,
      lengthMenu = list(c(-1))
    )
  }
  
  # Create the DataTable with the selected options
  dt <- datatable(df, rownames = FALSE, options = dt_options)
  
  # Save the DataTable as an HTML file
  htmltools::save_html(dt, file = paste0(file_name, ".html"))
  
  # Capture an image of the HTML table and save it as a PNG file
  webshot2::webshot(url = paste0(file_name, ".html"), 
                    file = paste0(file_name, ".png"), delay = 4,
                    cliprect = "viewport", vwidth = 992, vheight = vheight)
  
}

