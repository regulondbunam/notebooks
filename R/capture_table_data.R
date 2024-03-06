#' Define function to export DataFrame to a tab-delimited text file
#'
#' This function exports a DataFrame to a text file with tab-delimited values.
#' It takes two arguments: `df`, which is the DataFrame to be exported, and `name`,
#' which is the name used to save the file.
#'
#' @param df The DataFrame to be exported.
#' @param name The name used to save the file.
#' @return Nothing (the function writes the text file)
#' @author Emanuel Pacheco Alberto
#' @examples
#' capture_table_data(my_dataframe, "output_data")
#' 
capture_table_data <- function(df, name) {
  # Define the directory where the file will be saved
  directory <- "datas/"
  
  # Construct the file path by concatenating directory, name, and ".csv" extension
  file_path <- paste0(directory, name, ".txt")
  
  # Write the DataFrame to a tab-delimited text file at the specified path
  write.table(df, file = file_path, sep = "\t", row.names = FALSE)
}
