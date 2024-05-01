#' Export a graphic to a PNG file
#' 
#' This function exports a graphic to a PNG file and saves it in the specified 
#' directory with the given name.
#' 
#' @param graphic The graphic to be exported.
#' @param name The name used to save the file.
#' @return Nothing. The function writes the PNG file.
#' @author Emanuel Pacheco Alberto
#' @examples
#' capture_graphic_png(my_graphic, "output_graphic")
#' 
capture_graphic_png <- function(graphic, name) {
  # Define the directory where the file will be saved
  directory <- "graphics/"
  
  # Construct the file path by concatenating directory, name, and ".png" extension
  file_path <- paste0(directory, name, ".png")
  
  # Suppress messages during the execution of ggsave()
  suppressMessages({
    # Save the graphic as a PNG file
    ggsave(filename = file_path, plot = graphic)
  })
}
