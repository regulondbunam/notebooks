#' @title Perform the row count in a column.
#' @description This function performs a count of the number of rows in the data set.
#' @author Pina Acosta Daniel, Valle Mondragon Sergio.
#' @param df The dataset being worked with.
#' @param column_name Column name from which the rows are to be counted.
#' @return Dataset with the frequency of unique rows in the dataset.
#' @examples
#' ## Connect to the RegulonDB database if necessary
#' if (!exists("regulondb_conn")) regulondb_conn <- connect_database()
#'
#' # Example 1:
#' total_tus_by_operon <- count_elements(operons_tus,operon_names,tus_name)
#'
#' # Example 2:
#' total_genes_by_tf <- count_elements(genes_by_tf, tfs_name, genes_name)
#' @import dplyr
#' @export
count_elements <- function(df, colum_name_group, column_name_summarise){
  df_count <- df %>%
    group_by({{colum_name_group}}) %>%
    summarise(column_name_summarise = n())
  df_count <- df_count %>% rename(cantidad = column_name_summarise)
  return(df_count)
}
