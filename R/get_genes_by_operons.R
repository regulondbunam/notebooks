#' @title get_genes_by_operons
#' @description This function queries RegulonDB to retrieve a list of
#' operons and their associated genes. It uses the GraphQL
#' syntax to construct a query and fetches the data using the RegulonDB API.
#' The resulting genes are returned as
#' a dataframe with two columns: operon name and gene name.
#' @author Pina Acosta Daniel.
#' @return dataframe containing genes grouped by operons.
#' @examples
#' genes_by_operon <- get_genes_by_operons()
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
get_genes_by_operons <- function() {
  query <-
    '{
  getAllOperon{
    data{
      operon{
        name
      }
      transcriptionUnits{
        genes{
          name
        }
      }
    }
  }
}'

  regulondb_conn
  req <- POST(regulondb_conn, body = list(query = query), encode = "json")
  stop_for_status(req)
  json <- content(req, "text", encoding = "UTF-8")
  df <- fromJSON(json) %>% data.frame()
  # Obtaining the names of the operons
  split_operon <- unnest(df, data.getAllOperon.data.operon, .drop = TRUE)
  # To access the lists of transcription units
  split_tus <- unnest(split_operon, data.getAllOperon.data.transcriptionUnits, .drop = TRUE)
  # To obtain genes per row (the operon may repeat)
  split_genes <- unnest(split_tus, genes, .drop = TRUE)
  # To remove null values from the 'split_genes' dataframe
  genes_by_operon <- split_genes[complete.cases(split_genes$name), ]
  #change the column names
  colnames(genes_by_operon) <- c("operon_names","genes_name")
  # Remove duplicate rows
  unique_df <- distinct(genes_by_operon, operon_names, genes_name)
  return(unique_df)
}
