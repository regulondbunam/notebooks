#' @title get_tus_by_operons
#' @description This function queries RegulonDB to retrieve a list of
#' operons and their associated tus. It uses the GraphQL
#' syntax to construct a query and fetches the data using the RegulonDB API.
#' The resulting tus are returned as
#' a dataframe with two columns: operon name and tus name.
#' @author Pina Acosta Daniel.
#' @return dataframe containing tus grouped by operons.
#' @examples
#' tus_by_operons <- get_tus_by_operons()
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
get_tus_by_operons <- function(){
  query <- '{
  getAllOperon{
    data{
      operon{
        name
      }
      transcriptionUnits{
        name
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
  # To obtain transcription units per row (the operon may repeat)
  split_tus <- unnest(split_operon, data.getAllOperon.data.transcriptionUnits, .drop = TRUE)
  # To remove null values from the 'split_tus' dataframe
  tus_by_operon <- split_tus[complete.cases(split_tus$name), ]
  #change the column names
  colnames(tus_by_operon) <- c("operon_names","tus_name")
  return(tus_by_operon)
}
