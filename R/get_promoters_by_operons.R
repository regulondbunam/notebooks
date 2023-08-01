#' @title get_promoters_by_operons
#' @description This function queries RegulonDB to retrieve a list of
#' operons and their associated promoters. It uses the GraphQL
#' syntax to construct a query and fetches the data using the RegulonDB API.
#' The resulting promoters are returned as
#' a dataframe with two columns: operon name and promoter name.
#' @author Pina Acosta Daniel.
#' @return dataframe containing promoters grouped by operons.
#' @examples
#' promoters_by_operon <- get_promoters_by_operons()
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
get_promoters_by_operons <- function() {
  query <- '{
  getAllOperon{
    data{
      operon{
        name
      }
      transcriptionUnits{
        promoter{
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
  # To obtain promoters per row (the operon may repeat) by accessing transcription units
  split_tus <- unnest(split_operon, data.getAllOperon.data.transcriptionUnits, .drop = TRUE)
  # Obtaining promoters name
  split_promoters <- unnest(split_tus, promoter, .drop = TRUE)
  # To remove null values from the 'split_promotores' dataframe
  promoters_by_operon <- split_promoters[complete.cases(split_promoters$name1), ]
  #change the column names
  colnames(promoters_by_operon) <- c("operon_names","promoters_name")
  return(promoters_by_operon)
}
