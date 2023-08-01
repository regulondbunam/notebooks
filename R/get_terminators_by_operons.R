#' @title get_terminators_by_operons
#' @description This function queries RegulonDB to retrieve a list of
#' operons and their associated terminators. It uses the GraphQL
#' syntax to construct a query and fetches the data using the RegulonDB API.
#' The resulting terminators are returned as
#' a dataframe with two columns: operon name and promoter id.
#' @author Pina Acosta Daniel.
#' @return dataframe containing terminators grouped by operons.
#' @examples
#' terminators_by_operon <- get_terminators_by_operons()
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
get_terminators_by_operons <- function() {
  query <- '{
  getAllOperon{
    data{
      operon{
        name
      }
      transcriptionUnits{
        terminators{
          _id
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
  # Convert the dataframes into lists
  operons_terminators <- split_tus %>%
    mutate(terminators = map(terminators, as.list))
  # Expand the lists in the 'regulatorBindingSites' column
  promoters_by_operon <- unnest(operons_terminators, terminators, .drop = TRUE)
  #change the column names
  colnames(promoters_by_operon) <- c("operon_names","terminators_id")
  return(promoters_by_operon)
}
