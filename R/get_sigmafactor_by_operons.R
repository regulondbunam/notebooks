#' @title get_sigmafactor_by_operons
#' @description This function queries RegulonDB to retrieve a list of
#' operons and their associated sigmafactor. It uses the GraphQL
#' syntax to construct a query and fetches the data using the RegulonDB API.
#' The resulting sigmafactor are returned as
#' a dataframe with two columns: operon name and sigmafactor name.
#' @author Pina Acosta Daniel.
#' @return dataframe containing sigmafactor grouped by operons.
#' @examples
#' sigmafactor_by_operons <- get_sigmafactor_by_operons()
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
get_sigmafactor_by_operons <- function(){
  query <- '{
  getAllOperon{
    data{
      operon{
        name
      }
      transcriptionUnits{
        promoter{
          bindsSigmaFactor{
            name
          }
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
  # To access transcriptionUnits to obtain the name of the sigma factor (the operon may repeat)
  split_tus <- unnest(split_operon, data.getAllOperon.data.transcriptionUnits, .drop = TRUE)
  # To access promoter
  split_promoter <- unnest(split_tus, promoter, .drop = TRUE)
  # To access bindsSigmaFactor
  split_binds <- unnest(split_promoter, bindsSigmaFactor, .drop = TRUE)
  # To remove null values from the 'split_binds' dataframe in the column 'name1'
  sigmafactor_by_operon <- split_binds[complete.cases(split_binds), ]
  #change the column names
  sigmafactor_by_operon <- sigmafactor_by_operon %>% rename(operon_names = name, sigmafactor_names = name1)
  return(sigmafactor_by_operon)
}
