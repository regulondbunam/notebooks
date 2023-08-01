#' @title get_ri_by_operons
#' @description This function queries RegulonDB to retrieve a list of
#' operons and their associated ri. It uses the GraphQL
#' syntax to construct a query and fetches the data using the RegulonDB API.
#' The resulting ri are returned as
#' a dataframe with two columns: operon name, id, ri name.
#' @author Pina Acosta Daniel.
#' @return dataframe containing ri grouped by operons.
#' @examples
#' ri_by_operons <- get_ri_by_operons()
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
get_ri_by_operons <- function(){
  query <- '{
  getAllOperon{
    data{
      operon{
        name
      }
      transcriptionUnits{
        regulatorBindingSites{
          regulatoryInteractions{
            function
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
  # To access the lists of transcription units
  split_tus <- unnest(split_operon, data.getAllOperon.data.transcriptionUnits, .drop = TRUE)
  # Convert the dataframes into lists
  operons_ri <- split_tus %>%
    mutate(regulatorBindingSites = map(regulatorBindingSites, as.list))
  # Expand the lists in the 'regulatorBindingSites' column
  split_regulatorBS <- unnest(operons_ri, regulatorBindingSites)
  # To access the lists of regulatoryInteractions
  split_ri <- unnest(split_regulatorBS, regulatorBindingSites, .drop = TRUE)
  #
  split_rBS <- unnest(split_ri, regulatorBindingSites, .drop = TRUE)
  # Remove NULL
  ri_by_operon <- split_rBS[complete.cases(split_rBS$`function`), ]
  colnames(ri_by_operon) <- c("operon_names","ris_name")
  return(ri_by_operon)
}

