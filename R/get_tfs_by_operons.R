#' @title get_tfs_by_operons
#' @description This function queries RegulonDB to retrieve a list of
#' operons and their associated tfs. It uses the GraphQL
#' syntax to construct a query and fetches the data using the RegulonDB API.
#' The resulting tfs are returned as
#' a dataframe with two columns: operon name and tfs name.
#' @author Pina Acosta Daniel.
#' @return dataframe containing tfs grouped by operons.
#' @examples
#' tfs_by_operons <- get_tfs_by_operons()
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
get_tfs_by_operons <- function() {
  query <- '{
  getAllOperon{
    data{
      operon{
        name
      }
      transcriptionUnits{
        regulatorBindingSites{
            regulator{
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
  # To obtain transcription factors by row (the operon may repeat)
  split_tu <- unnest(split_operon, data.getAllOperon.data.transcriptionUnits, .drop = TRUE)
  # Convert the dataframes into lists
  operons_tfs <- split_tu %>%
    mutate(regulatorBindingSites = map(regulatorBindingSites, as.list))
  # Expand the lists in the 'regulatorBindingSites' column
  split_regulator <- unnest(operons_tfs, regulatorBindingSites)
  #Obtaining the names of the regulators
  split_rbs <- unnest(split_regulator, regulatorBindingSites, .drop = TRUE)
  # To remove null values from the 'split_tfs' dataframe
  tfs_by_operon <- split_rbs[complete.cases(split_rbs$name1), ]
  #change the column names
  colnames(tfs_by_operon) <- c("operon_names","tfs_name")
  return(tfs_by_operon)
}
