#' @title get_tus_by_tfs.
#' @description This function queries RegulonDB to retrieve a list of
#' tfs and their associated tus It uses the GraphQL
#' syntax to construct a query and fetches the data using the RegulonDB API.
#' The resulting genes are returned as
#' a dataframe with two columns: tus name and tfs name.
#' @author Sergio Valle Mondragon
#' @return dataframe containing tus grouped by tfs.
#' @examples
#' get_tus_by_tfs <- get_tus_by_tfs()
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
get_tus_by_tfs <- function() {
  # Escribimos la consulta que queremos realizar
  query <- paste0('{
  getRegulonBy(advancedSearch:"transcriptionFactor[regulator.type]"){
    data{
      regulator{
        name
      }
      regulates{
        transcriptionUnits{
          name
        }
      }
    }
     }
}')
  regulondb_conn
  req <- POST(regulondb_conn, body = list(query = query), encode = "json")
  stop_for_status(req)
  json <- content(req, "text", encoding = "UTF-8")
  fromJSON(json)
  #### the json is converted to df ###
  df <- fromJSON(json) %>% data.frame()
  split_regulator <- unnest(df, data.getRegulonBy.data.regulator, .drop = TRUE)
  split_regulates <- unnest(split_regulator, data.getRegulonBy.data.regulates, .drop = TRUE)
  split_transcriptionUnits <- split_regulates %>%
    mutate(transcriptionUnits = map(transcriptionUnits, as.list))
  split_transcriptionUnits2 <- unnest(split_transcriptionUnits, transcriptionUnits, .drop = TRUE)
  tus <- split_transcriptionUnits2 %>%
    unnest(transcriptionUnits)
  tus_by_tf <- tus[complete.cases(tus), ]
  num_datos_nulos <- sum(is.na(tus_by_tf))
  colnames(tus_by_tf) <- c("trascriptionFactor_name","transcriptionUnits_name")
  return(tus_by_tf)
}

