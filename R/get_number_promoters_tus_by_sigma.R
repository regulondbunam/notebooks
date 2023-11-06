#' @title get_number_promoters_tus_by_sigma
#' @description This function queries RegulonDB to retrieve a list of values, 
#' such as the factor name, number of promoters, and number of associated TUs.
#' Uses GraphQL to create a query and retrieve data using the RegulonDB API.
#' The resulting data is returned as a data frame with three columns: 
#' name of the sigma factor, number of promoters and number of associated TUs.
#' @author Israel Cigarrero Salgado
#' @return Data frame containing the name of the sigma factor, number of promoters and number of associated TUs.
#' @examples
#' get_number_promoters_tus_by_sigma <- get_number_promoters_tus_by_sigma()
get_number_promoters_tus_by_sigma <- function() {
  # Consulta a realizar
  query <- '{
    getAllSigmulon {
    data {
      sigmaFactor {
        abbreviatedName
      }
      statistics {
        promoters
        transcriptionUnits
      }
      
    }
  }
  }'
  regulondb_conn
  req <- POST(regulondb_conn, body = list(query = query), encode = "json")
  stop_for_status(req)
  json <- content(req, "text", encoding = "UTF-8")
  #### From json becomes df
  df <- fromJSON(json) %>% data.frame()
  
  # Obtaining the name of the sigma factor, number of promoters and number of associated TUs.
  split_sigma <- unnest(df, data.getAllSigmulon.data.sigmaFactor, .drop = TRUE)
  split_data <- unnest(split_sigma, data.getAllSigmulon.data.statistics, .drop = TRUE)
  
  # Rename the columns
  rename_genes <- split_data %>% 
    rename('Sigma Factor Name' = abbreviatedName, 'Number Of Regulated Promoters' = promoters, 'Number TUs' = transcriptionUnits)
  
  # Sort by the name of the sigma factor
  rename_genes <- rename_genes[order(rename_genes$`Sigma Factor Name`, decreasing = FALSE), ]
  
  # Convert to dataframe
  rename_genes <- as.data.frame(rename_genes)
  
  return(rename_genes)
}
