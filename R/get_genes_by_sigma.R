#' @title get_sigma_factor
#' @description This function queries RegulonDB to retrieve a list of values 
#' such as the factor name, gene name and its synonym.
#' It uses GraphQL to build a query and retrieve the data using the RegulonDB API.
#' The resulting data is returned as a data frame with three columns: 
#' sigma factor name, gene name, and its synonym.
#' @author Israel Cigarrero Salgado
#' @return Dataframe that contains the name of the sigma factor, 
#' name of the gene and its synonym.
#' @examples
#' get_sigma_factor <- get_sigma_factor()
get_sigma_factor <- function() {
  # Consulta a realizar
  query <- '
   {
  getAllSigmulon { 
    data {
      sigmaFactor {
        abbreviatedName
        gene {
          name
        }
        synonyms
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
  
  # Obtaining the name of the sigma factor, gene name and synonym.
  split_synonyms <- unnest(df, sigmaFactor, .drop = TRUE)
  split_gene <- unnest(split_synonyms, gene, .drop = TRUE)
  
  # Rename the columns
  remane_genes <- split_gene %>% 
    rename('Sigma Factor Name' = abbreviatedName, 'Gene Name' = name, Synonyms = synonyms)
  
  # Sort by the name of the sigma factor
  remane_genes <- remane_genes[order(remane_genes$`Sigma Factor Name`, decreasing = FALSE), ]
  
  return(remane_genes)
}
