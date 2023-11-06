#' @title get_number_genes_by_sigma
#' @description This function queries RegulonDB to retrieve a list of values 
#' such as the factor name and the number of genes associated with the factor.
#' Uses GraphQL to create a query and retrieve data using the RegulonDB API.
#' The resulting data is returned as a data frame with two columns:
#' name of the sigma factor and the number of genes associated with the factor.
#' @author Israel Cigarrero Salgado
#' @return Dataframe containing the name of the sigma factor and the number of associated genes.
#' @examples
#' get_number_genes_by_sigma <- get_number_genes_by_sigma()
get_number_genes_by_sigma <- function() {
  # Consulta a realizar
  query <- '{
    getAllSigmulon {
      data {
        sigmaFactor {
          abbreviatedName
        }
          statistics {
            genes
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
  
  # Obtaining the name of the sigma factor and the number of genes associated with the factor.
  split_sigma <- unnest(df, data.getAllSigmulon.data.sigmaFactor, .drop = TRUE)
  split_gene <- unnest(split_sigma, data.getAllSigmulon.data.statistics, .drop = TRUE)
  
  # Rename the columns
  rename_genes <- split_gene %>% 
    rename('Sigma Factor Name' = abbreviatedName, 'Number Genes' = genes)
  
  # Sort by the name of the sigma factor
  rename_genes <- rename_genes[order(rename_genes$`Sigma Factor Name`, decreasing = FALSE), ]
  
  return(rename_genes)
}
