library(dplyr)
#' @title get_genes_by_tfs.
#' @description This function queries RegulonDB to retrieve a list of
#' tfs and their associated genes. It uses the GraphQL
#' syntax to construct a query and fetches the data using the RegulonDB API.
#' The resulting genes are returned as
#' a dataframe with two columns: tfs name and gene name.
#' @author Sergio Valle Mondragon
#' @return dataframe containing genes grouped by tfs.
#' @examples
#' get_genes_by_tf <- get_genes_by_tfs()
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
get_genes_by_tf <- function() {
  # Escribimos la consulta que queremos realizar
  query <- paste0('{
  getRegulonBy(advancedSearch:"transcriptionFactor[regulator.type]"){
    data{
      regulator{
        name
        type
      }
      regulates{
        genes{
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
  #### the json is converted to df ###
  df <- fromJSON(json) %>% data.frame()
  split_regulates <- unnest(df, data.getRegulonBy.data.regulates, .drop = TRUE)
  split_regulator <- unnest(split_regulates,data.getRegulonBy.data.regulator, .drop = TRUE)
  split_genes <- unnest(split_regulator, genes, .drop = TRUE)
  genes_by_tf <- split_genes[complete.cases(split_genes), ]
  colnames(genes_by_tf) <- c("trascriptionFactor_name","regulator_type","genes_name")
  return(genes_by_tf)

}





