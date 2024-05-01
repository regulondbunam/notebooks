#' Get Transcription Factors (TFs) Regulations and Associated Genes
#' 
#' This function retrieves data on transcription factors (TFs) regulations and 
#' their associated genes from a database using a given query.
#' 
#' @return A dataframe containing information about TFs, their regulations, 
#' and associated genes.
#' @author Emanuel Pacheco Alberto
#' @examples
#' get_tfs_regulations_genes()
#' 
get_tfs_regulations_genes <- function(){
  # Obtaining data from the query
  query <-'
  {
  getAllGenes
  {
    data{
      gene{
        _id
        name
      }
      regulation{
        statistics{
          regulators
        }
      }
    }
  }
}
'
  regulondb_conn
  
  # Sending the POST request to fetch the data
  req <- POST(regulondb_conn, body = list(query = query), encode = "json")
  stop_for_status(req)
  
  # Extracting the JSON response
  json <- content(req, "text", encoding = "UTF-8")
  
  # Converting JSON to a dataframe in R
  df <- fromJSON(json) %>% data.frame() # Result: 4746
  
  # Unnesting the regulation.regulators column
  df_gene <- unnest(df, data.getAllGenes.data.gene, .drop = TRUE)
  df_regulation <- unnest(df_gene, data.getAllGenes.data.regulation, .drop = TRUE)
  df_statistics <- unnest(df_regulation, statistics, .drop = TRUE) # Result: 4746
  
  # Removing NULL values and 0s
  df_statistics_clean <- df_statistics %>%
    filter(!is.na(regulators) & regulators != 0)
  
  # Creating a new table with the name of the regulator, count and names of associated genes
  df_count_regulators <- df_statistics_clean %>%
    group_by(regulators) %>%
    summarise(count_regulators = n())
  
  # Convert the column "regulators" to string type
  df_count_regulators$regulators <- as.character(df_count_regulators$regulators)
  
  # Renaming the column headers and reordering columns
  df_count_regulators <- df_count_regulators %>%
    rename(`Total of TFs that regulate the gene` = regulators,
           `Total of Genes` = count_regulators) %>%
    select(`Total of Genes`, `Total of TFs that regulate the gene`)
  
  # Convert the column "Total of TFs that regulate the gene" to a factor with the desired order.
  df_count_regulators$`Total of TFs that regulate the gene` <- 
    factor(df_count_regulators$`Total of TFs that regulate the gene`,
           levels = unique(df_count_regulators$`Total of TFs that regulate the gene`))
  
  # Returning the table dataframe
  return(df_count_regulators)
}


