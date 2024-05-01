#' Retrieve data on genes regulated by transcription units (TUs)
#' 
#' This function retrieves data on genes regulated by transcription units (TUs) 
#' from a database query and processes it into a DataFrame.
#' 
#' @return A DataFrame containing information on the total number of genes 
#' regulated by different numbers of TUs.
#' @author Emanuel Pacheco Alberto
#' @examples
#' get_tus_regulations_genes()
#' 
get_tus_regulations_genes <- function(){
  # Obtaining data from the query
  query <-'
  {
  getAllGenes{
    data{
      gene{
        _id
        name
      }
    	regulation{
        operon{
          arrangement{
            promoters{
              _id
              name
            }
            transcriptionUnit{
              _id
              name
            }
          }
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
df <- fromJSON(json) %>% data.frame()

# Unnesting data 
df_gene <- unnest(df, data.getAllGenes.data.gene, .drop = TRUE)
df_regulation <- unnest(df_gene, data.getAllGenes.data.regulation, .drop = TRUE)
df_operon <- unnest(df_regulation, operon, .drop = TRUE)
df_arrangement <- unnest(df_operon, arrangement, .drop = TRUE) 

# Unnesting the transcription units (TUs)
df_tus <- df_arrangement %>%
  unnest(cols = c(transcriptionUnit), names_sep = "_")

# Remove rows where TUs list is empty
df_tus <- df_tus %>% filter(!is.na(transcriptionUnit_name))

# Count unique TUs for each gene name
df_count_tus <- df_tus %>%
  group_by(name) %>%
  summarise(num_tus = n_distinct(transcriptionUnit_name))

# Count genes for each number of TUs
df_genes_count <- df_count_tus %>%
  group_by(num_tus) %>%
  summarise(total_genes = n())

# Convert the column "df_genes_count$num_tus" to string type
df_genes_count$num_tus <- as.character(df_genes_count$num_tus)

# Renaming columns
df_genes_count <- df_genes_count %>%
  rename(`Total of TUs for gene` = num_tus,
         `Total of Genes` = total_genes) %>% 
  select(`Total of Genes`, `Total of TUs for gene`)

# Returning the final table dataframe
return(df_genes_count)

}


