#' Retrieve data on genes regulated by promoters
#' 
#' This function retrieves data on genes regulated by promoters from a database 
#' query and processes it into a DataFrame.
#' 
#' @return A DataFrame containing information on the total number of genes 
#' regulated by different numbers of promoters.
#' @author Emanuel Pacheco Alberto
#' @examples
#' get_promoters_regulations_genes()
#' 
get_promoters_regulations_genes <- function(){
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

# Remove rows where promoters list is empty
df_promoters <- df_arrangement %>% filter(lengths(promoters) > 0)

# Unnesting the promoters list
df_promoters <- df_promoters %>%
  unnest(cols = c(promoters), names_sep = "_")

# Count unique promoters for each gene name
df_count_promoters <- df_promoters %>%
  group_by(name) %>%
  summarise(num_promoters = n_distinct(promoters_name))

# Count genes for each number of promoters
df_genes_count <- df_count_promoters %>%
  group_by(num_promoters) %>%
  summarise(total_genes = n())

# Convert the column "df_genes_count$num_promoters" to string type
df_genes_count$num_promoters <- as.character(df_genes_count$num_promoters)

# Renaming columns y select order
df_genes_count <- df_genes_count %>%
  rename(`Total of Promoters for gene` = num_promoters, 
         `Total of Genes` = total_genes) %>% 
  select(`Total of Genes`, `Total of Promoters for gene`)

# Returning the final table dataframe
return(df_genes_count)
}

