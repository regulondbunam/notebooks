#' Retrieve data on biological process classes by genes
#' 
#' This function retrieves data on biological process classes associated with genes 
#' from a database query and processes it into a DataFrame. It then counts the number 
#' of genes associated with each biological process class and returns this information.
#' 
#' @return A DataFrame containing information on the total number of genes associated 
#' with different biological process classes.
#' @author Emanuel Pacheco Alberto
#' @examples
#' get_class_biologicalprocess_by_genes()
get_class_biologicalprocess_by_genes <- function() {
  # Database query
  query <-'
  {
getAllGenes{
  data{
    gene{
      _id
      name
    }
    products{
      geneOntologyTerms{
        molecularFunction{
          _id
          name
        }
        cellularComponent{
          _id
          name
        }
        biologicalProcess{
          _id
          name
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

# Extracting and processing the JSON response
json <- content(req, "text", encoding = "UTF-8")
df <- fromJSON(json) %>% data.frame()

# Unnesting data 
df_gene <- unnest(df, data.getAllGenes.data.gene, .drop = TRUE)
df_products <- unnest(df_gene, data.getAllGenes.data.products, .drop = TRUE)
df_geneOntologyTerms <- unnest(df_products, geneOntologyTerms, .drop = TRUE)

# Remove rows where biologicalProcess list is empty
df_biologicalProcess <- df_geneOntologyTerms %>% filter(lengths(biologicalProcess) > 0)

# Unnest the biologicalProcess list
df_biologicalProcess <- df_biologicalProcess %>%
  unnest(cols = c(biologicalProcess), names_sep = "_")

# Count genes and group by biologicalProcess name
df_group_biologicalProcess <- df_biologicalProcess %>%
  group_by(biologicalProcess_name) %>%
  summarise(count_genes = n())

# Sort the count_genes column from highest to lowest
df_group_biologicalProcess <- df_group_biologicalProcess %>%
  arrange(desc(count_genes))

# Prioritize the levels of 'biologicalProcess_name' in the order they appear in the dataframe
df_group_biologicalProcess$biologicalProcess_name <- factor(df_group_biologicalProcess$biologicalProcess_name,
                                                            levels = df_group_biologicalProcess$biologicalProcess_name)

# Rename columns
df_group_biologicalProcess <- df_group_biologicalProcess %>%
  rename("Class Biological Process" = biologicalProcess_name, 
         "Total of Genes" = count_genes)

# Return the final table dataframe
return(df_group_biologicalProcess)
}


