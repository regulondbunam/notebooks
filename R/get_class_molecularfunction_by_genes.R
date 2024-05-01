#' Retrieve data on molecular function classes by genes
#' 
#' This function retrieves data on molecular function classes associated with genes 
#' from a database query and processes it into a DataFrame. It then counts the number 
#' of genes associated with each molecular function class and returns this information.
#' 
#' @return A DataFrame containing information on the total number of genes associated 
#' with different molecular function classes.
#' @author Emanuel Pacheco Alberto
#' @examples
#' get_class_molecularfunction_by_genes()
get_class_molecularfunction_by_genes <- function() {
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

# Remove rows where molecularFunction list is empty
df_molecularFunction <- df_geneOntologyTerms %>% 
  filter(lengths(molecularFunction) > 0)

# Unnest the molecularFunction list
df_molecularFunction <- df_molecularFunction %>%
  unnest(cols = c(molecularFunction), names_sep = "_")

# Count genes and group by molecularFunction name
df_group_molecularFunction <- df_molecularFunction %>%
  group_by(molecularFunction_name) %>%
  summarise(count_genes = n())

# Sort the count_genes column from highest to lowest
df_group_molecularFunction <- df_group_molecularFunction %>%
  arrange(desc(count_genes))

# Prioritize the levels of 'molecularFunction_name' in the order they appear in the dataframe
df_group_molecularFunction$molecularFunction_name <- factor(df_group_molecularFunction$molecularFunction_name,
                                                            levels = df_group_molecularFunction$molecularFunction_name)

# Rename columns
df_group_molecularFunction <- df_group_molecularFunction %>%
  rename("Class Molecular Function" = molecularFunction_name, 
         "Total of Genes" = count_genes) 

# Return the final table dataframe
return(df_group_molecularFunction)
}


