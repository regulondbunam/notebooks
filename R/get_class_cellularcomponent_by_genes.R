#' Retrieve data on cellular component classes by genes
#' 
#' This function retrieves data on cellular component classes associated with genes 
#' from a database query and processes it into a DataFrame. It then counts the number 
#' of genes associated with each cellular component class and returns this information.
#' 
#' @return A DataFrame containing information on the total number of genes associated 
#' with different cellular component classes.
#' @author Emanuel Pacheco Alberto
#' @examples
#' get_class_cellularcomponent_by_genes()
get_class_cellularcomponent_by_genes <- function() {
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

# Remove rows where cellularComponent list is empty
df_cellularComponent <- df_geneOntologyTerms %>% filter(lengths(cellularComponent) > 0)

# Unnest the cellularComponent list
df_cellularComponent <- df_cellularComponent %>%
  unnest(cols = c(cellularComponent), names_sep = "_")

# Count genes and group by cellularComponent name
df_group_cellularComponent <- df_cellularComponent %>%
  group_by(cellularComponent_name) %>%
  summarise(count_genes = n())

# Sort the count_genes column from highest to lowest
df_group_cellularComponent <- df_group_cellularComponent %>%
  arrange(desc(count_genes))

# Prioritize the levels of 'cellularComponent_name' in the order they appear in the dataframe
df_group_cellularComponent$cellularComponent_name <- factor(df_group_cellularComponent$cellularComponent_name,
                                                            levels = df_group_cellularComponent$cellularComponent_name)

# Rename columns
df_group_cellularComponent <- df_group_cellularComponent %>%
  rename("Class Cellular Component" = cellularComponent_name,
         "Total of Genes" = count_genes)

# Return the final table dataframe
return(df_group_cellularComponent)
}


