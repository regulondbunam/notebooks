#' Get general statistics about genes from RegulonDB
#'
#' This function retrieves general statistics about genes from RegulonDB
#' including total number of genes, types of genes, total number of gene products,
#' types of gene products, genes with known regulation, genes in operons, and
#' types of gene strands.
#'
#' @return A dataframe containing the general statistics about genes
#' @author Emanuel Pacheco Alberto
#' @examples
#' get_gene_general_statistics()
#' 
get_gene_general_statistics <- function() {
  
  # Function to remove null values 
  remove_null <- function(df, column) {
    df %>%
      filter(!is.na({{column}}))
  }
  
  # Function to remove null values and empty lists
  remove_null_empty_lists <- function(df, column) {
    df %>%
      filter(!is.na({{column}}), lengths({{column}}) > 0)
  }
  
  # Function to count and group by column
  count_and_group <- function(df, column) {
    df %>%
      group_by({{ column }}) %>%
      summarise(Total = n(), .groups = "drop") %>%
      rename(Description = {{ column }})
  }
  
  # Function to count, group by, and rename null values
  count_group_and_rename_null <- function(df, column, description) {
    df %>%
      group_by({{column}}) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(Description = if_else(is.na({{column}}), paste0("--- ", description),
                                   paste0("--- ", as.character({{column}})))) %>%
      select(Description, Total)
  }
  
  
  # Function to create a dataframe with predefined description
  create_predefined_dataframe <- function(description, total) {
    data.frame(Description = description, Total = total)
  }
  
  # Obtaining data from the query
  query <-'
  {
  getAllGenes{
    data{
      gene{
        _id
        name
        strand
        type
      }
      products{
        _id
        type
      }
      regulation{
        regulators{
          _id
          name
        }
        operon{
          _id
          name
          arrangement{
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

# Sending the POST request to fetch the data
req <- POST(regulondb_conn, body = list(query = query), encode = "json")
stop_for_status(req)

# Extracting the JSON response
json <- content(req, "text", encoding = "UTF-8")

# Converting JSON to a dataframe in R
df <- fromJSON(json) %>% data.frame()

# Unnesting the regulation.regulators column
df_gene <- unnest(df, data.getAllGenes.data.gene, .drop = TRUE)
df_products <- unnest(df, data.getAllGenes.data.products, .drop = TRUE)
df_regulation <- unnest(df, data.getAllGenes.data.regulation, .drop = TRUE)
df_operon <- unnest(df_regulation, operon, .drop = TRUE)
df_tu_gene_regulation <- unnest(df_gene, data.getAllGenes.data.regulation, .drop = TRUE)
df_tu_gene_operon <- unnest(df_tu_gene_regulation, operon, .drop = TRUE)
df_tu_gene_arrangement <- unnest(df_tu_gene_operon, arrangement, .drop = TRUE)

# ------------------------------------------------------------------------------
# Rows 1 to 5

# Create a new dataframe for total number of genes, statement, and response
df_table_gene <- create_predefined_dataframe("Total of genes in the chromosome",
                                             nrow(df_gene))
# Add announcement for gene types
df_table_gene <- rbind(df_table_gene, c("Type of genes:", ""))

# Group by "type" column and count to get gene types, also, null grouping
df_type_gene <- count_group_and_rename_null(df_gene, type, "genes")

# ------------------------------------------------------------------------------
# Rows 6 to 12

# Create a new dataframe for total number of gene products
df_table_products <- create_predefined_dataframe("Total of genes products",
                                                 nrow(df_products))

# Add announcement for types of gene products
df_table_products <- rbind(df_table_products, c("Type of genes products:", ""))

# Group by "type" column and count to get types of gene products, also, null grouping
df_type_products <- count_group_and_rename_null(df_products, type, "polypeptides")

# ------------------------------------------------------------------------------
# Row 13

# Filter out NULL or empty lists in regulators and create a new dataframe
df_regulation_regulators <- remove_null_empty_lists(df_regulation, regulators)

# Create a new dataframe for genes with known regulation
df_gene_regulation <- create_predefined_dataframe("Genes with known regulation",
                                                  nrow(df_regulation_regulators))

# ------------------------------------------------------------------------------
# Row 14

# Remove null values in operons by the name column
df_operon_name <- remove_null(df_operon, name)

# Create a new dataframe for genes in operons
df_gene_operon <- create_predefined_dataframe("Genes in operons",
                                              nrow(df_operon_name))
# ------------------------------------------------------------------------------

# Row 15 Genes in Transcription Unit

# Remove null values in TUs by the name column
df_tu_name <- remove_null(df_tu_gene_arrangement, transcriptionUnit$name)

# Group by "gene$name" column and count
df_gene_tu <- count_and_group(df_tu_name, name)

# Create a new dataframe for total number of genes, statement, and response
df_table_tu <- create_predefined_dataframe("Genes in transcription unit",
                                             nrow(df_gene_tu))

# ------------------------------------------------------------------------------

# Rows 16 and 17

# Remove null values in genes by the strand column
df_gene_strand <- remove_null(df_gene, strand)

# Group by "strand" column and count of the types in strand
df_type_strand <- count_and_group(df_gene_strand, strand)

#Renaming the categories in the "Description" column based on certain conditions
df_type_strand <- df_type_strand %>%
  mutate(Description = case_when(
    Description == "forward" ~ "Genes in forward strand",
    Description == "reverse" ~ "Genes in reverse strand",
    TRUE ~ as.character(Description)
  ))

# ------------------------------------------------------------------------------

# Dataframe with the result of the combination of final dataframes
df_result <- rbind(df_table_gene, df_type_gene, df_table_products, 
                   df_type_products, df_gene_regulation,df_gene_operon, 
                   df_table_tu, df_type_strand)

return(df_result)

}

