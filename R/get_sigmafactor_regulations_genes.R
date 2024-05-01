#' Retrieve data on genes regulated by sigma factors
#' 
#' This function retrieves data on genes regulated by sigma factors from a database query 
#' and processes it into a DataFrame. It then calculates the total number of genes regulated 
#' by each sigma factor and returns this information.
#' 
#' @return A DataFrame containing information on the total number of genes regulated by 
#' different sigma factors.
#' @author Emanuel Pacheco Alberto
#' @examples
#' get_sigmafactor_regulations_genes()
#' @export
get_sigmafactor_regulations_genes <- function() {
  # Database query
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
          sigmaFactors
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
df <- fromJSON(json) %>% data.frame() # Result: 4746

# Unnesting the regulation.regulators column
df_gene <- unnest(df, data.getAllGenes.data.gene, .drop = TRUE)
df_regulation <- unnest(df_gene, data.getAllGenes.data.regulation, .drop = TRUE)
df_statistics <- unnest(df_regulation, statistics, .drop = TRUE)

# Removing NULL values and 0s
df_statistics_clean <- df_statistics %>%
  filter(!is.na(sigmaFactors) & sigmaFactors != 0)

# Creating a new table with the number of the sigma factor, count of associated genes
df_count_sigmaFactors <- df_statistics_clean %>%
  group_by(sigmaFactors) %>%
  summarise(count_regulators = n())

# Convert the column "sigmaFactors" to string type
df_count_sigmaFactors$sigmaFactors <- as.character(df_count_sigmaFactors$sigmaFactors)

# Renaming the column headers and reordering columns
df_count_sigmaFactors <- df_count_sigmaFactors %>%
  rename(`Total of sigma factors regulating genes` = sigmaFactors,
         `Total of Genes` = count_regulators) %>%
  select(`Total of Genes`, `Total of sigma factors regulating genes`)

# Convert the column "Total of sigma factors regulating genes" to a factor with the desired order.
df_count_sigmaFactors$`Total of sigma factors regulating genes` <-
  factor(df_count_sigmaFactors$`Total of sigma factors regulating genes`,
         levels = unique(df_count_sigmaFactors$`Total of sigma factors regulating genes`))

# Returning the table dataframe
return(df_count_sigmaFactors)
}
