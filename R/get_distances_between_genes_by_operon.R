#' @title get_distances_between_genes_by_operon
#' @description This function retrieves the distances between genes within
#' operons by performing two queries to a GraphQL service. The first query
#' retrieves operon names and gene names, while the second query retrieves
#' gene names and their positions. The function then combines the information
#' from both queries to calculate the distances between genes within operons.
#' @author Pina Acosta Daniel.
#' @return A dataframe with columns operon_names, genes_name,
#' leftEndPosition, rightEndPosition, and distance.
#' @examples
#' distances__between_genes_by_operon <- distribution_genes_by_operon()
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr data_frame distinct filter group_by lag mutate arrange ungroup
#' @importFrom tidyr unnest complete.cases
#' @importFrom httr POST content stop_for_status
#' @export

get_distances_between_genes_by_operon <- function(){
  #First query to obtain operon_names and genes_names
  query1 <-
    '{
  getAllOperon{
    data{
      operon{
        name
      }
      transcriptionUnits{
        genes{
          name
        }
      }
    }
  }
}'
  regulondb_conn
  req <- POST(regulondb_conn, body = list(query = query1), encode = "json")
  stop_for_status(req)
  json <- content(req, "text", encoding = "UTF-8")
  #Convert the result to a dataframe
  df <- fromJSON(json) %>% data.frame()
  # Obtaining the names of the operons
  split_operon <- unnest(df, data.getAllOperon.data.operon, .drop = TRUE)
  # To access the lists of transcription units
  split_tus <- unnest(split_operon, data.getAllOperon.data.transcriptionUnits, .drop = TRUE)
  # To obtain genes per row (the operon may repeat)
  split_genes <- unnest(split_tus, genes, .drop = TRUE)
  # To remove null values from the 'split_genes' dataframe
  genes_by_operon <- split_genes[complete.cases(split_genes$name), ]
  #change the column names
  colnames(genes_by_operon) <- c("operon_names","genes_name")
  # Remove duplicate rows
  unique_df <- distinct(genes_by_operon, operon_names, genes_name)

  #Second query to obtain gene_name and their positions
  query2 <-
    '{
  getAllGenes{
    data{
      gene{
        name
        leftEndPosition
        rightEndPosition
      }
    }
  }
}'
  regulondb_conn
  req <- POST(regulondb_conn, body = list(query = query2), encode = "json")
  stop_for_status(req)
  json <- content(req, "text", encoding = "UTF-8")
  #Convert the result to a dataframe
  df2 <- fromJSON(json) %>% data.frame()
  # Obtain the gene names and their positions
  split_genes <- unnest(df2, gene, .drop = TRUE)
  # Rename columns
  colnames(split_genes) <- c("genes_name","leftEndPosition","rightEndPosition")
  # Merge dataframes
  resultado <- merge(unique_df, split_genes, by = "genes_name")
  data <- resultado[, c("operon_names", "genes_name", "leftEndPosition", "rightEndPosition")]
  # Filter the operons with more than 1 gene and sort them in ascending order by leftEndPosition
  new_data <- data %>%
    group_by(operon_names) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    arrange(leftEndPosition)
  # Obtain the difference in distances between genes
  df <- new_data %>%
    group_by(operon_names) %>%
    mutate(distance = -(leftEndPosition - lag(rightEndPosition)))

  df <- replace(df, is.na(df), 0)

  # delete rows where 'distance' = to -1189254
  df <- df %>%
    filter(distance != -1189254)

  return(df)
}
