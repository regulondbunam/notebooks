#' Calculate Gene Intervals
#' 
#' This function retrieves gene data, calculates the size of each gene, 
#' groups the gene sizes into intervals of 100 base pairs, and counts the 
#' number of genes in each interval.
#' 
#' @return A dataframe containing intervals and the total number of genes within each interval.
#' @author Emanuel Pacheco Alberto
#' @examples
#' get_calculate_gene_intervals()
#' 
get_calculate_gene_intervals <- function() {
  # Getting query data
  query <- '
  {
    getAllGenes{
      data{
        gene{
          _id
          name
          leftEndPosition
          rightEndPosition
        }
      }
    }
  }
  '
  
  # Sending POST request to obtain data
  req <- POST(regulondb_conn, body = list(query = query), encode = "json")
  stop_for_status(req)
  
  # Extracting response in JSON format
  json <- content(req, "text", encoding = "UTF-8")
  
  # Converting JSON to a dataframe in R
  df <- fromJSON(json) %>% data.frame()
  
  # Unnesting data to obtain gene name
  df <- unnest(df, gene , .drop = TRUE)
  
  # Calculating the size of each gene and creating a new column called 'size'
  df <- df %>% 
    mutate(size = rightEndPosition - leftEndPosition + 1)
  
  # Removing null or NA/Inf values
  df <- na.omit(df)
  
  # Grouping gene sizes into intervals of 100 base pairs. Creating a new column called 'interval'
  df <- df %>% 
    mutate(interval = cut(size, 
                          breaks = seq(0, ceiling(max(size)/100)*100, by = 100), 
                          labels = paste(seq(1, ceiling(max(size)/100)*100, by = 100), 
                                         seq(100, ceiling(max(size)/100)*100, by = 100), sep = "-")))
  
  # Counting the number of genes in each interval. Creating a dataframe df_count
  df_count_interval <- df %>% 
    count(interval)
  
  # Dataframe with intervals and the total number of genes, with column renaming
  df_count_interval <- df_count_interval %>%
    rename("Size of Genes" = interval, "Total" = n)
  
  # Returning the table dataframe
  return(df_count_interval)
}



