#' Join promoters and transcription units (TUs) dataframes
#' 
#' This function takes two dataframes, one containing information about promoters 
#' and the other containing information about transcription units (TUs), and joins 
#' them into a single dataframe. It renames any differing columns between the two 
#' dataframes to "Number", adds a "Type" column to each dataframe with the specified 
#' labels, and then combines them into one dataframe. Finally, it converts the "Number" 
#' column to numeric type.
#' 
#' @param df1 The dataframe containing information about promoters.
#' @param label1 The label for the Type column corresponding to df1.
#' @param df2 The dataframe containing information about transcription units (TUs).
#' @param label2 The label for the Type column corresponding to df2.
#' @return A combined dataframe containing information from both df1 and df2.
#' @author Emanuel Pacheco Alberto
#' @examples
#' combined_df <- dfs_promoters_tus_genes_joined(df_promoters, "Promoters", df_tus, "Transcription Units")
dfs_promoters_tus_genes_joined <- function(df1, label1, df2, label2){
  
  # Get column names of both dataframes
  cols_df1 <- colnames(df1)
  cols_df2 <- colnames(df2)
  
  # Identify columns that are different between the two dataframes
  diff_cols_df1 <- setdiff(cols_df1, cols_df2)
  diff_cols_df2 <- setdiff(cols_df2, cols_df1)
  
  # Rename differing columns to "Number"
  df1 <- df1 %>%
    rename_with(~"Number", .cols = all_of(diff_cols_df1))
  
  df2 <- df2 %>%
    rename_with(~"Number", .cols = all_of(diff_cols_df2))
  
  # Add a "Type" column to each dataframe with the specified labels
  df1 <- df1 %>%
    mutate(Type = label1)
  
  df2 <- df2 %>%
    mutate(Type = label2)
  
  # Combine the two dataframes into one
  combined_df <- rbind(df1, df2)
  
  # Convert the "Number" column to numeric type
  combined_df <- combined_df %>%
    mutate(Number = as.numeric(Number))
  
  # Return the combined dataframe
  return(combined_df)
  
}

