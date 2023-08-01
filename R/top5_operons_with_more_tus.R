#' @title Get the top 5 operons with the most tus
#' @description This function utilizes the `get_tus_by_operons` function to retrieve
#' tus grouped by operons from RegulonDB.
#' It then counts the number of tus per operon, calculates the tus frequencies,
#' and finally returns a dataframe
#' containing the top 5 operons with the highest number of tus.
#' @author Pina Acosta Daniel
#' @return A dataframe with the top 5 operons having the most tus,
#' along with the tus count and tu names.
#' @examples
#' top5_operons_with_more_tus <- top5_operons_with_more_tus()
#' @export
top5_operons_with_more_tus <- function(dataframe){
  get_tus_by_operon <- dataframe
  # Get the top 5 operons with the most tus
  top_5_operons_with_more_tus <- get_tus_by_operon %>%
    group_by(operon_names) %>%
    summarise(cantidad = n(), name_tus = toString(tus_name)) %>%
    arrange(desc(cantidad)) %>%
    slice(1:5)
  return(top_5_operons_with_more_tus)
}

