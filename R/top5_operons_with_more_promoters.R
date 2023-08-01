#' @title Get the top 5 operons with the most promoters
#' @description This function utilizes the `get_promoters_by_operons` function to retrieve
#' promoters grouped by operons from RegulonDB.
#' A dataframe is created by grouping, summing, and sorting the data
#' in descending order to obtain the top 5 promoters by operon
#' @author Pina Acosta Daniel
#' @return A dataframe with the top 5 operons having the most promoters,
#' along with the promoter count and promoter names.
#' @examples
#' top5_operons_with_more_promoters <- top5_operons_with_more_promoters()
#' @export
  top5_operons_with_more_promoters <- function(dataframe) {
  get_promoters_by_operon <- dataframe
  # Get the top 5 operons with the most promoters
  top_5_operons_with_more_promoters <- get_promoters_by_operon %>%
    group_by(operon_names) %>%
    summarise(cantidad = n(), name_promoters = toString(promoters_name)) %>%
    arrange(desc(cantidad)) %>%
    slice(1:5)
  return(top_5_operons_with_more_promoters)
}

