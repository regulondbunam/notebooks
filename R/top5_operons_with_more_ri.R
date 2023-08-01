#' @title Get the top 5 operons with the most ri
#' @description This function utilizes the `get_ri_by_operons` function to retrieve
#' ri grouped by operons from RegulonDB.
#' A dataframe is created by grouping, summing, and sorting the data
#' in descending order to obtain the top 5 ris by operon
#' @author Pina Acosta Daniel
#' @return A dataframe with the top 5 operons having the most ri,
#' along with the ri count and ri name.
#' @examples
#' top5_operons_with_more_ri <- top5_operons_with_more_ri()
#' @export
top5_operons_with_more_ri <- function(dataframe) {
  get_ri_by_operon <- dataframe
  # Get the top 5 operons with the most ri
  top_5_operons_with_more_ri <- get_ri_by_operon %>%
    group_by(operon_names) %>%
    summarise(cantidad = n(), name_ri = toString(ris_name)) %>%
    arrange(desc(cantidad)) %>%
    slice(1:5)
  return(top_5_operons_with_more_ri)
}
