#' @title Get the top 5 operons with the most tfs
#' @description This function utilizes the `get_tfs_by_operons` function to retrieve
#' tfs grouped by operons from RegulonDB.
#' A dataframe is created by grouping, summing, and sorting the data
#' in descending order to obtain the top 5 tfs by operon
#' @author Pina Acosta Daniel
#' @return A dataframe with the top 5 operons having the most tfs,
#' along with the tf count and tf names.
#' @examples
#' top5_operons_with_more_tfs <- top5_operons_with_more_tfs()
#' @export
top5_operons_with_more_tfs <- function(dataframe) {
  get_tfs_by_operon <- dataframe
  # Get the top 5 operons with the most tfs
  top_5_operons_with_more_tfs <- get_tfs_by_operon %>%
    group_by(operon_names) %>%
    summarise(cantidad = n(), name_tfs = toString(tfs_name)) %>%
    arrange(desc(cantidad)) %>%
    slice(1:5)
  return(top_5_operons_with_more_tfs)
}
