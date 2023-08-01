#' @title Get the top 5 operons with the most genes
#' @description This function utilizes the `get_genes_by_operons` function to retrieve
#' genes grouped by operons from RegulonDB.
#' A dataframe is created by grouping, summing, and sorting the data
#' in descending order to obtain the top 5 genes by operon
#' @author Pina Acosta Daniel
#' @return A dataframe with the top 5 operons having the most genes,
#' along with the gene count and gene names.
#' @examples
#' top5_operons_with_more_genes <- top5_operons_with_more_genes()
#' @export
top5_operons_with_more_genes <- function(dataframe) {
  get_genes_by_operon <- dataframe
  # Get the top 5 operons with the most genes
  top_5_operons_with_more_genes <- get_genes_by_operon %>%
    group_by(operon_names) %>%
    summarise(cantidad = n(), name_genes = toString(genes_name)) %>%
    arrange(desc(cantidad)) %>%
    slice(1:5)
  return(top_5_operons_with_more_genes)
}
