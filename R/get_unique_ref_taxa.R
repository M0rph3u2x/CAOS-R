#Classifier: Get unique taxa data summary --------------------------------------------------
#' Title Get unique taxa data summary
#'
#' @param ref_taxa_data Reference taxa data
#'
#' @return Return unique taxon data
#' @export
#'
#' @examples get_unique_ref_taxa(ref_taxa_data)
get_unique_ref_taxa <- function(ref_taxa_data){
  unique_ref_taxa_data <- unique(bind_rows(ref_taxa_data))
  unique_ref_taxa_data <- unique_ref_taxa_data[order(unique_ref_taxa_data$Node_Position),]
  return(unique_ref_taxa_data)
}

#-------------------------------------------------------------------------------
