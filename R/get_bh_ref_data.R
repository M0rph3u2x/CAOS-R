#Classifier: Subfunction 2 of "align_query_and_bh_refs" ------------------------------------

#Create list with reference marker data
#' Title Create list with reference marker data
#'
#' @param bh Best hit
#' @param ref_data Reference data
#'
#' @return Return best hit data
#' @export
#'
#' @examples get_bh_ref_data(bh, ref_data)
get_bh_ref_data <- function(bh, ref_data){
  #Get position of best hit (bh)
  bh_data_pos <- grepl(bh, ref_data[[1]])
  bh_list     <- list() #Create list with reference marker data
  for(m_id in marker_data){
    bh_list[length(bh_list)+1] <- ref_data[[m_id]][bh_data_pos]
  }
  return(bh_list)
}

#-------------------------------------------------------------------------------
