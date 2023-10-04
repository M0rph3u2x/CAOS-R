#Classifier: Subfunction 4 of "compare_barcodes_vs_query" ----------------------------------

#Find query match with simple pure/private diagnostic
#' Title Find query match with simple pure/private diagnostic
#'
#' @param CA_query Character attribute query data
#' @param ref Reference data
#' @param match Match
#' @param match_control Match control
#' @param points Points
#'
#' @return Return matches between query and reference data
#' @export
#'
#' @examples find_match_query_vs_ref(CA_query,ref,match,match_control,points)
find_match_query_vs_ref <- function(CA_query,ref,match,match_control,points){

  #Find query match with simple pure diagnostic
  point_pos <- grep(CA_query,ref, fixed=TRUE)
  if(length(point_pos)>=1){
    match[point_pos]         <- match[point_pos]+points
    match_control[point_pos] <- match_control[point_pos]+points
  }

  m_return <- list(match,match_control)
  return(m_return)
}

#-------------------------------------------------------------------------------
