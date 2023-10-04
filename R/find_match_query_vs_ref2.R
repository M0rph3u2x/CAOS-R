#Classifier: Subfunction 5 of "compare_barcodes_vs_query" ----------------------------------

#Find query match with unique diagnostic
#' Title Find query match with unique diagnostic
#'
#' @param CA_query Character attribute query
#' @param ref Reference data
#' @param match Match
#' @param match_control Match control
#' @param points Points
#'
#' @return Return matching data between query and unique diagnostics
#' @export
#'
#' @examples find_match_query_vs_ref2(CA_query,ref,match,match_control,points)
find_match_query_vs_ref2 <- function(CA_query,ref,match,match_control,points){

  #If no sPu and sPr have been found search for unique diagnostics
  #shared between branches and different for other branches
  if(unique(match_control)[1]==0){
    #Find query match with unique diagnostics
    point_pos <- grep(CA_query,ref, fixed=TRUE) #fixed=TRUE important to test for exact match
    if(length(point_pos)>=1){
      match[point_pos] <- match[point_pos]+points
    }
  }
  return(match)
}

#-------------------------------------------------------------------------------
