#Classifier: Subfunction 1 of "align_query_and_bh_refs" ------------------------

#Extract best hit (bh) names
#' Title Extract best hit (bh) names
#'
#' @param query_nr Query number (ID)
#' @param best_hits Best hits
#'
#' @return Best hit names
#' @export
#'
#' @examples extract_bh_names(query_nr,best_hits)
extract_bh_names <- function(query_nr,best_hits){
  bh_names <- best_hits$Hits[query_nr]
  bh_names <- strsplit(bh_names, split = ",")
  bh_names <- bh_names[[1]]
  return(bh_names)
}

#-------------------------------------------------------------------------------
