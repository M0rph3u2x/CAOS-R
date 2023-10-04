#Classifier: Subfunction 7 of "compare_barcodes_vs_query" ----------------------------------

#Filter score for hits
#' Title Filter score for hits
#'
#' @param filters Filters (sorts out nodes without matching diagnostics)
#' @param hit_score Hit score
#'
#' @return Return filtered hit score
#' @export
#'
#' @examples filter_score(filters, hit_score)
filter_score <- function(filters, hit_score){
  filters <- unique(filters)
  filt_hit_score <- hit_score
  for(filter in filters){
    filt_hit_score <- filt_hit_score[filt_hit_score$Priority_Score != filter,]
  }
  return(filt_hit_score)
}

#-------------------------------------------------------------------------------
