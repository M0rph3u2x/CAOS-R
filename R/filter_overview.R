#Classifier: Subfunction 8 of "compare_barcodes_vs_query" ----------------------------------

#Filter score for hits
#' Title Filter score for hits
#'
#' @param filters Filter (sorts out nodes without matching diagnostics)
#' @param OV_score Hit score
#'
#' @return Return filtered hit score
#' @export
#'
#' @examples filter_overview(filters, score_overview)
filter_overview <- function(filters, OV_score){
  filters <- unique(filters)
  filt_ov_score <- OV_score
  for(filter in filters){
    filt_ov_score <- filt_ov_score[filt_ov_score$Score != filter,]
  }
  return(filt_ov_score)
}

#-------------------------------------------------------------------------------
