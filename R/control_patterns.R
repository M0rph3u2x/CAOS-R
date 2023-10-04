# 7c) Barcoder: Control if patterns in data are matched by allowed patterns
#' Title Control if patterns in data are matched by allowed patterns
#'
#' @param marker.CAs Marker character attributes
#' @param marker_pattern Allowed diagnostics
#'
#' @return Return only character attributes that match reference diagnostics (Marker Patterns)
#' @export
#'
#' @examples control_patterns(marker.CAs, marker_pattern)
control_patterns <- function(marker.CAs, marker_pattern){
  taxa_nr <- length(marker.CAs)
  CA_nr   <- length(marker.CAs[[1]])
  for(taxa_id in 1:taxa_nr){
    for(CA_id in 1:CA_nr){
      CA <- marker.CAs[[taxa_id]][CA_id]
      switch <- CA %in% marker_pattern #Check if character/pattern is allowed
      if(!(switch)){#mask if FALSE
        marker.CAs[[taxa_id]][CA_id] <- "#"
      }
    }
  }
  return(marker.CAs)
}
