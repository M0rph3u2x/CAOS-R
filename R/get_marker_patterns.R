# 7b) Define allowed patterns of characters
#' Title Define allowed patterns of characters
#'
#' @param marker_patterns Marker patterns
#' @param mark_nr Marker id
#'
#' @return Return allowed marker specific diagnostics
#' @export
#'
#' @examples get_marker_patterns(marker_patterns,mark_nr)
get_marker_patterns <- function(marker_patterns,mark_nr){
  marker_pattern  <- str_split(marker_patterns, pattern = "&")
  marker_pattern  <- marker_pattern[[1]][mark_nr] #Delist items
  marker_pattern  <- str_split(marker_pattern, pattern = "\\$") #Separate diagnostics
  marker_pattern  <- marker_pattern[[1]] #Delist items
  marker_pattern  <- toupper(marker_pattern) #Turn all letter to capital letters
  return(marker_pattern)
}
