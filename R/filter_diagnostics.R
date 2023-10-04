#Barcoder: Subfunction 1 of "format_All_CA_Overview" ---------------------------------

#' Title Filter diagnostics
#'
#' @param data Character data
#'
#' @return Return modified character data
#' @export
#'
#' @examples filter_diagnostics(CA_subset$All_Characters)
filter_diagnostics <- function(data){
  mod_data <- paste(unique(data), collapse = "&")
  mod_data <- strsplit(mod_data, split = "&")
  mod_data <- sort(unique(gsub(pattern = "&", replacement = "", mod_data[[1]])))
  if(length(which(mod_data==""))!=0){
    mod_data <- mod_data[-(which(mod_data==""))] # Delete empty cells
  }
  mod_data <- paste(mod_data, collapse = "&")
  return(mod_data)
}

#-------------------------------------------------------------------------------
