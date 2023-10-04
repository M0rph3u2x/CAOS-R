#Barcoder: Subfunction 2 of "compare_barcodes_vs_query" ----------------------------------

#List diagnostics for each marker
#' Title List diagnostics for each marker
#'
#' @param input.path Input data path
#' @param barcode_location Location of barcode data
#'
#' @return Return marker diagnostics
#' @export
#'
#' @examples get_marker_diagnostics(input.path,barcode_location)
get_marker_diagnostics <- function(input.path,barcode_location){
  marker_diagnostics <- list()
  for(i in 1:length(barcode_location)){#i<-2

    #Create correct path to reference barcoding tables:
    ref_path <- paste(input.path, "reference", sep = "/")
    ref_name <- barcode_location[i]
    ref_path <- paste(ref_path, ref_name, sep = "/")
    marker_diagnostics[[i]] <- read_excel(ref_path)
  }
  return(marker_diagnostics)
}

#-------------------------------------------------------------------------------
