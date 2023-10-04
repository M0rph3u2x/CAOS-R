#Classifier: Subfunction 1 of "compare_barcodes_vs_query" ----------------------

#Load reference barcode location for each marker
#' Title Load reference barcode location for each marker
#'
#' @param marker_data Marker data
#' @param ref_node Reference node
#' @param node Node data
#'
#' @return Return barcode location
#' @export
#'
#' @examples get_barcode_location(marker_data,ref_node,node)
get_barcode_location <- function(marker_data,ref_node,node){
  barcode_location <- c()
  for(marker_nr in 1:length(marker_data)){#marker_nr<-1
    pos      <- str_locate(ref_node[[marker_nr]], as.character(node)) #locate path with "Node" string
    pos      <- which(pos[,1] != "NA")       #get position of ref_data in dataframe
    if(length(pos)>=1){
      barcode_location[marker_nr] <- ref_node[[marker_nr]][pos]
    }else{
      barcode_location[marker_nr] <- NA
    }
  }
  return(barcode_location)
}

#-------------------------------------------------------------------------------
