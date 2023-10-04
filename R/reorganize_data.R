#Classifier: Reorganize ref_taxa based on marker hierarchy ---------------------------------
#' Title Reorganize ref_taxa based on marker hierarchy
#'
#' @param data Data
#' @param data_order Order of data
#'
#' @return Return reorganized data
#' @export
#'
#' @examples reorganize_data(data,data_order)
reorganize_data <- function(data,data_order){
  reorg_data <- c()
  for(string in data_order){
    string <- paste("/",string,"/",sep="")
    pos    <- str_locate(data, string) #locate path with string
    pos    <- which(pos[,1] != "NA")   #get position of string in vector

    #reorganize data in order of data_order strings
    reorg_data[length(reorg_data)+1] <- data[pos]
  }
  return(reorg_data)
}

#-------------------------------------------------------------------------------
