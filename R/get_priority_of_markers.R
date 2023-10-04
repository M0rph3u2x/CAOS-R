#Classifier: Subfunction 1 of "index_input_data2" ------------------------------

#Get priority level for each marker
#' Title Get priority level for each marker
#'
#' @param ref_path Path to reference data
#' @param ref_marker Reference marker data
#'
#' @return Return marker priority
#' @export
#'
#' @examples get_priority_of_markers(ref_path, ref_marker)
get_priority_of_markers <- function(ref_path, ref_marker){
  marker_path     <- paste(ref_path, ref_marker, sep = "/") #create path to markers
  marker.priority <- readLines(marker_path)                 #get markers

  if(length(marker.priority)>1){#Get user defined priority
    priority <- strsplit(marker.priority[2], split = ":")
    priority <- as.numeric(priority[[1]])
  }else{
    priority        <- c()
    bigger_test     <- str_count(marker.priority, pattern = ">")
    if(bigger_test>=1){
      priority_level <- 1
      marker.prior   <- strsplit(marker.priority, split = ">") #Split markers
      marker.prior   <- marker.prior[[1]]
      for(prior in marker.prior){ #prior <- marker.prior[1]
        equal_test   <- str_count(prior, pattern = "&|=")
        if(equal_test>=1){
          priority2 <- c()
          markers   <- strsplit(prior, split = "&|=") #Split markers
          priority2[1:length(markers[[1]])] <- priority_level
          priority <- c(priority,priority2)
          priority_level <- priority_level +1
        }else{
          priority       <- c(priority,priority_level)
          priority_level <- priority_level +1
        }
      }
    }else{ #Get all markers the equal priority, if names separated by "=" or "&"
      marker_count             <- str_count(marker.priority, pattern = "&|=")+1
      priority[1:marker_count] <- 1
    }
  }

  return(priority)
}

#-------------------------------------------------------------------------------
