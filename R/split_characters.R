# 7) Barcoder: Split data into single diagnostic characters or patterns (CAs) --
# 7a) Separate single/multi-character patterns
#Classifier: Subfunction 1 of "create_query_list" ------------------------------

#' Title Separate single/multi-character patterns
#'
#' @param marker_seqs Marker sequence
#'
#' @return Return character attributes for each marker
#' @export
#'
#' @examples split_characters(marker_seqs)
split_characters <- function(marker_seqs){
  marker.CAs   <- c()
  dollar_check <- sum(str_count(marker_seqs, pattern = "\\$"))
  if(dollar_check>=1){#Separate character patterns (more then one character)
    for(i in 1:length(marker_seqs)){#i<-1
      marker.CAs[i]   <- str_split(marker_seqs[i], pattern = "\\$")
    }
  }else{#Separate single characters (single character)
    for(i in 1:length(marker_seqs)){
      marker.CAs[i] <- strsplit(marker_seqs[i], "")
    }
  }
  return(marker.CAs)
}

#-------------------------------------------------------------------------------
