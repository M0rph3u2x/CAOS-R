#Classifier: Subfunction 3 of "align_query_and_bh_refs" ------------------------------------

#Split diagnostics
#' Title Split diagnostics
#'
#' @param bh_list List of best hits
#'
#' @return Return edited list of best hits
#' @export
#'
#' @examples split_diagnostics(bh_list)
split_diagnostics <- function(bh_list){

  for(bh_id in 1:length(bh_list)){#bh_id<-1

    #Test if data holds single or string diagnostics
    TF_switch <- grepl("\\$", bh_list[[bh_id]])
    if(TF_switch){ #Separate string diagnostics
      bh_split <- strsplit(bh_list[[bh_id]], split = "\\$")
      bh_split <- bh_split[[1]]
      for(i in 1:length(bh_split)){
        bh_list[[bh_id]][i] <- bh_split[i]
      }
    }else{ #Separate single character diagnostics
      bh_split <- strsplit(bh_list[[bh_id]], "")
      bh_split <- bh_split[[1]]
      for(i in 1:length(bh_split)){
        bh_list[[bh_id]][i] <- bh_split[i]
      }
    }
  }
  return(bh_list)
}

#-------------------------------------------------------------------------------
