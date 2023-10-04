#Classifier: Subfunction 4 of "align_query_and_bh_refs" ------------------------------------

#Create alignment symbols ("|"=match , ""=mismatch)
#' Title Create alignment symbols ("|"=match , ""=mismatch)
#'
#' @param bh_list Best hit list
#' @param query_list Query list
#'
#' @return Return modified alignment
#' @export
#'
#' @examples create_align_symbols(bh_list,query_list)
create_align_symbols <- function(bh_list,query_list){
  align <- bh_list
  for(align_id in 1:length(bh_list)){#align_id<-3
    for(diag_pos in 1:length(bh_list[[align_id]])){#diag_pos <- 1
      if(bh_list[[align_id]][diag_pos] == query_list[[align_id+1]][diag_pos]){
        symb <- ""
        cas  <- strsplit(query_list[[align_id+1]][diag_pos], split = "")
        cas  <- cas[[1]]
        for(sign in 1:length(cas)){
          symb <- paste(symb,"|",sep="")
        }
        align[[align_id]][diag_pos] <- symb
      }else{
        symb <- ""
        cas  <- strsplit(query_list[[align_id+1]][diag_pos], split = "")
        cas  <- cas[[1]]
        for(sign in 1:length(cas)){
          symb <- paste(symb," ",sep="")
        }
        align[[align_id]][diag_pos] <- symb
      }
    }
  }
  return(align)
}


#-------------------------------------------------------------------------------
