#Classifier: Subfunction 5 of "align_query_and_bh_refs" ------------------------------------

#Combine query with ref
#' Title Combine query with ref
#'
#' @param bh_list best hit list
#' @param query_list query list
#' @param bh best hit
#' @param marker_data marker data
#' @param align alignment
#'
#' @return Aligned table
#' @export
#'
#' @examples combine_query_with_ref(bh_list,query_list,bh,marker_data,align)
combine_query_with_ref <- function(bh_list,query_list,bh,marker_data,align){

  Align_table <- c()
  Align_ID    <- 1
  for(align_id in 1:length(bh_list)){#align_id<-3
    Align_table[Align_ID] <- paste("Query: ",gsub(">","",query_list[[1]]), "; Best Hit: ", bh, "; Marker:", marker_data[align_id], sep="")
    Align_ID <- Align_ID+1
    query_align <- query_list[[align_id+1]]
    align_align <- align[[align_id]]
    besth_align <- bh_list[[align_id]]
    step_start <- 0
    if(length(query_align)>100){
      step_stop  <- 100
    }else{
      step_stop  <- length(query_align)
    }
    while(step_stop<length(query_align)){
      if(nchar(query_align[1])==1){
        q_seg <- query_align[step_start:step_stop]
        a_seg <- align_align[step_start:step_stop]
        r_seg <- besth_align[step_start:step_stop]
        q_seg <- paste(q_seg,collapse="")
        a_seg <- paste(a_seg,collapse="")
        r_seg <- paste(r_seg,collapse="")
        Align_table[Align_ID] <- q_seg
        Align_ID <- Align_ID+1
        Align_table[Align_ID] <- a_seg
        Align_ID <- Align_ID+1
        Align_table[Align_ID] <- r_seg
        Align_ID <- Align_ID+1
        Align_table[Align_ID] <- ""
        Align_ID <- Align_ID+1
        step_start <- step_stop
        step_stop  <- step_stop+100
      }else{
        q_seg <- query_align[step_start:step_stop]
        a_seg <- align_align[step_start:step_stop]
        r_seg <- besth_align[step_start:step_stop]
        q_seg <- paste(q_seg,collapse="$")
        a_seg <- paste(a_seg,collapse="$")
        r_seg <- paste(r_seg,collapse="$")
        Align_table[Align_ID] <- q_seg
        Align_ID <- Align_ID+1
        Align_table[Align_ID] <- a_seg
        Align_ID <- Align_ID+1
        Align_table[Align_ID] <- r_seg
        Align_ID <- Align_ID+1
        Align_table[Align_ID] <- ""
        Align_ID <- Align_ID+1
        step_start <- step_stop
        step_stop  <- step_stop+100
      }

    }

    if(nchar(query_align[1])==1){
      q_seg <- query_align[step_start:length(query_align)]
      a_seg <- align_align[step_start:length(query_align)]
      r_seg <- besth_align[step_start:length(query_align)]
      q_seg <- paste(q_seg,collapse="")
      a_seg <- paste(a_seg,collapse="")
      r_seg <- paste(r_seg,collapse="")
      Align_table[Align_ID] <- q_seg
      Align_ID <- Align_ID+1
      Align_table[Align_ID] <- a_seg
      Align_ID <- Align_ID+1
      Align_table[Align_ID] <- r_seg
      Align_ID <- Align_ID+1
      Align_table[Align_ID] <- ""
      Align_ID <- Align_ID+1
      Align_table[Align_ID] <- ""
      Align_ID <- Align_ID+1
    }else{
      q_seg <- query_align[step_start:length(query_align)]
      a_seg <- align_align[step_start:length(query_align)]
      r_seg <- besth_align[step_start:length(query_align)]
      q_seg <- paste(q_seg,collapse="$")
      a_seg <- paste(a_seg,collapse="$")
      r_seg <- paste(r_seg,collapse="$")
      Align_table[Align_ID] <- q_seg
      Align_ID <- Align_ID+1
      Align_table[Align_ID] <- a_seg
      Align_ID <- Align_ID+1
      Align_table[Align_ID] <- r_seg
      Align_ID <- Align_ID+1
      Align_table[Align_ID] <- ""
      Align_ID <- Align_ID+1
      Align_table[Align_ID] <- ""
      Align_ID <- Align_ID+1
    }

  }
  return(Align_table)
}

#-------------------------------------------------------------------------------
