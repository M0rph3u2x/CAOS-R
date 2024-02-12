#Classifier: 3) Align query and best hit references ----------------------------

#' Title Align query and best hit references
#'
#' @param query_nr Query number (ID)
#' @param best_hits List of best hit data
#' @param ref_data  List of reference data
#' @param marker_data List of markers
#' @param query_list List of query data
#' @param out.path Output path
#'
#' @return Creates table of aligned query and best hit references
#' @export
#'
#' @examples align_query_and_bh_refs(query_nr,best_hits,ref_data,marker_data,query_list,out.path)
#'
align_query_and_bh_refs <- function(query_nr,best_hits,ref_data,marker_data,query_list,out.path){

  #Extract best hit (bh) names
  bh_names <- extract_bh_names(query_nr,best_hits)


  #Create alignment for each best hit identified
  for(bh in bh_names){#bh<-bh_names[1]

    #Create list with reference marker data
    bh_list <- get_bh_ref_data(bh, ref_data,marker_data)

    #Split diagnostics
    bh_list <- split_diagnostics(bh_list)

    #Create alignment symbols ("|"=match , ""=mismatch)
    align <- create_align_symbols(bh_list,query_list)

    #Combine query with ref
    Align_table <- combine_query_with_ref(bh_list,query_list,bh,marker_data,align)

    #Print alignment in query folder
    path <- paste(out.path,"/", gsub(">","",query_list[[1]]),"/", gsub(">","",query_list[[1]]), "_vs_", bh, "_alignment.txt", sep="")
    write.table(Align_table, file = path)

  }


}

#-------------------------------------------------------------------------------
