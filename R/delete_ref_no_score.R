#Barcoder: Delete node data with no score ------------------------------------------------
#' Title Delete node data with no score
#'
#' @param hit_score Hit Score
#' @param ref_taxa_data Reference taxa data
#'
#' @return Modified reference taxa data
#' @export
#'
#' @examples delete_ref_no_score(hit_score,ref_taxa_data)
delete_ref_no_score <- function(hit_score,ref_taxa_data){
  #Identify query based on score and taxa data
  nodes <- unique(hit_score$Node)

  #Delete node data with no score
  for(ref_mark_id in 1:length(ref_taxa_data)){#Process each marker database
    for(ref_node in ref_taxa_data[[ref_mark_id]]$Node_Position){
      true_node  <- unique(grepl(ref_node, nodes)) #Check if node is present in hit_score
      if(TRUE %in% true_node){#next if true
        next
      }else{#delete node from taxa database if false
        del_pos       <- which(ref_taxa_data[[ref_mark_id]]$Node_Position==ref_node)
        ref_taxa_data[[ref_mark_id]] <- ref_taxa_data[[ref_mark_id]][-del_pos,]
      }
    }
  }
  return(ref_taxa_data)
}

#-------------------------------------------------------------------------------
