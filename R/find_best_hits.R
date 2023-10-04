#Classifier: Identify and save best hit reference taxa from nodes with hits ----
#' Title Identify and save best hit reference taxa from nodes with hits
#'
#' @param hit_score Hit score
#' @param ref_taxa_data Reference taxa data
#' @param unique_ref_taxa_data Unique reference taxa data
#'
#' @return Return leading taxa
#' @export
#'
#' @examples find_best_hits(hit_score,ref_taxa_data,unique_ref_taxa_data)
find_best_hits <- function(hit_score,ref_taxa_data,unique_ref_taxa_data){

  #Identify query based on score and taxa data
  nodes <- unique(hit_score$Node)

  while(length(nodes)>1){#node<-"48"

    node <- nodes[1] #Get first node in nodes vector

    hits <- hit_score[hit_score$Node==node,]

    best_score <- data.frame(position=integer(), max_score=integer())
    for(i in 1:nrow(hits)){#i<-1
      row_score <- hits$Priority_Score[i]
      row_score <- strsplit(row_score, split = "\\|")
      row_score <- as.numeric(row_score[[1]])
      max_score <- max(row_score)
      pos_score <- which(row_score==max_score)
      best_score[nrow(best_score) + 1, ] <- c(pos_score,max_score)
    }

    best_score_pos <- which(best_score$max_score==max(best_score$max_score))

    #Get first score if multiple positions exist
    best_score_pos <- best_score_pos[1]

    leading_branch <- best_score$position[best_score_pos]
    leading_marker <- hits$Marker[best_score_pos]
    leading_taxa   <- ref_taxa_data[[best_score_pos]] #Get taxa list from marker with highest score
    leading_taxa   <- leading_taxa[leading_taxa$Node_Position==node,] #Get node row from leading marker
    leading_taxa   <- strsplit(leading_taxa$Branch_Taxa[1], split = "\\|") #Split branch data
    leading_taxa   <- leading_taxa[[1]][leading_branch] #Get branch data with highest score
    leading_taxa   <- strsplit(leading_taxa, split = ",") #Split taxa names
    leading_taxa   <- leading_taxa[[1]] #Transform list into vector with separated taxa names
    leading_taxa   <- leading_taxa[leading_taxa!=""] #Delete empty cells from vector

    #End loop if only one best hit taxa remains
    if(length(leading_taxa)==1){
      break
    }

    #Find node where all/most taxa of "leading_taxa" are present (y-axis=nodes; x-axis=taxa_names)
    fit_df         <- data.frame(matrix(NA,nrow=length(nodes),ncol=1))
    match_ref_taxa <- unique_ref_taxa_data$Branch_Taxa #Extract taxa names of nodes with hits
    for(taxon in leading_taxa){
      fit_col<- c()
      for(node_taxa in match_ref_taxa){
        fit_col[length(fit_col)+1] <- grepl(taxon, node_taxa)
      }
      fit_df <- cbind(fit_df,fit_col)
    }
    fit_df <- fit_df[,-1]

    #Count "TRUE" events for each row (row=node,col=leading taxa)
    true_row<- c()
    for(row in 1:nrow(fit_df)){
      true.hits     <- which(fit_df[row,]==TRUE)
      true_row[row] <- length(true.hits)
    }
    max_hit_score <- max(true_row[2:length(true_row)])
    max_hit_pos   <- which(true_row==max_hit_score)
    next_node_id  <- last(max_hit_pos)

    #Delete all nodes before next node
    unique_ref_taxa_data <- unique_ref_taxa_data[next_node_id:length(nodes),]

    #Delete all nodes before next node
    nodes <- nodes[next_node_id:length(nodes)]
  }

  return(leading_taxa)
}

#-------------------------------------------------------------------------------
