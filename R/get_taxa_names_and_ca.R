#Barcoder: Subfunction 7 of "find_diagnostic_characters" ---------------------------------

#Get Species names in node and all characters
#' Title Get Species names in node and all characters
#'
#' @param node Nodes
#' @param CAOS CAOS input database
#'
#' @return Return taxa name and character attribute information
#' @export
#'
#' @examples get_taxa_names_and_ca(node,CAOS)
get_taxa_names_and_ca <- function(node,CAOS){
  Species.names <- c()
  Species.sequs <- c()
  name_nr       <-1

  for(name_node_nr in 1:length(node)){
    if(node[name_node_nr]!="|"){
      node.pos2              <- CAOS[[1]] %in% node[name_node_nr]
      Species.names[name_nr] <- CAOS[[1]][node.pos2]
      Species.sequs[name_nr] <- CAOS[[2]][node.pos2]
      name_nr <- name_nr+1
    }
  }
  info <- list(Species.names,Species.sequs)
  return(info)
}

#-------------------------------------------------------------------------------
