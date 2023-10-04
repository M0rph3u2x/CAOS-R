#Barcoder: Subfunction 1 of "find_diagnostic_characters" ---------------------------------

#Save taxa names in separate vectors saved in list "branches"
#' Title Save taxa names in separate vectors saved in list "branches"
#'
#' @param node Node
#'
#' @return Return branch data
#' @export
#'
#' @examples separate_branch_taxa(node)
separate_branch_taxa <- function(node){
  node_data <- paste(node,collapse=",")              #Collapse taxa names and branch elements "|"
  node_data <- str_split(node_data, pattern = "\\|") #Split string by branch elements "|"
  node_data <- node_data[[1]]                        #Reduce list to vector
  branches <- list()                                 #Create list with branch vector data of taxa names
  for(i in 1:length(node_data)){#Split taxa names as separate elements for each branch in loaded node
    branch_taxa_names <- str_split(node_data[i], pattern = ",")
    branch_taxa_names <- branch_taxa_names[[1]] #Reduce list to vector
    branch_taxa_names <- branch_taxa_names[-(which(branch_taxa_names==""))] # Delete empty cells
    branches[[i]]     <- branch_taxa_names
  }
  return(branches)
}

#-------------------------------------------------------------------------------
