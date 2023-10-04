#Barcoder: Subfunction 2 of "find_diagnostic_characters" ---------------------------------

#Save CAs for each branch, taxa and position in list
#' Title Save CAs for each branch, taxa and position in list
#'
#' @param branches Branches
#' @param CAOS CAOS processed input database
#'
#' @return Return branch character attributes
#' @export
#'
#' @examples get_CA_per_taxa(branches,CAOS)
get_CA_per_taxa <- function(branches,CAOS){
  branch_CA <- list()
  for(branch_nr in 1:length(branches)){#branch_nr<-1
    m.label <- CAOS[[1]] %in% branches[[branch_nr]]
    CA      <- CAOS[[2]][m.label]
    branch_CA[branch_nr] <- list(CA)
  }
  return(branch_CA)
}

#-------------------------------------------------------------------------------
