#Classifier: Subfunction 3 of "compare_barcodes_vs_query" ----------------------------------

#get diagnostics character attributes (CA) from reference barcode database
#' Title Get diagnostics character attributes (CA) from reference barcode database
#'
#' @param CA Character attributes
#' @param row Data position
#' @param character_type Diagnostic character type
#'
#' @return Return reference character attributes
#' @export
#'
#' @examples get_pos_diagnostics(CA,row,character_type)
get_pos_diagnostics <- function(CA,row,character_type){
  CA_ref   <- str_split(CA[[character_type]][row], pattern = "\\|") #Get unique characters
  CA_ref   <- CA_ref[[1]]                                           #Turn list into vector
  branch_nr<- length(CA_ref)                                        #Get number of branches in node
  CA_ref   <- chunk(CA_ref,branch_nr)                               #Turn vector into branch separated list
  CA_ref   <- str_split(CA_ref,branch_nr, pattern = "\\&")          #Get unique characters
  return(CA_ref)
}

#-------------------------------------------------------------------------------
