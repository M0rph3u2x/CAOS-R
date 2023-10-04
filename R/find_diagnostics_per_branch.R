#Barcoder: Subfunction 3 of "find_diagnostic_characters" ---------------------------------

# Find unique diagnostics for loaded node --------------------------------------
# Create a list for each branch listing all unique characters found at each
# position within the dataset
#' Title Find unique diagnostics for loaded node
#'
#' @param branch_CA Branch character attributes
#' @param branches Branch data
#'
#' @return Return list of diagnostics for loaded node
#' @export
#'
#' @examples find_diagnostics_per_branch(branch_CA,branches)
find_diagnostics_per_branch <- function(branch_CA,branches){
  branch_sep.CA <- list()
  for(branch_nr in 1:length(branches)){ #Number of branches in node           #branch_nr<-1
    CA.specimen.nr <- length(branch_CA[[branch_nr]])      #Specimen count
    CA.chars.nr    <- length(branch_CA[[branch_nr]][[1]]) #Character count
    CA.ALL         <- list()
    CA.types       <- c()
    #Compare for each position the diagnostics for all specimen of the branch
    for(char_nr in 1:CA.chars.nr){ #Number of characters
      for(spec_nr in 1:CA.specimen.nr){ #Number of specimen in branch
        CA.types <- c(CA.types, branch_CA[[branch_nr]][[spec_nr]][char_nr])
      }
      CA.ALL[[char_nr]] <- unique(CA.types) #Filter unique diagnostics for each position
      no_diagnostic     <- "#" #Delete nondiagnostic information
      CA.ALL[[char_nr]] <- CA.ALL[[char_nr]][which(CA.ALL[[char_nr]] != no_diagnostic)]
      CA.types          <- c()
    }
    branch_sep.CA[branch_nr] <- list(CA.ALL)  #Save unique diagnostics for each position and branch
  }
  info <- list(branch_sep.CA,CA.chars.nr)
  return(info)
}

#-------------------------------------------------------------------------------
