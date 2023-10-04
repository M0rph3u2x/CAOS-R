#Barcoder: Subfunction 4 of "find_diagnostic_characters" ---------------------------------

# Find unique diagnostics between branches of the selected node ------------
# Diagnostics sorted by data position and branch data separated by "|"
#' Title Find unique diagnostics between branches of the selected node
#'
#' @param branch_sep.CA branch specific character attribute data
#' @param CA.chars.nr Character attribute position
#'
#' @return Return diagnostic data sorted by nodes
#' @export
#'
#' @examples identify_unique_diagnostics(branch_sep.CA,CA.chars.nr)
identify_unique_diagnostics <- function(branch_sep.CA,CA.chars.nr){
  branch.numbers        <- length(branch_sep.CA)
  unique.CAs.branchpos  <- c()
  all.CAs.characters    <- c()
  unique.CAs.characters <- c()
  unique.CAs.Position   <- c()
  diagnostic            <-"FALSE"
  diag.more             <-"FALSE"
  counter               <- 1
  for(ca_pos in 1:CA.chars.nr){#ca_pos<-1
    unique  <-"FALSE"
    #Get all characters for each branch at selected position
    CA.list        <- c()
    diagnostic.CAs <- c()
    CA.branch      <- c()
    for(branch_nr in 1:branch.numbers){#branch_nr<-2
      value <- branch_sep.CA[[branch_nr]][[ca_pos]]
      if(length(value)>0){
        CA.list[branch_nr] <- list(branch_sep.CA[[branch_nr]][[ca_pos]])
      }else{
        CA.list[branch_nr] <- "#"
      }
    }
    #Test if diagnostic characters are present at current position
    unique.CA <- unique(c(CA.list,recursive = TRUE))
    unique.CA <- unique.CA[unique.CA!="#"]
    all.CA    <- c(CA.list,recursive = TRUE)
    if(length(unique.CA)>0){
      for(test_nr in 1:length(unique.CA)){#test_nr<-1
        test <- which(all.CA==unique.CA[test_nr])
        if(length(test)!=branch.numbers){
          diagnostic.CAs <- c(diagnostic.CAs,unique.CA[test_nr])
          unique         <- "TRUE"
        }
      }
    }

    #Save data for current position if diagnostic characters where identified
    if(unique=="TRUE"){
      all.CAs.characters[counter]   <- ""
      unique.CAs.characters[counter]<- ""
      for(branch_nr in 1:branch.numbers){#branch_nr<-2
        CA.branch[branch_nr] <- branch_nr
        this.CA              <- branch_sep.CA[[branch_nr]][[ca_pos]]
        if(length(this.CA)>0){
          for(this.CA_Nr in 1:length(this.CA)){#this.CA_Nr<-1
            for(diag.CA_NR in 1:length(diagnostic.CAs)){#diag.CA_NR<-1
              if(this.CA[this.CA_Nr]==diagnostic.CAs[diag.CA_NR]){
                if(diag.more=="FALSE"){
                  unique.CAs.characters[counter] <- paste(unique.CAs.characters[counter],diagnostic.CAs[diag.CA_NR],sep="")
                  diagnostic                     <-"TRUE"
                  diag.more                      <-"TRUE"
                }else{
                  unique.CAs.characters[counter] <- paste(unique.CAs.characters[counter],diagnostic.CAs[diag.CA_NR],sep="&")
                  diagnostic                     <-"TRUE"
                }
              }
            }
            all.CAs.characters[counter]   <- paste(all.CAs.characters[counter],this.CA[this.CA_Nr],sep="")
            if(this.CA_Nr<length(this.CA)){
              all.CAs.characters[counter] <- paste(all.CAs.characters[counter],"&",sep="")
            }
          }
        }else{
          all.CAs.characters[counter]    <- paste(all.CAs.characters[counter]   ,"-",sep="")
        }
        if(diagnostic=="FALSE"){
          unique.CAs.characters[counter] <- paste(unique.CAs.characters[counter],"-",sep="")
        }
        if(branch_nr<branch.numbers){
          unique.CAs.characters[counter] <- paste(unique.CAs.characters[counter],"|",sep="")
          all.CAs.characters[counter]    <- paste(all.CAs.characters[counter]   ,"|",sep="")
        }
        diagnostic <-"FALSE"
        diag.more  <-"FALSE"
      }

      unique.CAs.branchpos[counter] <- paste(CA.branch,collapse="|")
      unique.CAs.Position[counter]  <- ca_pos
      counter                       <- counter+1
    }
  }

  diagnostic_data <- list(unique.CAs.branchpos,
                          unique.CAs.Position,
                          all.CAs.characters,
                          unique.CAs.characters)

  return(diagnostic_data)
}

#-------------------------------------------------------------------------------
