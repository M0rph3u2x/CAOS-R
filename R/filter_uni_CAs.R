#Barcoder: Subfunction 5 of "find_diagnostic_characters" ---------------------------------

#Filter unique CAs in array "unique.CAs.characters"
#Change e.g. A|A|C -> -|-|C (A is present in branch 1 and 2!)
#' Title Filter unique CAs in array "unique.CAs.characters"
#'
#' @param unique.CAs.characters Unique CA characters
#'
#' @return Return true unique CA characters
#' @export
#'
#' @examples filter_uni_CAs(unique.CAs.characters)
filter_uni_CAs <- function(unique.CAs.characters){

  true.unique.CAs.characters <- c()

  #check each position for diagnostic type
  for(pos_id in 1:length(unique.CAs.characters)){ #pos_id <-1

    #Get diagnostics
    uni_CA <- unique.CAs.characters[pos_id]

    #Split diagnostics per branch "|"
    uni_CA <- unlist(strsplit(uni_CA, "|", fixed=T))

    #Split diagnostics per branch "&"
    uni_diag1 <- unlist(strsplit(uni_CA, "&", fixed=T))

    #get unique diagnostics
    uni_diag2 <- unique(c(uni_diag1,recursive = TRUE))

    #Get diagnostics only present in one branch
    for(diag in uni_diag2){#diag<-"C"
      freq_nr <- length(which(uni_diag1==diag)) #Check if diagnostic is present in more than one branch
      if(freq_nr>1){

        for(diag_nr in 1:length(uni_CA)){#diag_nr<-2

          diag3           <- unlist(strsplit(uni_CA[diag_nr], "&", fixed=T))
          dupl_pos        <- which(diag3==diag)
          diag3[dupl_pos] <- "-"
          uni_CA[diag_nr] <- paste(diag3, collapse="&")
        }
      }
    }
    uni_CA <- paste(uni_CA, collapse="|")
    true.unique.CAs.characters[pos_id] <- uni_CA
  }

  #Clear out deleted CAs "-&" or "&-"
  true.unique.CAs.characters <- gsub(pattern = "(-&|&-)", replacement = "", true.unique.CAs.characters)

  return(true.unique.CAs.characters)
}

#-------------------------------------------------------------------------------
