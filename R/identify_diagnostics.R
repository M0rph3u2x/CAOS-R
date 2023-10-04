#Barcoder: Subfunction 6 of "find_diagnostic_characters" ---------------------------------

#Sort CAs into simple pure and simple private character types
#' Title Sort CAs into simple pure and simple private character types
#'
#' @param all.CAs.characters All character attribute characters
#' @param exclusive.unique.CAs.characters Exclusive unique character attribute characters
#'
#' @return Return character attribute types(simple pure & simple private)
#' @export
#'
#' @examples identify_diagnostics(all.CAs.characters,exclusive.unique.CAs.characters)
identify_diagnostics <- function(all.CAs.characters,exclusive.unique.CAs.characters){
  SimplePure_Characters <- c()
  SimplePriv_Characters <- c()

  #check each position for diagnostic type
  for(pos_id in 1:length(all.CAs.characters)){ #pos_id <-1

    #Get diagnostics
    all_CA <- all.CAs.characters[pos_id]
    uni_CA <- exclusive.unique.CAs.characters[pos_id]

    #Split diagnostics per branch "|"
    all_CA <- unlist(strsplit(all_CA, "|", fixed=T))
    uni_CA <- unlist(strsplit(uni_CA, "|", fixed=T))

    for(ca_id in 1:length(all_CA)){#ca_id<-2
      if(ca_id==1){
        if(all_CA[ca_id] == uni_CA[ca_id]){
          SimplePure_Characters[pos_id] <- all_CA[ca_id]
          SimplePriv_Characters[pos_id] <- "-"
        }else{
          SimplePriv_Characters[pos_id] <- uni_CA[ca_id]
          SimplePure_Characters[pos_id] <- "-"
        }
      }else{
        if(all_CA[ca_id] == uni_CA[ca_id]){
          SimplePure_Characters[pos_id] <- paste(SimplePure_Characters[pos_id],all_CA[ca_id], sep="|")
          SimplePriv_Characters[pos_id] <- paste(SimplePriv_Characters[pos_id],"-", sep="|")
        }else{
          SimplePriv_Characters[pos_id] <- paste(SimplePriv_Characters[pos_id],uni_CA[ca_id], sep="|")
          SimplePure_Characters[pos_id] <- paste(SimplePure_Characters[pos_id],"-", sep="|")
        }
      }
    }
  }
  CA_Type <- list(SimplePure_Characters,SimplePriv_Characters)
  return(CA_Type)
}

#-------------------------------------------------------------------------------
