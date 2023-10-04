# 6a) Barcoder: Process FASTA data -------------------------------------------------------

#Split fasta file into names and sequence data
#Store data in variables fasta.names and fasta.seq

#' Title Process FASTA data
#'
#' @param fasta.data Fasta data
#'
#' @return Return loaded fasta data
#' @export
#'
#' @examples process_fasta_data(fasta.data)
process_fasta_data <- function(fasta.data){
  fasta.names <- c()
  fasta.seq   <- c()
  i.names     <- 1
  i.seq       <- 0
  for(ii in 1:length(fasta.data)){#ii<-5
    if(nchar(fasta.data[ii])==0){#Skip row if empty
      next
    }else if(grepl('>', fasta.data[ii], fixed= TRUE)){
      #Extract name, then save name
      name <- str_remove(fasta.data[ii], ">")
      name <- str_replace_all(name, " ", "_")
      fasta.names[i.names] <- name
      i.names <- i.names+1
      i.seq   <- i.seq+1
    }else{
      #Convert nucleotides to uppercase characters, then save sequence
      if(length(fasta.seq)<i.seq){#Test if new sequence data is added
        fasta.seq[i.seq] <- toupper(fasta.data[ii])
      }else{#paste data together if data is distributed in more than one row
        fasta.seq[i.seq] <- paste(fasta.seq[i.seq], toupper(fasta.data[ii]), sep="")
      }
    }
  }

  fasta <- list(fasta.names, fasta.seq)
  return(fasta)
}

#-------------------------------------------------------------------------------
