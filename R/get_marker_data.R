# 6b) Barcoder: Split FASTA data into marker related data --------------------------------

#' Title Split FASTA data into marker related data
#'
#' @param fasta.names Fasta names
#' @param fasta.seq Fasta sequences
#' @param mark_nr marker number
#'
#' @return Return marker sorted database of sequence names and data
#' @export
#'
#' @examples get_marker_data(fasta.names,fasta.seq,mark_nr)
get_marker_data <- function(fasta.names,fasta.seq,mark_nr){

  #Get currents marker data names [FASTA]
  fasta.names_number <- str_count(fasta.names[1], pattern = "&")+1
  if(fasta.names_number > 1){
    f_names <- c()
    for(name_nr in 1:length(fasta.names)){
      marker_names     <- strsplit(fasta.names[name_nr], split = "&")
      f_names[name_nr] <- marker_names[[1]][mark_nr]
    }
    marker_names <- f_names

    #Remove list from memory
    rm(f_names)
  }else{
    marker_names <- fasta.names
  }

  #Get currents marker data sequences [FASTA]
  fasta.seq_number <- str_count(fasta.seq[1], pattern = "&")+1
  if(fasta.seq_number > 1){
    f_seq <- c()
    for(seq_nr in 1:length(fasta.seq)){#seq_nr<-3
      marker_seqs   <- strsplit(fasta.seq[seq_nr], split = "&")
      f_seq[seq_nr] <- marker_seqs[[1]][mark_nr]
    }
    marker_seqs   <- f_seq

    #Remove list from memory
    rm(f_seq)

  }else{
    marker_seqs <- fasta.seq
  }

  marker <- list(marker_names,marker_seqs)

  return(marker)

}

#-------------------------------------------------------------------------------
