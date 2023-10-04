# 1) Barcoder: Index input data ----------------------------------------------------------

# The data in the input folder is screened for the correct input data files, it is
# then indexed to be procedural processed.

#' Title Index input data
#'
#' @param query_path Path to query data
#'
#' @return Return index of input data
#' @export
#'
#' @examples index_input_data(query_path)
index_input_data <- function(query_path){

  print("Index input data")

  #List files (fasta and tree data is necessary for caos analysis)
  FASTA.file <- list.files(query_path, pattern = "\\.fas$")
  TREE.file  <- list.files(query_path, pattern = "\\.tre$")

  #List path to files
  FASTA.Path <- file.path(query_path, FASTA.file)
  TREE.Path  <- file.path(query_path, TREE.file)

  #Identify complete datasets (matching csv/wav)
  test.fas      <- sub(pattern = "(.*)\\..*$", replacement = "\\1", FASTA.Path)
  test.tre      <- sub(pattern = "(.*)\\..*$", replacement = "\\1", TREE.Path)
  match.fas_tre <- test.fas %in% test.tre
  match.tre_fas <- test.tre %in% test.fas
  FASTA.Path    <- FASTA.Path[match.fas_tre]
  TREE.Path     <- TREE.Path[match.tre_fas]
  FASTA.file    <- FASTA.file[match.fas_tre]
  TREE.file     <- TREE.file[match.tre_fas]

  #List marker and marker patterns
  Markers          <- list.files(query_path, pattern = "\\Markers.txt$")
  Marker_Patterns  <- list.files(query_path, pattern = "\\Marker_Patterns.txt$")

  #Get names of all datasets in input folder
  Dataset_Names <- sub(pattern = "(.*)\\..*$", replacement = "\\1", FASTA.file)

  index <- list(FASTA.file,TREE.file,FASTA.Path,TREE.Path,Markers,Marker_Patterns,Dataset_Names)
  return(index)

}

#-------------------------------------------------------------------------------
