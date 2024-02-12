#!/usr/bin/env Rscript --vanilla

# The script was written by Tjard Bergmann (2023) and is not licensed.
# Everyone is free to use or edit it under the rules of creative commons (CC-BY).

#-------------------------------------------------------------------------------

#-----------------------------
# Description
#-----------------------------
#
#CAOS-Classifier is a software that identifies query data to its closest match
#in a reference barcode database

#' Title CAOS-Classifier
#'
#' @return Classifies taxa based on diagnostic characters
#' @export
#'
#' @examples caos_classifier()
caos_classifier <- function(){

  # 1) Install/Load packages into RStudio ----------------------------------------

  print("1) Install/Load packages into RStudio")
  print("")

  # Install package manager (pacman)
  packages <- c("pacman")

  # Install package manager if not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  #Install and/or load all necessary packages via package manager "pacman"
  pacman::p_load(base,          #Find identic diagnostics: function(toupper, grep)
                 dplyr,         #function(group_by, summarize) needed for "All_CA_Overview"
                 readxl,        #function(read_excel)
                 rstudioapi,    #Get path from RStudio (function: getSourceEditorContext()$path)
                 stringr,       #function(str_remove, str_replace)
                 tidyr,         #function(separate)
                 writexl)       #function(write_xlsx)

  #-------------------------------------------------------------------------------

  # 2) Create path to R-Script ---------------------------------------------------

  print("2) Create path to R-Script")
  print("")

  #Create path to data
  R.Directory = sub(pattern = "(.*/).*\\..*$", replacement = "\\1", getSourceEditorContext()$path)

  #Create path to data
  FullPath = function(FileName){ return( paste(R.Directory,FileName,sep="") ) }

  # Setup working directory
  setwd(R.Directory)
  work_path <- getwd()

  #-------------------------------------------------------------------------------

  # 3) Setup directory where data input/output is stored -------------------------

  print("3) Setup input/output directory")
  print("")

  #Create path to input data
  input.path <- FullPath("input_classifier")

  #Create main output folder
  out.path <- FullPath("output_classifier")
  dir.create(out.path, showWarnings = FALSE)

  #-------------------------------------------------------------------------------

  # 4) Index input data (Query and reference barcode data) -----------------------

  print("4) Index input data")
  print("")

  # The data in the input folder is screened for the correct input data files, it is
  # then indexed to be procedural processed.
  index           <- index_input_data2(input.path)
  marker_data     <- index[[1]]  # Name of markers
  marker_priority <- index[[2]]  # Priority of each marker (1>2>3...)
  query_data      <- index[[3]]  # Query data as dataframe (taxa name & marker data)
  ref_taxa        <- index[[4]]  # Names of reference taxa per node
  ref_node        <- index[[5]]  # Location of node barcodes for each marker
  node_pos        <- index[[6]]  # Node ids for nodes with barcodes
  ref_data        <- index[[7]]  # Reference data as dataframe (taxa name & marker data)

  #Remove list from memory
  rm(index)

  #-------------------------------------------------------------------------------

  # 5) Compare each query with reference barcode ---------------------------------

  print("5) Compare query with reference barcode")
  print("")

  #Dataframe with best hits
  best_hits <- data.frame(Query = integer(), Hits = integer())

  #Loop through all query files in input folder
  for(query_nr in 1:nrow(query_data)){#query_nr<-1

    print(paste("Loaded query: ",query_data$taxa[query_nr], sep=""))

    # 5A) Score the loaded query with reference barcodes -------------------------

    #Load query data
    query <- query_data[query_nr,]

    #Create query list (1:Query_name,2-n:Markerdata (separated by character attributes))
    query_list <- create_query_list(query)

    #Score all query files with reference barcodes
    hit_score <- compare_barcodes_vs_query(query_nr,node_pos,marker_data,ref_node,input.path,out.path,query_data,query_list,marker_priority)

    #-----------------------------------------------------------------------------

    # 5B) Identify query based on score and taxa data ----------------------------

    #Reorganize ref_taxa based on marker hierarchy
    ref_taxa <- reorganize_data(ref_taxa,marker_data)

    #Get reference taxa for each barcoded node
    ref_taxa_data <- get_marker_diagnostics(input.path,ref_taxa)

    #Delete node data with no score
    ref_taxa_data <- delete_ref_no_score(hit_score,ref_taxa_data)

    #Get unique taxa data summary
    unique_ref_taxa_data <- get_unique_ref_taxa(ref_taxa_data)

    #Identify and save best hit reference taxa from nodes with hits
    leading_taxa <- find_best_hits(hit_score,ref_taxa_data,unique_ref_taxa_data)

    #Save and print best hits per query
    best_hits <- print_best_hits(query_nr,query_data,leading_taxa,best_hits,out.path)

    #-----------------------------------------------------------------------------

    #5c) Align query and best hit references
    align_query_and_bh_refs(query_nr,best_hits,ref_data,marker_data,query_list,out.path)

  }

  print("Finished processing the data!")
}
