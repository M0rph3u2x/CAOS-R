#!/usr/bin/env Rscript --vanilla

# The script was written by Tjard Bergmann (2023) and is not licensed.
# Everyone is free to use or edit it under the rules of creative commons (CC-BY).

#-------------------------------------------------------------------------------

#-----------------------------
# Description
#-----------------------------
#
#CAOS (Character Attribute Organisation System) is a software to identify
#diagnostic patterns in character attributes of any kind (DNA, Amino Acids, expression pattern, etc.)
#CAOS can work with stand-alone as well as concatinated data as long as it is
#properly formatted (read the guide for details).


#' Title CAOS-Barcoder
#'
#' @return Creates caos-barcodes
#' @export
#'
#' @examples caos_barcoder()
caos_barcoder <- function(){

  # 0) Identify operating system -------------------------------------------------

  print("0) Identify operating system")
  print("")

  #Get system information
  sysinf <- Sys.info()

  #-------------------------------------------------------------------------------

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

  #---------------------------

  # Install igraph
  packages <- c("igraph")

  # Install package manager if not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages("igraph", type="binary")
  }

  #---------------------------

  #Install and/or load all necessary packages via package manager "pacman"
  pacman::p_load(ape,           #Tree processing modules
                 base,          #Find identic diagnostics: function(toupper, grep)
                 berryFunctions,#function(insertRows)
                 dplyr,         #function(group_by, summarize) needed for "All_CA_Overview"
                 geiger,        #Tree processing modules
                 igraph,        #Graphic tool
                 phangorn,      #Tree processing modules
                 phytools,      #Tree processing modules
                 rstudioapi,    #Get path from RStudio (function: getSourceEditorContext()$path)
                 stringr,       #function(str_remove, str_replace)
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
  input.path <- FullPath("input_barcoder")

  #Create main output folder
  out.path <- FullPath("output_barcoder")
  dir.create(out.path, showWarnings = FALSE)

  #-------------------------------------------------------------------------------

  # 4) Index input data ----------------------------------------------------------

  print("4) Index input data")
  print("")

  # The data in the input folder is screened for the correct input data files, it is
  # then indexed to be procedural processed.

  index           <- index_input_data(input.path)
  FASTA.file      <- index[[1]]
  TREE.file       <- index[[2]]
  FASTA.Path      <- index[[3]]
  TREE.Path       <- index[[4]]
  Markers         <- index[[5]]
  Marker_Patterns <- index[[6]]
  Dataset_Names   <- index[[7]]

  #Remove list from memory
  rm(index)

  #-------------------------------------------------------------------------------

  # 5) Process data --------------------------------------------------------------

  print("5) Process data")
  print("")

  for(i in 1:length(FASTA.Path)){#i<-1

    print("")
    print("")
    print(paste("Current dataset: ",Dataset_Names[i], sep=""))

    print("5_Create output subfolders")
    dataset.path  <- pathmaker(out.path,Dataset_Names[i])


    # 5a) Resolve tree data into list of nodes and corresponding specimen --------

    print("5a_Resolve tree data into list of nodes and corresponding specimen")

    # Read tree
    if(sysinf[1] == "Windows"){
      tree <- read_tree_win(TREE.Path[i],Dataset_Names[i])
    }else{
      tree <- read_tree_osx(TREE.Path[i],Dataset_Names[i])
    }


    #-----------------------------------------------------------------------------

    # 5b) CAOS - Get branch hierarchy --------------------------------------------

    print("5b) CAOS - Get branch hierarchy")

    hierarchy        <- get_branch_hierarchy(tree)
    tree.nr          <- hierarchy[[1]]
    nodes.tree       <- hierarchy[[2]]
    all.branch.names <- hierarchy[[3]]
    empty_row        <- hierarchy[[4]]

    #Remove list from memory
    rm(hierarchy)

    #-----------------------------------------------------------------------------

    # 5c) Check marker names and character attributes ----------------------------

    print("5c) Check marker names and character attributes")

    #Check number of markers and split data if multiple markers are used
    marker.path <- file.path(input.path, Markers[i])
    markers     <- read.table(marker.path)

    #Identify number of "&" in markers
    marker_names  <- str_split(markers[[1]], pattern = "&")
    marker_names  <- marker_names[[1]] #Delist items

    #-----------------------------------------------------------------------------

    # 5d) Get marker patterns ----------------------------------------------------

    print("5d) Get marker patterns")
    print("")

    marker_patterns.path <- file.path(input.path, Marker_Patterns[i])
    marker_patterns      <- read.table(marker_patterns.path)

    #-----------------------------------------------------------------------------

    #Parameter
    mark_nr <-1  #mark_nr <-2
    for(marker in marker_names){#marker<-"Color" #marker<-"CO1"

      print(paste("Current marker: ",marker, sep=""))

      # 6) Create output path for loaded dataset ---------------------------------

      print("6_Create output subfolders")
      caos.path     <- pathmaker(out.path,Dataset_Names[i], paste("/",marker,"/caos",sep=""))
      overview.path <- pathmaker(out.path,Dataset_Names[i], paste("/",marker,"/overview",sep=""))

      # 7) Get the data from FASTA & TREE ----------------------------------------

      print("7_Load data")

      # Read in FASTA file
      fasta.data <- readLines(FASTA.Path[i])

      # Read in TREE file
      tree.data <- readLines(TREE.Path[i])

      #---------------------------------------------------------------------------

      # 8) Process FASTA data ----------------------------------------------------

      print("8_Process FASTA data")

      #8a) Split FASTA data into names and sequence data
      fasta       <- process_fasta_data(fasta.data)
      fasta.names <- fasta[[1]]
      fasta.seq   <- fasta[[2]]

      #Remove list from memory
      rm(fasta)

      # 8b) Split FASTA data into marker related data
      marker        <- get_marker_data(fasta.names,fasta.seq,mark_nr)
      marker_names  <- marker[[1]]
      marker_seqs   <- marker[[2]]

      #Remove list from memory
      rm(marker,fasta.names,fasta.seq)

      #---------------------------------------------------------------------------

      # 9) Split data into single diagnostic characters or character patterns (CAs)

      print("9_Split data")

      # 9a) Separate single/multi-character patterns
      marker.CAs <- split_characters(marker_seqs)

      # 9b) Define allowed patterns of characters
      marker_pattern <- get_marker_patterns(marker_patterns,mark_nr)

      # 9c) Control characters for allowed patterns and mask undefined patterns
      marker.CAs <- control_patterns(marker.CAs, marker_pattern)

      rm(marker_pattern)

      #---------------------------------------------------------------------------

      # 10) Reattach names and single nucleotide characters into list ------------

      print("10_Reattach names and single nucleotide characters into list")

      #Combine names and characters into list
      CAOS <- list(marker_names,marker.CAs)

      #Remove list from memory
      rm(marker_names,marker_seqs,marker.CAs)

      #---------------------------------------------------------------------------

      # 11) Find diagnostic characters -------------------------------------------

      print("11_Find diagnostic characters")
      print("")

      # Identify diagnostic characters
      find_diagnostic_characters(tree.nr,
                                 nodes.tree,
                                 all.branch.names,
                                 caos.path,
                                 CAOS,
                                 empty_row,
                                 overview.path)

      #Increase marker number to switch to next marker
      mark_nr <-mark_nr+1
    }
  }

  print("Finished processing the data!")
}


