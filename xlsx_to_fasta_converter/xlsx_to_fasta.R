#!/usr/bin/env Rscript --vanilla

# The script was written by Tjard Bergmann (2023) and is not licensed.
# Everyone is free to use or edit it under the rules of creative commons (CC-BY).

#-------------------------------------------------------------------------------

#-----------------------------
# Description
#-----------------------------
#
#This R script converts a xlsx data file into a CAOS-R fasta file
#
#Example:
#
#Before (xlsx):
#Specimen ID\Marker	CO1	        ND1	        Color_Wing  Color_Abdomen	    Size
#ID8	              ACGTAACGTA	TAATTAAGCT	green	      blue,yellow,red	  5-6
#ID12	              ACGTTTTT--	TAATTAAGCT	green	      blue,yellow,red	  5-6
#
#After (fasta):
#>ID8
#ACGTAACGTA&TAATTAAGCT&green&blue$yellow$red&5-6
#>ID12
#ACGTTTTT--&TAATTAAGCT&green&blue$yellow$red&5-6
#
#-------------------------------------------------------------------------------

# 0) Parameter -----------------------------------------------------------------

#Defines at which symbol the data is separated
separator <- ","

#-------------------------------------------------------------------------------

# 1) Load packages into RStudio ------------------------------------------------

print("1) Load packages into RStudio")
print("")

library(rstudioapi)#Get path from RStudio (function: getSourceEditorContext()$path)
library(readxl)    #function(read_excel): Reads excel files

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

print("3) Setup directory where data input/output is stored")
print("")

#Create path to input data
input.path <- FullPath("create_fasta_input")

#Create main output folder
out.path <- FullPath("create_fasta_output")
dir.create(out.path, showWarnings = FALSE)

#-------------------------------------------------------------------------------

# 4) Load xlsx files -----------------------------------------------------------

print("4) Load xlsx files")

#List files
XLSX_Paths <- list.files(input.path, pattern = "\\.xlsx$", full.names = TRUE)
XLSX_Names <- list.files(input.path, pattern = "\\.xlsx$")

#-------------------------------------------------------------------------------

# 5) Load xlsx files -----------------------------------------------------------

print("5) Process xlsx files")
i<-1
for(XLSX_Path in XLSX_Paths){#XLSX_Path <- XLSX_Paths[1]
  print(paste("Process XLSX file: ",XLSX_Names[i], sep=""))
  
  # Read in FASTA file
  xlsx_data <- read_excel(XLSX_Path, col_names = TRUE)
  
  #Convert data to fasta format
  new_fasta <- c()
  ii        <- 1
  for(id in xlsx_data$`Specimen ID\\Marker`){#id<-xlsx_data$`Specimen ID\\Marker`[1]
    #Create header
    header                         <- paste(">", id, sep="")
    new_fasta[length(new_fasta)+1] <- header
    data                           <- paste(xlsx_data[ii,2:ncol(xlsx_data)], collapse="&")
    data                           <- gsub(separator,"$",data)
    new_fasta[length(new_fasta)+1] <- data
    ii                             <- ii+1
  }
  
  #Save edited fasta dataset
  xlsx.out.path <- file.path(out.path,XLSX_Names[i])
  xlsx.out.path <- gsub("xlsx", "fas", xlsx.out.path)
  writeLines(new_fasta, xlsx.out.path)
  i <- i+1
}