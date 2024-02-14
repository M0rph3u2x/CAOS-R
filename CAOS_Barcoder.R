#!/usr/bin/env Rscript --vanilla

# The script was written by Tjard Bergmann (2024) and is not licensed.
# Everyone is free to use or edit it under the rules of creative commons (CC-BY).

#-------------------------------------------------------------------------------

#-----------------------------
# Description
#-----------------------------
#
#This R script loads the CAOS-R package and executes the CAOS-Barcoder program
#[function: caos_barcoder()]. The barcoder will detect diagnostic attributes in
#the user provided input data.This R script can automatically locate it's current 
#location. It is mandatory that the input data is stored in a folder labelled
#"input_barcoder". The "input_barcoder" folder with the user data must be located
#in the same directory as this script (CAOS-Barcoder).Examples for the correct
#format of input data can be found in github (https://github.com/M0rph3u2x/CAOS-R)
#within the "Examples" folder.
#
#
#-----------------------------
# Software dependencies
#-----------------------------

#CAOS-R is dependent on the following packages:

# ape,           #Tree processing modules
# base,          #Find identic diagnostics: function(toupper, grep)
# berryFunctions,#function(insertRows)
# dplyr,         #function(group_by, summarize) needed for "All_CA_Overview"
# geiger,        #Tree processing modules
# igraph,        #Graphic tool
# pacman,        #Package manager function(pacman)
# phangorn,      #Tree processing modules
# phytools,      #Tree processing modules
# readxl,        #function(read_excel)
# rstudioapi,    #Get path from RStudio (function: getSourceEditorContext()$path)
# stringr,       #function(str_remove, str_replace)
# tidyr,         #function(separate)
# writexl        #function(write_xlsx)

#All listed packages will be installed/loaded automatically, when the
#caos_barcoder() or caos_classifier() functions are executed

#-----------------------------
# Guide
#-----------------------------

# To successfully setup CAOS-R you need to:

# 1A) Download the latest CAOS-R package file (e.g. caosR_1.0.0.0015.tar.gz).
#     The package file is stored in the folder "CAOS-R_Package" on github
#     (https://github.com/M0rph3u2x/CAOS-R). Make sure to place the package file
#     in a folder labelled "CAOS-R_Package". This folder must be in the same directory
#     where this "Setup_CAOS-R.R" script is stored.

# 1B) Alternatively if this script cannot locate the "CAOS-R_Package" it will
#     directly download the package from github using the "devtools" package
#     (an online connection is necessary for this option).

# 2) Press Ctr+A, then Ctrl+Enter to execute the setup of CAOS-R

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
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

#Install and/or load all necessary packages via package manager "pacman"
pacman::p_load(rstudioapi)    #Get path from RStudio (function: getSourceEditorContext()$path)

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

#3) Load CAOS-R package --------------------------------------------------------

print("3) Install/Load CAOS-R package")
print("")


#Install and/or load all necessary packages via package manager "pacman"
pacman::p_load(caosR) #All functions written for CAOS Barcoder and Classifier

#-------------------------------------------------------------------------------

#4) Run CAOS-Barcoder ----------------------------------------------------------

caos_barcoder()

#-------------------------------------------------------------------------------