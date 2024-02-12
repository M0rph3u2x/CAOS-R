# 9) Barcoder: Find diagnostic characters ------------------------------------------------

# Identify diagnostic characters
#' Title Identify diagnostic characters
#'
#' @param tree.nr Tree number
#' @param nodes.tree Tree nodes
#' @param all.branch.names All branch names
#' @param caos.path Path to CAOS file
#' @param CAOS CAOS sorted list of all processed input data
#' @param empty_row Placeholder positions to separate branch data
#' @param overview.path Overview path
#'
#' @return Return tables with diagnostic characters
#' @export
#'
#' @examples find_diagnostic_characters(tree.nr,nodes.tree,all.branch.names,caos.path,CAOS,empty_row,overview.path)
find_diagnostic_characters <- function(tree.nr,nodes.tree,all.branch.names,caos.path,CAOS,empty_row,overview.path){

  #Parameters:
  All_CA_Overview <- data.frame(Diagnostic_Positions      = integer(),
                                Branch_Positions          = integer(),
                                All_Characters            = integer(),
                                Unique_Characters         = integer(),
                                Diagnostic_Characters     = integer(),
                                SimplePure_Characters     = integer(),
                                SimplePrivate_Characters  = integer())

  All_CA_Taxa <- data.frame(Node_Position    = integer(),
                            Branch_Positions = integer(),
                            Branch_Taxa      = integer())


  for(node.pos in 1:tree.nr){#node.pos<-2

    node <- all.branch.names[[node.pos]]

    #Devide loaded node data into branches -------------------------------------
    branches <- separate_branch_taxa(node) #Subfunction 1

    #---------------------------------------------------------------------------

    #Get all character Attributes (CAs) for each branch, taxa and position -----

    #Save CAs for each branch, taxa and position in list
    branch_CA <- get_CA_per_taxa(branches,CAOS) #Subfunction 2

    #---------------------------------------------------------------------------

    # Find unique diagnostics for loaded node ----------------------------------

    # Create a list for each branch listing all unique characters found at each
    # position within the dataset
    info          <- find_diagnostics_per_branch(branch_CA,branches) #Subfunction 3
    branch_sep.CA <- info[[1]]
    CA.chars.nr   <- info[[2]]

    #Remove list from memory
    rm(info)

    #---------------------------------------------------------------------------

    # Find unique diagnostics between branches of the selected node ------------

    #Diagnostics sorted by data position and branch data separated by "|"
    diagnostic_data       <- identify_unique_diagnostics(branch_sep.CA,CA.chars.nr)  #Subfunction 4
    unique.CAs.branchpos  <- diagnostic_data[[1]] #Get branches overview
    unique.CAs.Position   <- diagnostic_data[[2]] #Get branches ID's
    all.CAs.characters    <- diagnostic_data[[3]] #Get all CAs per branch
    unique.CAs.characters <- diagnostic_data[[4]] #Get only diagnostic CAs per branch

    #Remove list from memory
    rm(diagnostic_data)

    #---------------------------------------------------------------------------

    #Create diagnostic overview and data table if diagnostics are identified
    if(length(unique.CAs.Position)>0){

      #Filter unique CAs in array "unique.CAs.characters
      #Change e.g. A|A|C -> -|-|C (A is present in branch 1 and 2!)
      exclusive.unique.CAs.characters <- filter_uni_CAs(unique.CAs.characters) #Subfunction 5

      #Sort CAs into simple pure and simple private character types
      CA_Type              <- identify_diagnostics(all.CAs.characters,exclusive.unique.CAs.characters) #Subfunction 6
      SimplePure_Characters <- CA_Type[[1]]
      SimplePriv_Characters <- CA_Type[[2]]

      #Remove list from memory
      rm(CA_Type)

      CA_Overview    <- data.frame(Diagnostic_Positions      = unique.CAs.Position,
                                   Branch_Positions          = unique.CAs.branchpos,
                                   All_Characters            = all.CAs.characters,
                                   Unique_Characters         = unique.CAs.characters,
                                   Diagnostic_Characters     = exclusive.unique.CAs.characters,
                                   SimplePure_Characters     = SimplePure_Characters,
                                   SimplePrivate_Characters  = SimplePriv_Characters)

      #-------
      # Create more columns in CA_Overview with more specific separation of diagnostic quality (sPr vs hetero sPu vs homo sPu)
      #-------

      #Create xlsx summary table (create CA overview for each node)
      table.path <- file.path(overview.path,paste("CA_Overview_Node_",nodes.tree[node.pos],".xlsx",sep=""))
      write_xlsx(CA_Overview,table.path)

      #Cummulate data for overview (all data)
      All_CA_Overview <- rbind(All_CA_Overview, CA_Overview)

      #-------------------------------------------------------------------------

      #Get Species names in node and all characters-----------------------------

      info <- get_taxa_names_and_ca(node,CAOS) #Subfunction 7
      Species.names <- info[[1]] #Extract taxa names from list
      Species.sequs <- info[[2]] #Extract CAs from list

      #Remove list from memory
      rm(info)

      #-------------------------------------------------------------------------

      #Reduce characters to diagnostic characters ------------------------------

      Species.sequences <- c()

      #Get characters in diagnostic positions
      for(i in 1:length(Species.names)){
        Species.sequences[i] <- list(Species.sequs[[i]][unique.CAs.Position])
      }

      #Transform list into dataframe
      df <- data.frame(matrix(unlist(Species.sequences), nrow=length(Species.sequences), byrow=TRUE))

      #Combine names and characters in dataframe
      names(df)     <- unique.CAs.Position #Change names to diagnostic positions
      Species.names <- data.frame(Species_Names=Species.names)
      df            <- cbind(Species.names,df)

      #Create empty rows to divide branched data in node overview table
      df <-insertRows(df, empty_row[[node.pos]], new = NA)

      #Create xlsx summary table (contains cummulative data of all processed MNTBs)
      table.path       <- file.path(caos.path,paste("CA_Table_Node_",nodes.tree[node.pos],".xlsx",sep=""))
      write_xlsx(df,table.path)
      #---------------------------------------------------------------------------

      #Create Taxa overview data.frame (Important to correlate diagnostics to taxa)
      CA_Taxa <- data.frame(Node_Position    = nodes.tree[node.pos],
                            Branch_Positions = unique.CAs.branchpos[1],
                            Branch_Taxa      = paste(all.branch.names[[node.pos]], collapse = ','))

      #Cummulate data for taxa overview (all data)
      All_CA_Taxa <- rbind(All_CA_Taxa, CA_Taxa)
    }
  }

  #Create xlsx summary table (create cumulative CA overview of all taxa)
  table.path <- file.path(overview.path,"CA_Taxa_All.xlsx")
  write_xlsx(All_CA_Taxa,table.path)

  #Format data for all data overview
  All_CA_Overview <- format_All_CA_Overview(All_CA_Overview) #Subfunction 5

  #Create xlsx summary table (create cumulative CA overview of all data)
  table.path <- file.path(overview.path,"CA_Overview_All.xlsx")
  write_xlsx(All_CA_Overview,table.path)
}

#-------------------------------------------------------------------------------
