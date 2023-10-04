# 2b) Barcoder: CAOS - Get branch hierarchy ----------------------------------------------

#' Title Load branch hierarchy
#'
#' @param tree Tree file
#'
#' @return Return tree hierarchy
#' @export
#'
#' @examples get_branch_hierarchy(tree)
get_branch_hierarchy <- function(tree){

  #Get all nodes
  nodes.tree <- unique(tree$edge[,1])

  #Get id number of last node in tree
  max.node <- max(nodes.tree)

  #Get total number of nodes
  tree.nr <- tree$Nnode

  #Get number of tip-labels (number of total specimen used in tree)
  label.nr <- Ntip(tree)

  #Create lists for branch names and coordinates
  all.branch.names <- list()
  all.branch.coord <- list()

  #Get all branching coordinates for all nodes
  for(i in 1:tree.nr){#Loop for nodes #i<-1
    node.pos              <- which(nodes.tree[i]==tree$edge[,1])
    all.branch.coord[[i]] <- tree$edge[node.pos,2]
  }

  #Extract all specimen groups for each node
  for(i in 1:length(all.branch.coord)){
    branch.names <- c()
    #Get all names for each branch
    for(ii in 1:length(all.branch.coord[[i]])){
      branch <- all.branch.coord[[i]][ii]
      x <- 1
      labels <- c()
      #Repeat loop until all nodes have been resolved
      #and names have been extracted
      while(x==1){
        node.pos   <- c()
        node.pos   <- tree$edge[,1] %in% branch
        low.branch <- tree$edge[node.pos,2]
        if(length(low.branch)==0){
          branch.names <- c(branch.names,sort(tree$tip.label[branch]))
          branch       <- c()
          x            <- 0
          next
        }
        branch  <- c()
        #Get node coordinates from lower nodes
        #or save names if last node has been resolved
        for(iii in 1:length(low.branch)){
          if(low.branch[iii]>label.nr){
            branch <- c(branch,low.branch[iii])
          }else{
            labels <- c(labels,low.branch[iii])
          }
        }
        #Close loop if all nodes have been resolved
        if(length(branch)==0){
          x <- 0
        }
      }
      #Sort and save names, devide each main branch data by "|"
      labels <- sort(labels)
      if(ii==length(node.pos)){
        branch.names <- c(branch.names,sort(tree$tip.label[labels]))
      }else{
        branch.names <- c(branch.names,sort(tree$tip.label[labels]),"|")
      }
    }
    branch.names <- str_replace_all(branch.names, " ", "_") #Edit branch names to fit with fasta formatting
    branch.names <- str_replace_all(branch.names, "'", "")  #Edit branch names to fit with fasta formatting
    all.branch.names[[i]] <- branch.names
  }

  #Delete last "|" from data
  for(i in 1:length(all.branch.names)){
    all.branch.names[[i]] <- all.branch.names[[i]][-length(all.branch.names[[i]])]
  }

  #Get positions of "|" symbol for each node (important to include empty rows for
  #character attribute overview tables)
  empty_row <- list()
  for(node_nr in 1:length(all.branch.names)){
    empty_row[[node_nr]] <- which(all.branch.names[[node_nr]]=="|")
  }

  hierarchy <- list(tree.nr, nodes.tree, all.branch.names, empty_row)

  return(hierarchy)

}

#-------------------------------------------------------------------------------
