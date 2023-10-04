# 1) Classifier: Index input data ----------------------------------------------

# The data in the input folder is screened for the correct input data files, it is
# then indexed to be procedural processed.

#' Title Index input data
#'
#' @param input.path Path to input data
#'
#' @return Return index of input data
#' @export
#'
#' @examples index_input_data2(input.path)
index_input_data2 <- function(input.path){

  print("Index input data")

  #-----------------------------------------------------------------------------

  print("Get reference markers")

  #Create correct path to reference folder:
  ref_path <- paste(input.path, "reference", sep = "/")

  #get markers
  ref_marker <- list.files(ref_path, pattern = "(Marker_Priority)\\.txt$", recursive = TRUE)

  #Load reference markers
  marker_path <- paste(ref_path, ref_marker, sep = "/")     #create path to markers
  marker.data <- readLines(marker_path)                     #get markers
  marker.data <- strsplit(marker.data[1], split = "[:&=>]") #Split markers
  marker_data <- marker.data[[1]]                           #Turn list into vector

  #Get priority level for each marker
  marker_priority <- get_priority_of_markers(ref_path, ref_marker)

  #-----------------------------------------------------------------------------

  print("Get query data")

  #Create correct path to query wave files:
  query_path <- paste(input.path, "query", sep = "/")

  #List files (fasta and tree data is necessary for caos analysis)
  query_name <- list.files(query_path, pattern = "\\.fas$")

  #Load query data
  query_path <- paste(query_path, query_name, sep = "/")
  query.data <- readLines(query_path)

  query_nr <- (floor(length(query.data)/2)*2)  #Get number of valid entries
  row_odd  <- seq_len(length(query.data)) %% 2 #Get odd rows information

  query.data <- query.data[1:query_nr] #Delete invalid entries
  q_taxa <- query.data[row_odd == 1]   #Separate taxa from query
  q_data <- query.data[row_odd == 0]   #Separate data from query

  query_data <- data.frame(taxa=q_taxa,data=q_data) #Create dataframe with taxa and data column

  # Split data column into different markers
  query_data <- query_data %>% separate(data, marker_data, sep = "&")

  #-----------------------------------------------------------------------------

  print("Get reference data")

  #List files (fasta and tree data is necessary for caos analysis)
  ref_name <- list.files(ref_path, pattern = "\\.xlsx$", recursive = TRUE)

  #library(stringr) [needed for function: str_locate]
  pos      <- str_locate(ref_name, "overview") #locate path with "overview" string
  pos      <- which(pos[,1] != "NA")       #get position of ref_data in dataframe
  ref_name <- ref_name[pos]

  #Separate node and taxa information
  pos      <- str_locate(ref_name, "Taxa") #locate path with "Taxa" string
  pos      <- which(pos[,1] != "NA")       #get position of ref_data in dataframe
  ref_taxa <- ref_name[pos]

  #Separate node and taxa information
  pos      <- str_locate(ref_name, "Node") #locate path with "Node" string
  pos      <- which(pos[,1] != "NA")       #get position of ref_data in dataframe
  ref.node <- ref_name[pos]

  #Get all unique node ids and sort them by number
  node_pos <- str_extract(ref.node, "[0-9]+\\.xlsx") #Extract node number string
  node_pos <- gsub("\\.xlsx", "",node_pos)           #Extract only number string
  node_pos <- unique(sort(as.numeric(node_pos)))     #Convert string to number format

  #Create list with marker separated node barcodes
  ref_node <- list()
  for(mark in marker_data){#mark<-marker_data[1]
    string              <- paste("/",mark,"/",sep="")
    pos                 <- str_locate(ref.node, string) #locate path with "Node" string
    pos                 <- which(pos[,1] != "NA")       #get position of ref_data in dataframe
    ref_node[[length(ref_node)+1]] <- ref.node[pos]     #get barcoded nodes into the list
  }

  #List files (fasta and tree data is necessary for caos analysis)
  ref_fasta <- list.files(ref_path, pattern = "\\.(fas|FAS|fasta|FASTA)$", recursive = TRUE)

  #Load reference data
  ref_path <- paste(ref_path, ref_fasta, sep = "/")
  ref.data <- readLines(ref_path)

  ref_nr <- (floor(length(ref.data)/2)*2)  #Get number of valid entries
  row_odd  <- seq_len(length(ref.data)) %% 2 #Get odd rows information

  ref.data <- ref.data[1:ref_nr]     #Delete invalid entries
  r_taxa   <- ref.data[row_odd == 1] #Separate taxa from ref
  r_data   <- ref.data[row_odd == 0] #Separate data from ref

  ref_data <- data.frame(taxa=r_taxa,data=r_data) #Create dataframe with taxa and data

  # Split data column into different markers
  ref_data <- ref_data %>% separate(data, marker_data, sep = "&")

  #-----------------------------------------------------------------------------

  #Load input data into index list
  index <- list(marker_data,marker_priority,query_data,ref_taxa,ref_node,node_pos,ref_data)

  return(index)
}

#-------------------------------------------------------------------------------
