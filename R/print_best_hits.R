#Classifier: Save and print best hits per query --------------------------------------------
#' Title Save and print best hits per query
#'
#' @param query_data Query data
#' @param leading_taxa Leading taxa
#' @param best_hits Best hit data
#' @param out.path  Path to output
#'
#' @return Best hits per query
#' @export
#'
#' @examples print_best_hits(query_data,leading_taxa,best_hits,out.path)
print_best_hits <- function(query_data,leading_taxa,best_hits,out.path){

  #Get query name
  query_name <- gsub("[>]","",query_data$taxa[query_nr])

  #Collapse best hit references
  leading_taxa <- paste(leading_taxa, collapse=",")

  #Save best hits in dataframe
  best_hits[nrow(best_hits)+1,] <- c(query_name, leading_taxa)

  #Save best hits
  table.path <- file.path(out.path,"best_hits.xlsx")
  write_xlsx(best_hits,table.path)
  table.path <- file.path(out.path,"best_hits.csv")
  write.table(best_hits, file=table.path, sep=",", row.names=FALSE, col.names=TRUE,  na="NA")

  return(best_hits)
}

#-------------------------------------------------------------------------------
