#Classifier: Create query list (1:Query_name,2-n:Markerdata (separated)) -------
#' Title Create query list (1:Query_name,2-n:Markerdata (separated))
#'
#' @param query Query data
#'
#' @return Query list
#' @export
#'
#' @examples create_query_list(query)
create_query_list <- function(query){
  query_list <- list()
  for(col in 1:ncol(query)){#col<-4
    if(col==1){
      query_list[[col]] <- query[,col]
    }else{
      q.data <- query[,col]
      q.data <- split_characters(q.data)
      q.data <- q.data[[1]]
      query_list[[col]] <- q.data
    }
  }
  return(query_list)
}

#-------------------------------------------------------------------------------
