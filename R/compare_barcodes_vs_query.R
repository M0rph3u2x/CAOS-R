#Classifier: Compare barcodes with query data ----------------------------------

#' Title Compare barcodes with query data
#'
#' @param node_pos Node position
#' @param marker_data Marker data
#' @param ref_node Reference node
#' @param input.path Input path
#' @param out.path Output path
#' @param query_list Query list
#' @param marker_priority Marker priority
#'
#' @return Save hit score, filtered hit score, score overview and filtered score overview
#' @export
#'
#' @examples compare_barcodes_vs_query(node_pos,marker_data,ref_node,input.path,out.path,query_list,marker_priority)
compare_barcodes_vs_query <- function(node_pos,marker_data,ref_node,input.path,out.path,query_list,marker_priority){

  #Define dataframes and vectors -----------------------------------------------

  #Create dataframe for CA hit score
  hit_score <- data.frame(Marker         = integer(),
                          Node           = integer(),
                          Raw_Score      = integer(),
                          Priority_Score = integer())

  #Create detailed overview of hit score
  score_overview <- data.frame(Marker      = integer(),
                               Node        = integer(),
                               CA_Position = integer(),
                               sPu_CA      = integer(),
                               sPr_CA      = integer(),
                               Unique_CA   = integer(),
                               Query_CA    = integer(),
                               Score       = integer())

  #Create filters (sorts out nodes without matching diagnostics)
  filters <- c()

  #-----------------------------------------------------------------------------

  #Process each barcode relevant node in tree
  for(node in node_pos){#node<-35

    #Load reference barcode location for each marker
    barcode_location <- get_barcode_location(marker_data,ref_node,node) #Get path to barcode

    marker_out <- marker_data[!is.na(barcode_location)]     #Get names of markers with barcodes

    barcode_location <- barcode_location[!is.na(barcode_location)] #Delete marker path if no barcodes observed "NA"

    #List diagnostics for each marker
    marker_diagnostics <- get_marker_diagnostics(input.path,barcode_location)

    #Compare query with each markers reference barcodes
    for(i in 1:length(barcode_location)){#i<-1
      ref_codes             <- marker_diagnostics[[i]]           #Get reference barcodes
      query_pos             <- which(marker_out[i]==marker_data) #Get correct marker location of query
      query_codes           <- query_list[[query_pos+1]]         #Get query diagnostics
      branch_count          <- str_count(ref_codes$Branch_Positions[1], pattern = "\\|")+1
      match                 <- c()
      match[1:branch_count] <- 0

      #Process CAs for each barcode position
      for(row in 1:nrow(ref_codes)){#row<-1
        diag_pos <- ref_codes$Diagnostic_Positions[row] # Get diagnostic position
        CA_query <- query_codes[diag_pos]               # Character Attribute query (at this position)
        CA_query <- toupper(CA_query)                   # Convert all diagnostic data to upper case format

        #Get diagnostics from barcodes
        sPu_ref <- get_pos_diagnostics(ref_codes,row,"SimplePure_Characters")
        sPr_ref <- get_pos_diagnostics(ref_codes,row,"SimplePrivate_Characters")
        uni_ref <- get_pos_diagnostics(ref_codes,row,"Unique_Characters")

        #match-control to test if any diagnostics matched
        #between query and ref barcodes
        match_control                 <- c()
        match_control[1:branch_count] <- 0
        filters <- c(filters,paste(match_control,collapse="|")) #Save empty control (needed later)

        #Create branch specific score based on matches between query and reference
        m_return <- find_match_query_vs_ref(CA_query,sPu_ref,match,match_control,2)   #Find query match with simple pure diagnostic
        match         <- m_return[[1]]
        match_control <- m_return[[2]]
        m_return <- find_match_query_vs_ref(CA_query,sPr_ref,match,match_control,1)   #Find query match with simple private diagnostic
        match         <- m_return[[1]]
        match_control <- m_return[[2]]
        match    <- find_match_query_vs_ref2(CA_query,uni_ref,match,match_control,0.5) #Find query match with unique diagnostic
        rm(match_control) #Release memory

        score_overview[nrow(score_overview) + 1, ] <- c(marker_out[i],
                                                        node,
                                                        diag_pos,
                                                        ref_codes[["SimplePure_Characters"]][row],
                                                        ref_codes[["SimplePrivate_Characters"]][row],
                                                        ref_codes[["Unique_Characters"]][row],
                                                        CA_query,
                                                        paste(match,collapse="|"))


      }#End of barcode position loop

      #Calculate score based on priority of marker
      prior_match <- round(match/marker_priority[i], digits = 0)

      #Save score in dataframe "hit_score"
      hit_score[nrow(hit_score) + 1, ] <- c(marker_out[i],node,paste(match, collapse = '|'),paste(prior_match, collapse = '|'))

    }#End of marker loop
  }#End of tree node loop


  #Filter score for hits
  filt_hit_score      <- filter_score(filters, hit_score)
  filt_score_overview <- filter_overview(filters, score_overview)

  #Create folder for query classification output
  out.name  <- str_remove(query_data$taxa[query_nr], ">")
  query_out <- paste(out.path,out.name,sep = "/")
  dir.create(query_out, showWarnings = FALSE)

  #Save hit score and filtered hit score
  table.path <- file.path(query_out,"hit_score.xlsx")
  write_xlsx(hit_score,table.path)
  table.path <- file.path(query_out,"hit_score.csv")
  write.table(hit_score, file=table.path, sep=",", row.names=FALSE, col.names=TRUE,  na="NA")
  table.path <- file.path(query_out,"filtered_hit_score.xlsx")
  write_xlsx(filt_hit_score,table.path)
  table.path <- file.path(query_out,"filtered_hit_score.csv")
  write.table(filt_hit_score, file=table.path, sep=",", row.names=FALSE, col.names=TRUE,  na="NA")


  #Save score overview and filtered score overview
  table.path <- file.path(query_out,"score_overview.xlsx")
  write_xlsx(score_overview,table.path)
  table.path <- file.path(query_out,"score_overview.csv")
  write.table(score_overview, file=table.path, sep=",", row.names=FALSE, col.names=TRUE,  na="NA")
  table.path <- file.path(query_out,"filtered_score_overview.xlsx")
  write_xlsx(filt_score_overview,table.path)
  table.path <- file.path(query_out,"filtered_score_overview.csv")
  write.table(filt_score_overview, file=table.path, sep=",", row.names=FALSE, col.names=TRUE,  na="NA")

  return(filt_hit_score)
}

#-------------------------------------------------------------------------------
