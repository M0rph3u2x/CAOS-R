#Barcoder: Subfunction 8 of "find_diagnostic_characters" ---------------------------------

#Format data for all data overview
#' Title Format data for all data overview
#'
#' @param All_CA_Overview All character attribute overview data
#'
#' @return Return modified all character attribute overview data
#' @export
#'
#' @examples format_All_CA_Overview(All_CA_Overview)
format_All_CA_Overview <- function(All_CA_Overview){

  mod_All_CA_Overview <- All_CA_Overview[0,]

  #Sort data by diagnostic positions
  All_CA_Overview <- All_CA_Overview[order(All_CA_Overview$Diagnostic_Positions),]

  #Delete second column "Branch_Positions"
  All_CA_Overview <- All_CA_Overview[,-2]

  #Remove element from strings in dataframe
  for(x in 2:length(All_CA_Overview)){
    for(y in 1:length(All_CA_Overview$Diagnostic_Positions)){
      All_CA_Overview[y,x] <- gsub("[|-]","&",All_CA_Overview[y,x])
    }
  }

  #Combine data of same diagnostic position ------------------------------------

  #get unique CA positions
  unique_IDs <- unique(All_CA_Overview$Diagnostic_Positions)

  for(ID in unique_IDs){ #ID <- 112
    CA_subset <- All_CA_Overview[All_CA_Overview$Diagnostic_Positions==ID,]
    Diagnostic_Positions     <- unique(CA_subset$Diagnostic_Positions)
    All_Characters           <- filter_diagnostics(CA_subset$All_Characters)
    Unique_Characters        <- filter_diagnostics(CA_subset$Unique_Characters)
    Diagnostic_Characters    <- filter_diagnostics(CA_subset$Diagnostic_Characters)
    SimplePure_Characters    <- filter_diagnostics(CA_subset$SimplePure_Characters)
    SimplePrivate_Characters <- filter_diagnostics(CA_subset$SimplePrivate_Characters)

    mod_data <- data.frame(Diagnostic_Positions     <- Diagnostic_Positions,
                           All_Characters           <- All_Characters,
                           Unique_Characters        <- Unique_Characters,
                           Diagnostic_Characters    <- Diagnostic_Characters,
                           SimplePure_Characters    <- SimplePure_Characters,
                           SimplePrivate_Characters <- SimplePrivate_Characters)

    mod_All_CA_Overview <- rbind(mod_All_CA_Overview,mod_data)

  }

  names(mod_All_CA_Overview) <- c("Diagnostic_Positions",
                                  "All_Characters",
                                  "Unique_Characters",
                                  "Diagnostic_Characters",
                                  "SimplePure_Characters",
                                  "SimplePrivate_Characters")

  return(mod_All_CA_Overview)
}

#-------------------------------------------------------------------------------
