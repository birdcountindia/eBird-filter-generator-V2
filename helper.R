library(sp)
library(terra) 
library(reshape2)
library(data.table)
library(tidyverse)

#' Returns lists filtered by location 
#' 
#' @param lists eBird lists
#' @param state One of the states in the eBird records file
#' @param region One of the regions in the shape file
#' @return eBird list for the specific location

getLocationFilteredLists <- function (lists, state, filterRegion) {
  
  print(paste("GetLocationFilteredLists", state, filterRegion))
  
  if(state != 'None')  
  {
    lists <- lists[which(lists$STATE.CODE == 
                           unique(g_states$STATE.CODE[ g_states$STATE==state ])), ]
    print(paste("StateFiltered", nrow(lists)))  
  }
  
  if(filterRegion != 'None')
  {
    # Match the user's selection strictly against the 'FILTER' column in g_filters
    if(filterRegion %in% g_filters$FILTER) {
      
      matched_ids <- g_filters$POLYGON.ID[g_filters$FILTER == filterRegion]
      lists <- lists[lists$POLYGON.ID %in% matched_ids, ]
      
    } 
  }
  
  print(paste("Final Filtered", nrow(lists)))  
  return(lists)
}

#' Returns duration in minutes at nth percentile based on sorted order of list duration
#' 
#' @param q 0..100 as percentile
#' @param state One of the states in the eBird records file
#' @param region One of the regions in the shape file
#' @return Duration in minutes at the nth percentile

getMinutes <- function(q, state, filterRegion) {
  
  print(q)
  print(state)
  print(filterRegion)
  
  m_ebd_lists <- g_lists
  
  # Filter lists by state and filter shape
  m_ebd_lists <- getLocationFilteredLists (m_ebd_lists, state, filterRegion)
  
  # Convert back to data frame and filter only duration
  m_ebd_lists <- subset(m_ebd_lists, select = c("DURATION.MINUTES")) 
  
  # Remove Null duration  
  m_ebd_lists <- drop_na(m_ebd_lists) %>% arrange(DURATION.MINUTES)
  
  # Return requested quantile
  return (round(quantile(m_ebd_lists$DURATION.MINUTES, q/100), digits = 0))
  }

#Generates the filter based on region shape and eBird data using a set of configurations
#' 
#' @param state state/province code
#' @param filterRegion Attribute name in the shape file
#' @param filterPercentile Percentile of records of a species to be considered as filter limit
#' @param duration Maximum list duration to be considered or filter calculations
#' @param fortnightly Whether to generate filters per fortnightly or monthly
#' @param makeXAs1 Whether to consider records marked as X as 1 or ignore
#' @return filter dataframe with species as rows and fortnight/month as column with values in cells.

generateFilter <- function(state, filterRegion, filterPercentile=90, duration=240, fortnightly=TRUE, makeXAs1=FALSE, dataView=1)
{
  print("--- STARTING FILTER GENERATION ---")
  
  f_ebd_lists <- g_lists
  f_ebd_lists <- getLocationFilteredLists(f_ebd_lists, state, filterRegion)
  
  if(nrow(f_ebd_lists) < 1) return(NULL)
  
  f_ebd_lists <- f_ebd_lists[which(f_ebd_lists$DURATION.MINUTES < duration+1), ]
  f_ebd_lists <- within(f_ebd_lists, if (!fortnightly) Fortnight <- floor(Fortnight))
  f_ebd_lists <- subset(f_ebd_lists, select = c("GROUP.ID","Fortnight", "ALL.SPECIES.REPORTED"))
  
  if( (g_current_state == state) || (state == "None"))
  {
    f_ebd_records <- g_records
  }
  else
  {
    f_ebd_records <- getRecords(state)
    g_records <<- f_ebd_records 
  }
  
  g_current_state <<- state
  
  dt_ebd_records  = as.data.table(f_ebd_records)
  dt_ebd_lists    = as.data.table(f_ebd_lists)
  
  setkey(dt_ebd_records, GROUP.ID)
  setkey(dt_ebd_lists, GROUP.ID)
  f_ebd_records <- as.data.table(dt_ebd_records[dt_ebd_lists, nomatch=0L, on = "GROUP.ID"])  
  
  if(makeXAs1) {
    f_ebd_records$OBSERVATION.COUNT[f_ebd_records$OBSERVATION.COUNT == "X"] <- "1"
  } else {
    f_ebd_records <- f_ebd_records[which(f_ebd_records$OBSERVATION.COUNT != 'X'), ]
  }
  
  f_ebd_records <- subset(f_ebd_records, select = c("TAXONOMIC.ORDER","OBSERVATION.COUNT", "Fortnight", "ALL.SPECIES.REPORTED"))
  colnames(f_ebd_records) <- c("TOrder", "Count", "Fortnight", "AllSpecies")
  f_ebd_records <- transform(f_ebd_records, Count = as.numeric(as.character(Count)))
  
  # 1. Pivots
  filter <- dcast.data.table(f_ebd_records, TOrder ~ Fortnight, value.var = "Count", fun.aggregate = quantile, probs = filterPercentile/100, na.rm = TRUE)
  all_lists <- dcast.data.table(dt_ebd_lists, 'ALL.SPECIES.REPORTED' ~ Fortnight, value.var = "ALL.SPECIES.REPORTED", fun.aggregate = length)
  
  print("--- TRIPWIRE 1: PIVOT COMPLETE ---")
  print(paste("Rows in filter:", nrow(filter)))
  print(paste("Columns looking like:", paste(colnames(filter)[1:5], collapse=", ")))
  
  filter <- as.data.frame(filter)
  all_lists <- as.data.frame(all_lists)
  
  print("--- TRIPWIRE 2: MERGE TYPES ---")
  print(paste("Type of filter$TOrder:", class(filter$TOrder)))
  print(paste("Type of g_species$TAXONOMIC.ORDER:", class(g_species$TAXONOMIC.ORDER)))
  
  # A. Merge
  filter <- merge(filter, g_species, by.x = "TOrder", by.y = "TAXONOMIC.ORDER", all.x = TRUE)
  
  print("--- TRIPWIRE 3: AFTER MERGE ---")
  print("First 5 Bird Names mapped:")
  print(head(filter$COMMON.NAME, 5))
  
  filter$TOrder <- NULL
  filter <- filter %>% relocate(COMMON.NAME)
  
  colnames(filter)[1]    <- "Species"
  colnames(all_lists)[1] <- "Species"
  filter$Species    <- as.character(filter$Species)
  all_lists$Species <- as.character(all_lists$Species)
  all_lists$Species[1] <- "No Complete Lists"
  
  print("--- TRIPWIRE 4: BEFORE BIND ---")
  print(paste("Filter Col 1 Name:", colnames(filter)[1], "| First val:", filter[1,1]))
  print(paste("Lists Col 1 Name:", colnames(all_lists)[1], "| First val:", all_lists[1,1]))
  
  # Bind
  filter <- dplyr::bind_rows(all_lists, filter)
  filter[is.na(filter)] <- 0
  filter[, -1] <- round(filter[, -1], digits = 0)
  filter <- filter[!duplicated(filter), ]  
  
  print("--- TRIPWIRE 5: BEFORE FINAL LOOP ---")
  print(paste("Columns present:", paste(colnames(filter)[1:5], collapse=", ")))
  
  # Final Formatting Loop
  if (fortnightly) {
    for (col in 1:24) {
      if(col%%2) { colname <- as.character(as.integer((col+1)/2)) }
      else { colname <- paste(as.character(as.integer((col+1)/2)),'.',as.character(as.integer(((col+1)%%2)*5)),sep='') }
      
      if (!(colname %in% colnames(filter))) { filter[colname] <- '-' }
      col <- col + 1
    }
    filter <- filter[c("Species", "1", "1.5", "2", "2.5","3","3.5", "4", "4.5", "5","5.5", "6","6.5", "7","7.5", "8","8.5", "9","9.5", "10","10.5", "11","11.5", "12", "12.5")]
    colnames(filter) <- c("Species", "J","J","F","F","M","M","A","A","M","M","J","J","J","J","A","A","S","S","O","O","N","N","D","D")
  } else {
    for (col in 1:12) {
      colname <- as.character(col)
      if (!(colname %in% colnames(filter))) { filter[colname] <- '-' }
      col <- col + 1
    }
    filter <- filter[c("Species", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")]
    colnames(filter) <- c("Species", "J","F","M","A","M","J","J","A","S","O","N","D")
  }
  
  print("--- FINISHED ---")
  return(filter)
}