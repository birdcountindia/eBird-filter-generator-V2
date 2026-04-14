library (dplyr)
library(sf)
library(skimmr)
library(googlesheets4)
library(tictoc)
library(lubridate)
library(future.apply)

#################################################################
#                   eBird Record Stripper                       #
# This script pre-processes eBird records file and india        # 
# shape files to create RDS files for uploading to shiny        #
#################################################################

#eBird data file
ebd_file_name <- "../ebird-datasets/EBD/ebd_IN_unv_smp_relFeb-2026.txt"

#Name of the India region shape file
india_shp <- 'India_v162' 
#state <- 'IN-KL'

filtersheet <- "https://docs.google.com/spreadsheets/d/1vH-Ptjdz6UUAnfoZgi-aqS2YjBcjC3EbuEjB_W4lhL0/"

gs4_auth(email = "alenalex@ncf-india.org")
################################################################

#Unzip and read eBird records
#unzip(paste(ebd_file_name, '.zip', sep='')) #if montly scripts haven't been run yet.
ebd <- read.ebd(ebd_file_name)
preimp <- c("TAXONOMIC.ORDER", "CATEGORY","COMMON.NAME", "SUBSPECIES.COMMON.NAME","OBSERVATION.COUNT", "STATE","STATE.CODE", "COUNTY","COUNTY.CODE", "LATITUDE", "LONGITUDE", "OBSERVATION.DATE", "SAMPLING.EVENT.IDENTIFIER", "GROUP.IDENTIFIER", "DURATION.MINUTES" ,"ALL.SPECIES.REPORTED")
ebd <- ebd[, preimp]

ebd <- ebd %>%
  filter(!is.na(DURATION.MINUTES)) %>%
  mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER),SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER),
         COMMON.NAME = ifelse(CATEGORY == "issf",SUBSPECIES.COMMON.NAME, COMMON.NAME))

filterRecords <- function (state, dat) {
  print(nrow(dat))  # Print the number of rows in the input data
  print(state)      # Print the state being processed
  # Strip unwanted rows from eBird records
  ebd_records <- dat |>  filter(STATE.CODE == state)
  # Remove entries from shared lists
  ebd_records <- ebd_records |>  distinct(GROUP.ID, COMMON.NAME, .keep_all = TRUE)%>%
    select(TAXONOMIC.ORDER, OBSERVATION.COUNT, GROUP.ID)
  saveRDS(ebd_records, paste0('data/ebd_records_', state, '.rds'))
}

#Create state wise datasets
ebd_states <- ebd |>
  distinct(STATE.CODE, STATE)

#Splitting into state based records
sapply (ebd_states$STATE.CODE, filterRecords, dat = ebd)
rm(dat) #Release memory

#Create species list by removing duplicate species entries
ebd_species   <- ebd |> distinct(TAXONOMIC.ORDER, COMMON.NAME)

#Create district list by removing duplicate district entries
ebd_districts <- ebd |> distinct(COUNTY.CODE, COUNTY)

#Create unique lists by removing duplicate lists
ebd_lists     <- ebd |> distinct(GROUP.ID, .keep_all = T) |> 
  select(STATE.CODE, COUNTY.CODE,OBSERVATION.DATE, DURATION.MINUTES, 
         LONGITUDE,LATITUDE,GROUP.ID, ALL.SPECIES.REPORTED)
# At this point, the primary ebd data is no longer needed
#rm(ebd) #Release memory

ebd_lists <- ebd_lists %>%
  mutate(obs_date = as.Date(OBSERVATION.DATE),
    Fortnight = month(obs_date) +
      0.5 * as.integer(0.5 + day(obs_date) / days_in_month(obs_date))) %>%
  select(-OBSERVATION.DATE)

#Open the shape file
indiamap <- st_read(paste0("data/",india_shp,".geojson"))

indiamap <- indiamap %>%
  rename(POLYGON.ID = id)

#Whenever a new Polygon file is loaded use this block to update the gsheet.

# match_regions <- function(indiamap, ebd_states, ebd_districts, target_col = "subregion") {
#   indiamap$STATE <- NA
#   indiamap$COUNTY <- NA
#   
#   clean_states <- na.omit(unique(ebd_states$STATE))
#   for (st in clean_states) {
#     match_idx <- grepl(st, indiamap[[target_col]], ignore.case = TRUE)
#     indiamap$STATE[match_idx] <- st}
#   
#   clean_districts <- na.omit(unique(ebd_districts$COUNTY))
#   for (dt in clean_districts) {
#     match_idx <- grepl(dt, indiamap[[target_col]], ignore.case = TRUE)
#     indiamap$COUNTY[match_idx] <- dt}
#   
#   indiamap$STATE[is.na(indiamap$STATE)] <- "spl_region"
#   indiamap$COUNTY[is.na(indiamap$COUNTY)] <- "spl_region"
#   
#   return(indiamap)
# }
# 
# indiamap <- match_regions(indiamap, ebd_states, ebd_districts, target_col = "subregion")
# 
# sheet_write(
#   data = sf::st_drop_geometry(indiamap), 
#   ss = filtersheet,
#   sheet = india_shp  
# )

ebd_lists_sf <- sf::st_as_sf(ebd_lists, 
                             coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Map the CRS
sf::st_crs(ebd_lists_sf) <- sf::st_crs(indiamap)

ebd_lists_with_filter <- NULL

# Fix geometries if invalid #maybe unnecessary 
indiamap <- sf::st_make_valid(indiamap)
ebd_lists_sf <- sf::st_make_valid(ebd_lists_sf)

# Ensure CRS alignment
sf::st_crs(ebd_lists_sf) <- sf::st_crs(indiamap)

tic("Vectorized Intersection")
sf::sf_use_s2(FALSE)
ebd_lists_with_filter <- sf::st_intersection(ebd_lists_sf, indiamap["POLYGON.ID"])
ebd_lists_with_filter <- sf::st_drop_geometry(ebd_lists_with_filter)
sf::sf_use_s2(TRUE)
toc()

ebd_polygons <- data.frame(
  POLYGON.ID = indiamap$POLYGON.ID,
  POLYGON = as.character(indiamap$subregion), 
  stringsAsFactors = FALSE
)
# Strip the list before joining
ebd_lists_with_filter <- subset(as.data.frame(ebd_lists_with_filter), select = c("GROUP.ID", "POLYGON.ID"))

ebd_lists <- ebd_lists %>%
  left_join(ebd_lists_with_filter, by = "GROUP.ID") %>%
  mutate(POLYGON.ID = coalesce(POLYGON.ID, 0))

saveRDS(ebd_species,'data/ebd_species.rds')
saveRDS(ebd_states,'data/ebd_states.rds')
saveRDS(ebd_districts,'data/ebd_districts.rds')
saveRDS(ebd_polygons,'data/ebd_polygons.rds')
saveRDS(ebd_lists,'data/ebd_lists.rds')

#Remove temp files
unlink ('*.txt')
unlink ('*.pdf')
unlink (paste(india_shp,'.*',sep=''))

# Generate interactive map based on updated data
source("mapprep.R")

