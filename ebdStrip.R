# eBird Record Stripper                       
# This script pre-processes eBird records file for India shape files to create RDS files for uploading to shiny        
# This script has to be run before the tool is pushed to Shiny

library (dplyr)
library(sf)
library(skimmr)
library(googlesheets4)
library(tictoc)
library(lubridate)
library(future.apply)

source("datapuller.R")

#eBird data file path
ebd_file_name <- "../ebird-datasets/EBD/ebd_IN_unv_smp_relMar-2026.txt"

#Name of the India region shape file
india_shp <- 'India_v249' 

filtersheet <- "https://docs.google.com/spreadsheets/d/1vH-Ptjdz6UUAnfoZgi-aqS2YjBcjC3EbuEjB_W4lhL0/"
gs4_auth(email = "alenalex@ncf-india.org")

#Unzip and read eBird records
#unzip(paste(ebd_file_name, '.zip', sep='')) # if montly scripts haven't been run yet.
ebd <- read.ebd(ebd_file_name)
preimp <- c("TAXONOMIC.ORDER", "CATEGORY","COMMON.NAME", "SUBSPECIES.COMMON.NAME","OBSERVATION.COUNT", "STATE","STATE.CODE", "COUNTY","COUNTY.CODE", "LATITUDE", "LONGITUDE", "OBSERVATION.DATE", "SAMPLING.EVENT.IDENTIFIER", "GROUP.IDENTIFIER", "DURATION.MINUTES" ,"ALL.SPECIES.REPORTED")
ebd <- ebd[, preimp]

print("Extracting checklist locations for JSON search...")
checklist_locations <- ebd |>
  distinct(STATE.CODE, COUNTY.CODE, SAMPLING.EVENT.IDENTIFIER, LATITUDE, LONGITUDE)
unique_st_codes <- na.omit(unique(checklist_locations$STATE.CODE))
special_states <- c("IN-KL", "IN-KA", "IN-TN", "IN-MH")
for (st_code in unique_st_codes) {
    if (st_code %in% special_states) {
    state_data <- checklist_locations |> filter(STATE.CODE == st_code)
    unique_counties <- na.omit(unique(state_data$COUNTY.CODE))
    
    for (cty_code in unique_counties) {
      cty_json <- state_data |>
        filter(COUNTY.CODE == cty_code) |>
        select(SAMPLING.EVENT.IDENTIFIER, LATITUDE, LONGITUDE)
      jsonlite::write_json(cty_json, paste0("data/json/", cty_code, "_lists.json"))
    }
    
  } else {
    st_json <- checklist_locations |>
      filter(STATE.CODE == st_code) |>
      select(SAMPLING.EVENT.IDENTIFIER, LATITUDE, LONGITUDE)
    jsonlite::write_json(st_json, paste0("data/json/", st_code, "_lists.json"))
  }
}

rm(checklist_locations, unique_st_codes, cty_json, st_json, state_data)
gc()

ebd <- ebd %>%
  filter(!is.na(DURATION.MINUTES)) %>%
  mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER),SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER),
         COMMON.NAME = ifelse(CATEGORY == "issf",SUBSPECIES.COMMON.NAME, COMMON.NAME))

#Create state wise datasets
ebd_states <- ebd |>
  distinct(STATE.CODE, STATE)

#Create district list by removing duplicate district entries
ebd_districts <- ebd |> distinct(STATE, STATE.CODE, COUNTY.CODE, COUNTY)

# This function saves eBird observations, statewise for small states and district wise for larger states.
for (st in na.omit(unique(ebd$STATE.CODE))) {
  st_records <- ebd |> filter(STATE.CODE == st) |>
    distinct(GROUP.ID, COMMON.NAME, .keep_all = TRUE) |>
    select(TAXONOMIC.ORDER, OBSERVATION.COUNT, GROUP.ID)
  
  if (st %in% special_states) {
    dists <- unique(ebd_districts$COUNTY.CODE[ebd_districts$STATE.CODE == st])
    for (dist in dists) {
      dist_records <- ebd |> filter(COUNTY.CODE == dist) |>
        distinct(GROUP.ID, COMMON.NAME, .keep_all = TRUE) |>
        select(TAXONOMIC.ORDER, OBSERVATION.COUNT, GROUP.ID)
      saveRDS(dist_records, paste0('data/ebd_records_', dist, '.rds'))
    }
  } else {
    saveRDS(st_records, paste0('data/ebd_records_', st, '.rds'))
  }
}

#Create species list by removing duplicate species entries
ebd_species   <- ebd |> distinct(TAXONOMIC.ORDER, COMMON.NAME)

#Create unique lists by removing duplicate lists
ebd_lists     <- ebd |> distinct(GROUP.ID, .keep_all = T) |> 
  select(STATE.CODE, COUNTY.CODE,OBSERVATION.DATE, DURATION.MINUTES, 
         LONGITUDE,LATITUDE,GROUP.ID, ALL.SPECIES.REPORTED)
# At this point, the primary ebd data is no longer needed
rm(ebd) #Release memory

ebd_lists <- ebd_lists %>%
  mutate(obs_date = as.Date(OBSERVATION.DATE),
    Fortnight = month(obs_date) +
      0.5 * as.integer(0.5 + day(obs_date) / days_in_month(obs_date))) %>%
  select(-OBSERVATION.DATE)

#Open the shape file
indiamap <- st_read(paste0("data/",india_shp,".geojson"))
indiamap <- indiamap %>%
  rename(POLYGON.ID = id) |> 
  rename(POLYGON = subregion)
# # 
# temp <- getPolygonFilters()
# temp <- temp %>%
#   select(-POLYGON) %>%
#   left_join(indiamap %>% select(POLYGON.ID, POLYGON), by = "POLYGON.ID")
# temp <- temp %>%
#   select(-any_of("geometry")) %>%
#   relocate(POLYGON, .before = everything()) %>%
#   arrange(POLYGON)
# # # 
# # #Whenever a new Polygon file is loaded use this block to update the gsheet.
# # #GSheet Process ----------------------------------------------------------
# # 
# # ebd_districts <- readRDS("data/ebd_districts.rds")
# # ebd_states <- readRDS("data/ebd_states.rds") #if necessarry
# 
# match_regions <- function(indiamap, ebd_states, ebd_districts, target_col = "POLYGON") {
#   indiamap$STATE <- NA
#   indiamap$COUNTY <- NA
# 
#   clean_states <- na.omit(unique(ebd_states$STATE))
#   clean_states <- clean_states[order(nchar(clean_states), decreasing = TRUE)]
#   indiamap$STATE <- NA
#   for (st in clean_states) {
#     match_idx <- grepl(st, indiamap[[target_col]], ignore.case = TRUE)
#     indiamap$STATE[match_idx] <- st
#   }
# 
#   clean_districts <- na.omit(unique(ebd_districts$COUNTY))
#   clean_districts <- clean_districts[order(nchar(clean_districts), decreasing = TRUE)]
#   indiamap$COUNTY <- NA
#   for (dt in clean_districts) {
#     match_idx <- grepl(dt, indiamap[[target_col]], ignore.case = TRUE)
#     indiamap$COUNTY[match_idx] <- dt
#   }
# 
#   indiamap$STATE[is.na(indiamap$STATE)] <- "spl_region"
#   indiamap$COUNTY[is.na(indiamap$COUNTY)] <- "spl_region"
# 
#   return(indiamap)
# }
# 
# indiamap <- match_regions(indiamap, ebd_states, ebd_districts, target_col = "POLYGON")
# indiamap <- indiamap |> arrange(POLYGON)
# 
# sheet_write(
#   data = sf::st_drop_geometry(indiamap),
#   ss = filtersheet,
#   sheet = india_shp
# )

#  -----------------------------------------------------------------------

ebd_lists_sf <- sf::st_as_sf(ebd_lists, 
                             coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
# Map the CRS
sf::st_crs(ebd_lists_sf) <- sf::st_crs(indiamap)

ebd_lists_with_polygon <- NULL

# Fix geometries if invalid # maybe unnecessary 
indiamap <- sf::st_make_valid(indiamap)
ebd_lists_sf <- sf::st_make_valid(ebd_lists_sf)

# Ensure CRS alignment
sf::st_crs(ebd_lists_sf) <- sf::st_crs(indiamap)

tic("Assigning Checklists to corresponding polygons") # ~ 68 mins
sf::sf_use_s2(FALSE)
ebd_lists_with_polygon <- sf::st_intersection(ebd_lists_sf, indiamap["POLYGON.ID"])
ebd_lists_with_polygon <- sf::st_drop_geometry(ebd_lists_with_polygon)
sf::sf_use_s2(TRUE)
toc()

ebd_polygons <- data.frame(
  POLYGON.ID = indiamap$POLYGON.ID,
  POLYGON = as.character(indiamap$POLYGON), 
  stringsAsFactors = FALSE
)
# Strip the list before joining
ebd_lists_with_polygon <- subset(as.data.frame(ebd_lists_with_polygon), select = c("GROUP.ID", "POLYGON.ID"))

ebd_lists <- ebd_lists %>%
  left_join(ebd_lists_with_polygon, by = "GROUP.ID") %>%
  mutate(POLYGON.ID = coalesce(POLYGON.ID, 0))

# This function saves eBird lists, state wise for small states and district wise for larger states.
for (st in na.omit(unique(ebd_lists$STATE.CODE))) {
  st_lists <- ebd_lists |> filter(STATE.CODE == st)
  
  if (st %in% special_states) {
    dists <- unique(ebd_districts$COUNTY.CODE[ebd_districts$STATE.CODE == st])
    for (dist in dists) {
      dist_lists <- st_lists |> filter(COUNTY.CODE == dist)
      saveRDS(dist_lists, paste0('data/ebd_lists_', dist, '.rds'))
    }
  } else {
    saveRDS(st_lists, paste0('data/ebd_lists_', st, '.rds'))
  }
}
rm(ebd_lists)

saveRDS(ebd_species,'data/ebd_species.rds')
saveRDS(ebd_states,'data/ebd_states.rds')
saveRDS(ebd_districts,'data/ebd_districts.rds')
saveRDS(ebd_polygons,'data/ebd_polygons.rds')

#Remove temp files
unlink ('*.txt')
unlink ('*.pdf')
unlink (paste(india_shp,'.*',sep=''))

# Generate interactive map based on updated data
source("mapprep.R")
