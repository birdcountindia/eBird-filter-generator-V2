library (googlesheets4) 
library (splitstackshape)
library (tidyverse)
gs4_deauth()

getPolygonFilters <- function(show = NULL) {
  
  config_url <- "1vH-Ptjdz6UUAnfoZgi-aqS2YjBcjC3EbuEjB_W4lhL0"
  
  polygonfilters <- read_sheet(
    ss = config_url, 
    sheet = "PolygonFilters", 
    range = "A:I", 
    col_types = "c"
  ) |>
    dplyr::filter(!is.na(FILTER)) |>
    dplyr::distinct()
  
  # Apply optional SHOW filter
  if (!is.null(show) && show == 1) {
    polygonfilters <- polygonfilters |>
      dplyr::filter(SHOW == "1")
  }
  
  polygonfilters <- polygonfilters |>
    dplyr::mutate(POLYGON.ID = as.numeric(POLYGON.ID)) |>
    as.data.frame()
  
  return(polygonfilters)
}

getLists <- function(state, district = "None") {
  # The [1] guarantees it only ever takes one value, preventing length > 1 errors
  state_code <- unique(g_states$STATE.CODE[g_states$STATE == state])[1]
  special_states <- c("IN-KL", "IN-KA", "IN-TN", "IN-MH")
  
  if (!is.na(state_code) && state_code %in% special_states && district != "None") {
    # Match by both DISTRICT and STATE to prevent cross-state duplicate name crashes
    dist_code <- unique(g_districts$COUNTY.CODE[g_districts$COUNTY == district & g_districts$STATE == state])[1]
    file_path <- paste0("data/ebd_lists_", dist_code, ".rds")
  } else {
    file_path <- paste0("data/ebd_lists_", state_code, ".rds")
  }
  
  # Ensure the path isn't NA before checking if it exists
  if(!is.na(file_path) && file.exists(file_path)) return(readRDS(file_path))
  
  return(data.frame()) # Return empty dataframe to prevent downstream crashes
}

getRecords <- function(state, district = "None") {
  state_code <- unique(g_states$STATE.CODE[g_states$STATE == state])[1]
  special_states <- c("IN-KL", "IN-KA", "IN-TN", "IN-MH")
  
  if (!is.na(state_code) && state_code %in% special_states && district != "None") {
    dist_code <- unique(g_districts$COUNTY.CODE[g_districts$COUNTY == district & g_districts$STATE == state])[1]
    file_path <- paste0('data/ebd_records_', dist_code, '.rds')
  } else {
    file_path <- paste0('data/ebd_records_', state_code, '.rds')
  }
  
  if(!is.na(file_path) && file.exists(file_path)) return(readRDS(file_path))
  
  return(data.frame())
}