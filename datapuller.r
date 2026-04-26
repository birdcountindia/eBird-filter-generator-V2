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
  state_code <- unique(g_states$STATE.CODE[g_states$STATE == state])
  special_states <- c("IN-KL", "IN-KA", "IN-TN", "IN-MH")
  
  if (state_code %in% special_states && district != "None") {
    dist_code <- unique(g_districts$COUNTY.CODE[g_districts$COUNTY == district])
    file_path <- paste0("data/ebd_lists_", dist_code, ".rds")
  } else {
    file_path <- paste0("data/ebd_lists_", state_code, ".rds")
  }
  
  if(file.exists(file_path)) return(readRDS(file_path))
  return(data.frame()) # Return empty dataframe if file is missing to prevent crashes
}

getRecords <- function(state, district = "None") {
  state_code <- unique(g_states$STATE.CODE[g_states$STATE == state])
  special_states <- c("IN-KL", "IN-KA", "IN-TN", "IN-MH")
  
  if (state_code %in% special_states && district != "None") {
    dist_code <- unique(g_districts$COUNTY.CODE[g_districts$COUNTY == district])
    file_path <- paste0('data/ebd_records_', dist_code, '.rds')
  } else {
    file_path <- paste0('data/ebd_records_', state_code, '.rds')
  }
  
  if(file.exists(file_path)) return(readRDS(file_path))
  return(data.frame())
}