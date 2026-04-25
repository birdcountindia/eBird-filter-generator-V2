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

getLists <- function(state) {
  state_code <- unique(g_states$STATE.CODE[g_states$STATE == state])
  file_path <- paste0("data/ebd_lists_", state_code, ".rds")
  return(readRDS(file_path))
}

getRecords <- function (state)
{
  files <- paste0('data/ebd_records_',g_states[g_states$STATE == state,]$STATE.CODE,'.rds')
  
  records <- do.call("rbind", lapply(files, FUN = function(file) {
    readRDS(file)
  }))  
  return (records)  
}