library (googlesheets4) 
library (splitstackshape)
library (tidyverse)
gs4_auth(email = "alenalex@ncf-india.org")

getPolygonFilters <- function ()
{
  config_url <- "1vH-Ptjdz6UUAnfoZgi-aqS2YjBcjC3EbuEjB_W4lhL0"
  polygonfilters <- read_sheet(
    ss = config_url, 
    sheet = "PolygonFilters", 
    range = "A:F", 
    col_types = "c"
  )
  polygonfilters <- as.data.frame(polygonfilters)
  polygonfilters <- polygonfilters %>% 
    drop_na(FILTER) %>%
    distinct() 
  polygonfilters$POLYGON.ID <- as.numeric(polygonfilters$POLYGON.ID)
  
  return (polygonfilters)
}

getRecords <- function (state)
{
  files <- paste0('data/ebd_records_',g_states[g_states$STATE == state,]$STATE.CODE,'.rds')
  
  records <- do.call("rbind", lapply(files, FUN = function(file) {
    readRDS(file)
  }))  
  return (records)  
}