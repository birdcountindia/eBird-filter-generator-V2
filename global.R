source("datapuller.R")

g_states    <- readRDS('data/ebd_states.rds')
g_species   <- readRDS('data/ebd_species.rds')
g_districts <- readRDS('data/ebd_districts.rds')
g_polygons  <- readRDS('data/ebd_polygons.rds')

firstState  <- g_states$STATE[1]
g_records   <- getRecords(firstState)
g_lists     <- getLists(firstState)    
g_current_state <- firstState
g_current_district <- "None"