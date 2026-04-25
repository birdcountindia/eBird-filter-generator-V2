source("datapuller.R")

g_states    <- readRDS('data/ebd_states.rds')
g_species   <- readRDS('data/ebd_species.rds')
g_districts <- readRDS('data/ebd_districts.rds')
g_polygons  <- readRDS('data/ebd_polygons.rds')

g_filters <- getPolygonFilters()
# Extract the exact 'FILTER' column to populate the app's dropdown menu
g_all_filters <- sort(unique(g_filters$FILTER))

firstState  <- g_states$STATE[1]
g_records   <- getRecords(firstState)
g_current_state <- firstState
