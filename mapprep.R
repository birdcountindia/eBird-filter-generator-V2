library(sf)          
library(jsonlite)    
library(dplyr)
library(base64enc) 
source("datapuller.r")

#indiamap is loaded in the previous script if not uncomment this
india_shp <- 'India_v239'
indiamap <- st_read(paste0("data/",india_shp,".geojson"))
indiamap <- indiamap %>%
  rename(POLYGON.ID = id)


old_indiamap <- st_read("data/old_polygons/indiama-editedSQ.shp")
st_write(old_indiamap, "data/map/old_map.geojson", delete_dsn = TRUE, quiet = TRUE)

print("--- STARTING MAP PREPARATION ---")

sheet_data <- getPolygonFilters()

map_data <- indiamap %>%
  left_join(sheet_data, by = "POLYGON.ID")

print("Saving processed map data...")

saveRDS(map_data, "data/map/processed_map_data.rds")
st_write(map_data, "data/map/processed_map_data.geojson", delete_dsn = TRUE, quiet = TRUE)

print("Generating individual state GeoJSON files...")
unique_states <- na.omit(unique(map_data$STATE)) 

for (st in unique_states) {
  state_data <- map_data %>% filter(STATE == st)
  safe_st <- gsub("[^A-Za-z0-9]", "_", st)
  file_path <- paste0("data/map/", safe_st, ".geojson")
  st_write(state_data, file_path, delete_dsn = TRUE, quiet = TRUE)
}

# 3. Prepare Assets for HTML (Logo & Colors)
print("Encoding Logo and Color Palette...")

# Convert the PNG logo to a Base64 string so it lives inside the HTML code
logo_path <- "data/logo/bci.png"
logo_base64 <- dataURI(file = logo_path, mime = "image/png")

# Building Colours
map_data$FILTER <- trimws(as.character(map_data$FILTER))
map_data$STATE <- trimws(as.character(map_data$STATE))
state_filter_colors <- list()
unique_states <- na.omit(unique(map_data$STATE))

for (st in unique_states) {
  state_filters <- na.omit(unique(map_data$FILTER[map_data$STATE == st]))
  raw_colors <- rainbow(length(state_filters))
  clean_colors <- substr(raw_colors, 1, 7)
  state_filter_colors[[st]] <- as.list(setNames(clean_colors, state_filters))
}

js_color_map <- toJSON(state_filter_colors, auto_unbox = TRUE)

# Generate HTML dropdown options
dropdown_options <- paste0(
  "<option value='",
  gsub("[^A-Za-z0-9]", "_", sort(unique_states)),
  "'>",
  sort(unique_states),
  "</option>",
  collapse = "\n"
)
# 5. Define GitHub Raw URL 
github_repo_url <- "https://raw.githubusercontent.com/birdcountindia/eBird-filter-generator-V2/main/data/map/"

# 6. Generate Single HTML Application
print("Generating self-contained HTML map file...")

html_content <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>eBird India Editor Polygons</title>
    
    <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
    <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>

    <style>
      #controls { display: flex; align-items: center; gap: 20px; }
        .slider-container { display: flex; align-items: center; gap: 8px; font-size: 14px; font-weight: bold; }
        
        body { margin: 0; padding: 0; font-family: "Segoe UI", Roboto, Helvetica, Arial, sans-serif; }
        
        /* Ribbon/Header Styling */
        #header {
            height: 60px;
            background-color: #2c3e50; /* Dark slate blue */
            color: white;
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 0 20px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.3);
            position: relative;
            z-index: 1000;
        }
        #header-left { display: flex; align-items: center; gap: 15px; }
        #header h1 { font-size: 20px; margin: 0; font-weight: 600; letter-spacing: 0.5px; }
        .logo { height: 45px; border-radius: 4px; background-color: white; padding: 2px;}
        
        /* Dropdown Styling */
        #stateSelect {
            padding: 8px 15px;
            font-size: 15px;
            border-radius: 5px;
            border: 1px solid #bdc3c7;
            cursor: pointer;
            background-color: white;
            color: #333;
            font-weight: bold;
        }
        
        #map { height: calc(100vh - 60px); width: 100%; }
        
        .leaflet-popup-content b { color: #2c3e50; }
    </style>
</head>
<body>

    <div id="header">
        <div id="header-left">
            <img src="', logo_base64, '" class="logo" alt="BCI Logo" onerror="this.style.display=\\\'none\\\'">
            <h1>eBird India Editor Polygons</h1>
        </div>
        <div id="controls">
            <div class="slider-container">
                <label for="opacitySlider">Fill Opacity:</label>
                <input type="range" id="opacitySlider" min="0" max="1" step="0.1" value="0.6">
            </div>
            
            <select id="stateSelect">
                <option value="">-- Select a State --</option>
                ', dropdown_options, '
            </select>
        </div>
    </div>

    <div id="map"></div>

    <script>
        // 1. Initialize Map
        var map = L.map("map").setView([20.5937, 78.9629], 5); // Centers on India

        // 2. Add Google Maps Layers (Roadmap & Satellite)
        var roadmap = L.tileLayer("https://mt1.google.com/vt/lyrs=m&hl=en&gl=in&x={x}&y={y}&z={z}", {
            attribution: "Google Maps",
            maxZoom: 20
        }).addTo(map);

        var satellite = L.tileLayer("https://mt1.google.com/vt/lyrs=s&hl=en&gl=in&x={x}&y={y}&z={z}", {
            attribution: "Google Maps Satellite",
            maxZoom: 20
        });

        // --- NEW: Overlay Map Layer for Old Boundaries ---
        var oldMapLayer = L.layerGroup(); 

        // Add Toggle Control for Maps
        L.control.layers(
            { "Google Roadmap": roadmap, "Google Satellite": satellite },
            { "Old Boundaries": oldMapLayer } // Adds the toggle box!
        ).addTo(map);

        // 3. Global Variables
        var currentLayer = null;
        var colorMap = ', js_color_map, ';
        var githubBaseUrl = "', github_repo_url, '";

        // --- NEW: Fetch and prepare the Old Map ---
        fetch(githubBaseUrl + "old_map.geojson")
            .then(res => res.json())
            .then(data => {
                L.geoJSON(data, {
                    style: {
                        fillColor: "transparent",
                        fillOpacity: 0, 
                        color: "white",
                        weight: 2,
                        opacity: 1
                    },
                    onEachFeature: function(feature, layer) {
                        // WE NOW HARDCODE THE EXACT COLUMN NAME: AREA_1
                        var polyName = feature.properties.AREA_1 || feature.properties.area_1 || "Unknown";
                        
                        layer.bindTooltip("Old Polygon: " + polyName, { 
                            sticky: true,
                            className: "custom-tooltip"
                        });
                    }
                }).addTo(oldMapLayer);
            })
            .catch(err => console.log("Old map geojson not loaded.", err));
        var opacitySlider = document.getElementById("opacitySlider");
        opacitySlider.addEventListener("input", function(e) {
            if (currentLayer) {
                currentLayer.setStyle({ fillOpacity: parseFloat(e.target.value) });
            }
        });
        
        // 4. Dropdown Change Listener (The GitHub Fetch Mechanism)
        document.getElementById("stateSelect").addEventListener("change", function(e) {
            var stateFile = e.target.value;
            
            // If they select "None", clear map and reset view
            if (!stateFile) {
                if(currentLayer) map.removeLayer(currentLayer);
                map.setView([20.5937, 78.9629], 5);
                return;
            }

            // Construct GitHub URL
            var url = githubBaseUrl + stateFile + ".geojson";

            fetch(url)
                .then(response => {
                    if (!response.ok) throw new Error("Network response was not ok");
                    return response.json();
                })
                .then(data => {
                    if(currentLayer) map.removeLayer(currentLayer);

                    // Draw the Polygons
                    currentLayer = L.geoJSON(data, {
                        style: function(feature) {
                            // 1. Get the exact State and Filter names
                            var rawState = feature.properties.STATE || "";
                            var cleanState = String(rawState).trim();
                            
                            var rawName = feature.properties.FILTER || "";
                            var cleanName = String(rawName).trim();
                            
                            // 2. Look up the specific dictionary for this state
                            var stateColors = colorMap[cleanState];
                            
                            // 3. Find the filters color inside that states dictionary
                            var polyColor = stateColors ? stateColors[cleanName] : null;
                            
                            if (!polyColor) {
                                console.log("FAILED MATCH! State: [" + cleanState + "] Filter: [" + cleanName + "]");
                                polyColor = "#3388ff"; 
                            }

                            return {
                                fillColor: polyColor,
                                fillOpacity: parseFloat(opacitySlider.value), 
                                color: "black",
                                weight: 3,
                                opacity: 1 
                            };
                        },
                        onEachFeature: function(feature, layer) {
                            // Hover Tooltip
                            layer.bindTooltip(feature.properties.FILTER || "Unknown", {
                                sticky: true,
                                className: "custom-tooltip"
                            });

                            // Click Popup
                            // JS Note: When sf writes GeoJSON, columns with dots usually stay with dots
                            var pID = feature.properties["POLYGON.ID"] || feature.properties.POLYGON_ID || "N/A";
                            
                            var popupContent = "<div style=\\\'font-size: 14px;\\\'>" +
                                "<b>Polygon Name:</b> " + (feature.properties.POLYGON || "N/A") + "<br/>" +
                                "<b>Polygon ID:</b> " + pID + "<br/>" +
                                "<b>Filter:</b> " + (feature.properties.FILTER || "N/A") + "<br/>" +
                                "<b>State:</b> " + (feature.properties.STATE || "N/A") + "<br/>" +
                                "<b>District:</b> " + (feature.properties.COUNTY || "N/A") + "<br/>" +
                                "<b>Owner:</b> " + (feature.properties.OWNER || "N/A") +
                                "</div>";
                            layer.bindPopup(popupContent);
                        }
                    }).addTo(map);

                    // Zoom to fit the state
                    map.fitBounds(currentLayer.getBounds());
                })
                .catch(err => {
                    console.error("Error loading state data from GitHub:", err);
                    alert("Could not load map data. Have you pushed the latest geojson files to GitHub yet?");
                });
        });
    </script>
</body>
</html>
')

# Write the final HTML file
writeLines(html_content, "index.html")
print("--- MAP PREPARATION COMPLETE ---")