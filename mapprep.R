library(sf)
library(jsonlite)
library(dplyr)
library(base64enc)
source("datapuller.R")

#indiamap is loaded in the previous script if not uncomment this
india_shp <- 'India_v249'
indiamap <- st_read(paste0("data/",india_shp,".geojson"))
indiamap <- indiamap %>%
  rename(POLYGON.ID = id)

old_indiamap <- st_read("data/old_polygons/indiama-editedSQ.shp")
st_write(old_indiamap, "data/map/old_map.geojson", delete_dsn = TRUE, quiet = TRUE)

print("--- STARTING MAP PREPARATION ---")

sheet_data <- getPolygonFilters(show=1)

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

ebird_api_key <- trimws(readLines("data/key.txt", warn = FALSE))

# Building Colours
map_data$FILTER <- trimws(as.character(map_data$FILTER))
map_data$STATE <- trimws(as.character(map_data$STATE))
state_filter_colors <- list()
unique_states <- na.omit(unique(map_data$STATE))

set.seed(100)

state_filter_colors <- setNames(lapply(unique_states, function(st) {
  
  state_filters <- na.omit(unique(map_data$FILTER[map_data$STATE == st]))
  n_filters <- length(state_filters)
  if (n_filters == 0) return(list())
  
  raw_colors <- hcl.colors(n = n_filters, palette = "Dark 2")
  clean_colors <- substr(raw_colors, 1, 7)
  shuffled_colors <- sample(clean_colors, size = n_filters)
  as.list(setNames(shuffled_colors, state_filters))
  
}), unique_states)

js_color_map <- toJSON(state_filter_colors, auto_unbox = TRUE)
writeLines(js_color_map, "data/map/colors.json")
print("Saved colors.json to data/map/")

print("Scanning JSON directory for search indices...")
json_files <- list.files("data/json", pattern = "_lists\\.json$")
search_prefixes <- gsub("_lists\\.json$", "", json_files)
js_search_codes <- jsonlite::toJSON(search_prefixes, auto_unbox = FALSE)

print("Generating State Code Dictionary for API...")
g_states <- readRDS('data/ebd_states.rds')
state_mapping <- g_states %>% select(STATE, STATE.CODE) %>% distinct()
safe_states <- gsub("[^A-Za-z0-9]", "_", state_mapping$STATE)
state_code_dict <- setNames(as.list(state_mapping$STATE.CODE), safe_states)
js_state_codes_map <- jsonlite::toJSON(state_code_dict, auto_unbox = TRUE)

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
github_json_url <- "https://raw.githubusercontent.com/birdcountindia/eBird-filter-generator-V2/main/data/json/"

g_states <- readRDS('data/ebd_states.rds')
js_state_codes <- jsonlite::toJSON(na.omit(unique(g_states$STATE.CODE)), auto_unbox = FALSE)

# 6. Generate Single HTML Application
print("Generating self-contained HTML map file...")

# =========================================================================
# THE HTML INJECTION STRING (All quotes inside are double, or backticks)
# =========================================================================
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
        
        /* --- UPDATED: Flexbox layout so the header can wrap automatically --- */
        body { 
            margin: 0; padding: 0; 
            font-family: "Segoe UI", Roboto, Helvetica, Arial, sans-serif; 
            display: flex; 
            flex-direction: column; 
            height: 100vh; 
            overflow: hidden; 
        }
        
        #header {
            background-color: #2c3e50;
            color: white;
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 10px 20px; 
            box-shadow: 0 2px 5px rgba(0,0,0,0.3);
            position: relative;
            z-index: 1000;
            flex-wrap: wrap; /* Allows the header to grow taller if needed */
            gap: 10px;
        }
        
        #controls { 
            display: flex; 
            align-items: center; 
            gap: 15px; 
            flex-wrap: wrap; /* Allows controls to drop to a second line */
        }
        
        .slider-container { display: flex; align-items: center; gap: 8px; font-size: 14px; font-weight: bold; }
        /* ... keep your existing search-container, search-input styles here ... */
        
        #map { 
            flex-grow: 1; /* Automatically fills whatever vertical space is left! */
            width: 100%; 
        }
        
        .leaflet-popup-content b { color: #2c3e50; }
    </style>
</head>
<body>

    <div id="header">
        <div id="header-left">
            <img src="', logo_base64, '" class="logo" alt="BCI Logo" onerror="this.style.display=\"none\"">
            <h1>eBird India Editor Polygons</h1>
        </div>
        <div id="controls">
            <div class="search-container">
                <input type="text" id="checklistInput" class="search-input" placeholder="Checklist ID or URL">
                <button id="fetchBtn" class="search-btn">Plot Checklist</button>
            </div>
            <div class="slider-container">
                <label for="opacitySlider">Fill Opacity:</label>
                <input type="range" id="opacitySlider" min="0" max="1" step="0.1" value="0.6">
            </div>
            
            <select id="stateSelect">
                <option value="">-- Select a State --</option>
                ', dropdown_options, '
            </select>
            <button id="hotspotBtn" class="search-btn" style="display: none; background-color: #f39c12;">Show Hotspots</button>
            
            <div id="secondaryControls" style="display: none; align-items: center; gap: 10px; margin-left: 15px; padding-left: 15px; border-left: 2px solid #bdc3c7;">
                <select id="secondaryStateSelect" style="padding: 6px; border-radius: 4px;">
                    <option value="">-- Compare State --</option>
                </select>
                <button id="secondaryHotspotBtn" class="search-btn" style="background-color: #8e44ad;">Add 2nd Layer</button>
            </div>
        </div>
    </div>

    <div id="map"></div>

    <script>
        // 1. Initialize Map
        var map = L.map("map").setView([20.5937, 78.9629], 5); // Centers on India

        // 2. Add Google Maps Layers (Roadmap & Satellite Hybrid)
        var roadmap = L.tileLayer("https://mt1.google.com/vt/lyrs=m&hl=en&gl=in&x={x}&y={y}&z={z}", {
            attribution: "Google Maps",
            maxZoom: 20
        }).addTo(map);

        var satellite = L.tileLayer("https://mt1.google.com/vt/lyrs=y&hl=en&gl=in&x={x}&y={y}&z={z}", {
            attribution: "Google Maps Satellite",
            maxZoom: 20
        });

        // Overlay Map Layer for Old Boundaries
        var oldMapLayer = L.layerGroup(); 

        // Add Toggle Control for Maps
        L.control.layers(
            { "Google Roadmap": roadmap, "Google Satellite": satellite },
            { "Old Boundaries": oldMapLayer }
        ).addTo(map);

        // 3. Global Variables
        var currentLayer = null;
        var colorMap = ', js_color_map, ';
        var githubBaseUrl = "', github_repo_url, '";

        // Fetch and prepare the Old Map
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
                            var rawState = feature.properties.STATE || "";
                            var cleanState = String(rawState).trim();
                            
                            var rawName = feature.properties.FILTER || "";
                            var cleanName = String(rawName).trim();
                            
                            var stateColors = colorMap[cleanState];
                            var polyColor = stateColors ? stateColors[cleanName] : null;
                            
                            if (!polyColor) {
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
                            layer.bindTooltip(feature.properties.FILTER || "Unknown", {
                                sticky: true,
                                className: "custom-tooltip"
                            });

                            var pID = feature.properties["POLYGON.ID"] || feature.properties.POLYGON_ID || "N/A";
                            
                            // USING TEMPLATE LITERALS FOR POPUP (Safe from R parsing)
                            var popupContent = `<div style="font-size: 14px;">
                                <b>Polygon Name:</b> ${feature.properties.POLYGON || "N/A"}<br/>
                                <b>Polygon ID:</b> ${pID}<br/>
                                <b>Filter:</b> ${feature.properties.FILTER || "N/A"}<br/>
                                <b>State:</b> ${feature.properties.STATE || "N/A"}<br/>
                                <b>District:</b> ${feature.properties.COUNTY || "N/A"}<br/>
                                <b>Owner:</b> ${feature.properties.OWNER || "N/A"}
                                </div>`;
                            layer.bindPopup(popupContent);
                        }
                    }).addTo(map);

                    map.fitBounds(currentLayer.getBounds());
                })
                .catch(err => {
                    console.error("Error loading state data from GitHub:", err);
                    alert("Could not load map data. Have you pushed the latest geojson files to GitHub yet?");
                });
        });

        // ----------------------------------------------------
        // CHECKLIST SEARCH LOGIC
        // ----------------------------------------------------
        var checklistInput = document.getElementById("checklistInput");
        var fetchBtn = document.getElementById("fetchBtn");
        var checklistMarker = null;
        var searchCodes = ', js_search_codes, ';

        fetchBtn.addEventListener("click", function() {
            var rawChecklist = checklistInput.value.trim();
            var match = rawChecklist.match(/([S|G]\\d{6,})/i); 
            if (!match) {
                alert("Could not find a valid checklist ID (e.g., S12345678) in your input.");
                return;
            }
            var subId = match[1].toUpperCase();

            var originalBtnText = fetchBtn.innerText;
            fetchBtn.innerText = "Searching...";
            var githubJsonUrl = "', github_json_url, '";

            var fetchPromises = searchCodes.map(code => {
                return fetch(githubJsonUrl + code + "_lists.json")
                    .then(res => res.ok ? res.json() : null)
                    .then(data => {
                        if (!data) return null;
                        return data.find(row => row["SAMPLING.EVENT.IDENTIFIER"] === subId) || null;
                    })
                    .catch(err => null);
            });

            Promise.all(fetchPromises)
                .then(results => {
                    var foundData = results.find(r => r !== null);

                    if (foundData) {
                        var lat = foundData.LATITUDE;
                        var lng = foundData.LONGITUDE;

                        if (checklistMarker) { map.removeLayer(checklistMarker); }
                        checklistMarker = L.marker([lat, lng]).addTo(map);
                        
                        var popupHTML = `<div style="font-size: 14px;">
                            <b>Checklist:</b> <a href="https://ebird.org/checklist/${subId}" target="_blank">${subId}</a><br/>
                            <b>Location:</b> Coordinates from local dataset<br/>
                            </div>`;
                        
                        checklistMarker.bindPopup(popupHTML).openPopup();
                        map.setView([lat, lng], 13);
                    } else {
                        alert("Checklist " + subId + " not found in the generated dataset.");
                    }
                })
                .catch(err => {
                    console.error("Search Error:", err);
                    alert("An error occurred while searching the datasets.");
                })
                .finally(() => {
                    fetchBtn.innerText = originalBtnText; 
                });
        });
        
        // ----------------------------------------------------
        // PRIMARY HOTSPOT LOGIC
        // ----------------------------------------------------
        var hotspotBtn = document.getElementById("hotspotBtn");
        var hotspotsLayer = L.layerGroup().addTo(map);
        var ebirdToken = "', ebird_api_key, '";
        var stateCodeMap = ', js_state_codes_map, ';
        var isHotspotsLoaded = false;
        
        var secondaryControls = document.getElementById("secondaryControls");
        var secondaryStateSelect = document.getElementById("secondaryStateSelect");
        var secondaryHotspotBtn = document.getElementById("secondaryHotspotBtn");
        var secondaryHotspotsLayer = L.layerGroup().addTo(map);
        var isSecondaryLoaded = false;

        // Reveal the Primary Button when a state is selected
        document.getElementById("stateSelect").addEventListener("change", function(e) {
            hotspotsLayer.clearLayers();
            isHotspotsLoaded = false;
            hotspotBtn.innerText = "Show Hotspots";
            hotspotBtn.style.backgroundColor = "#00008B"; 
            
            if (e.target.value) {
                hotspotBtn.style.display = "inline-block";
            } else {
                hotspotBtn.style.display = "none";
            }
        });

        // Primary Hotspot API Call
        hotspotBtn.addEventListener("click", function() {
            if (isHotspotsLoaded) {
                hotspotsLayer.clearLayers();
                isHotspotsLoaded = false;
                hotspotBtn.innerText = "Show Hotspots";
                hotspotBtn.style.backgroundColor = "#f39c12"; 
                return;
            }

            var stateFile = document.getElementById("stateSelect").value;
            var regionCode = stateCodeMap[stateFile];

            if (!regionCode) {
                alert("Could not find the eBird region code for this state.");
                return;
            }

            var originalBtnText = hotspotBtn.innerText;
            hotspotBtn.innerText = "Loading...";

            fetch("https://api.ebird.org/v2/ref/hotspot/" + regionCode + "?fmt=json", {
                method: "GET",
                headers: { "X-eBirdApiToken": ebirdToken }
            })
            .then(response => {
                if (!response.ok) throw new Error("API Error: " + response.statusText);
                return response.json();
            })
            .then(data => {
                hotspotsLayer.clearLayers();

                data.forEach(function(hs) {
                    var popupHTML = `<div style="font-size: 14px;">
                        <b>Hotspot:</b> <a href="https://ebird.org/hotspot/${hs.locId}" target="_blank">${hs.locName}</a>
                    </div>`;

                    var marker = L.circleMarker([hs.lat, hs.lng], {
                        radius: 5,
                        color: "#c0392b",
                        fillColor: "#00008B",
                        fillOpacity: 0.8,
                        weight: 1
                    }).bindPopup(popupHTML);

                    hotspotsLayer.addLayer(marker);
                });

                isHotspotsLoaded = true;
                hotspotBtn.innerText = "Hide Hotspots";
                hotspotBtn.style.backgroundColor = "#e74c3c"; 

                // Reveal Secondary Controls
                secondaryControls.style.display = "flex";
                var currentState = document.getElementById("stateSelect").value;
                secondaryStateSelect.innerHTML = "";

                var defaultOpt = document.createElement("option");
                defaultOpt.value = "";
                defaultOpt.text = "-- Compare State --";
                secondaryStateSelect.appendChild(defaultOpt);

                Object.keys(stateCodeMap).forEach(function(st) {
                    if (st !== currentState) {
                        var opt = document.createElement("option");
                        opt.value = st;
                        opt.text = st.replace(/_/g, " "); 
                        secondaryStateSelect.appendChild(opt);
                    }
                });
            })
            .catch(err => {
                console.error("Hotspot Fetch Error:", err);
                alert("Failed to load hotspots. Ensure your API key is valid.");
                hotspotBtn.innerText = originalBtnText;
            });
        });

        // ----------------------------------------------------
        // SECONDARY HOTSPOT LOGIC
        // ----------------------------------------------------
        secondaryHotspotBtn.addEventListener("click", function() {
            if (isSecondaryLoaded) {
                secondaryHotspotsLayer.clearLayers();
                isSecondaryLoaded = false;
                secondaryHotspotBtn.innerText = "Add 2nd Layer";
                secondaryHotspotBtn.style.backgroundColor = "#8e44ad"; 
                return;
            }

            var st = secondaryStateSelect.value;
            if (!st) {
                alert("Please select a second state to compare.");
                return;
            }
            
            var regionCode = stateCodeMap[st];
            var originalBtnText = secondaryHotspotBtn.innerText;
            secondaryHotspotBtn.innerText = "Loading...";

            fetch("https://api.ebird.org/v2/ref/hotspot/" + regionCode + "?fmt=json", {
                method: "GET",
                headers: { "X-eBirdApiToken": ebirdToken }
            })
            .then(response => {
                if (!response.ok) throw new Error("API Error");
                return response.json();
            })
            .then(data => {
                secondaryHotspotsLayer.clearLayers();

                data.forEach(function(hs) {
                    var popupHTML = `<div style="font-size: 14px;">
                        <b>Secondary Hotspot:</b> <a href="https://ebird.org/hotspot/${hs.locId}" target="_blank">${hs.locName}</a>
                    </div>`;

                    var marker = L.circleMarker([hs.lat, hs.lng], {
                        radius: 5,
                        color: "#27ae60",
                        fillColor: "#2ecc71",
                        fillOpacity: 0.8,
                        weight: 1
                    }).bindPopup(popupHTML);

                    secondaryHotspotsLayer.addLayer(marker);
                });

                isSecondaryLoaded = true;
                secondaryHotspotBtn.innerText = "Hide 2nd Layer";
                secondaryHotspotBtn.style.backgroundColor = "#e74c3c";
            })
            .catch(err => {
                console.error("Secondary Fetch Error:", err);
                alert("Failed to load secondary hotspots.");
                secondaryHotspotBtn.innerText = originalBtnText;
            });
        });

        // Hide secondary controls safely if primary state is changed
        document.getElementById("stateSelect").addEventListener("change", function() {
            if (secondaryControls) {
                secondaryControls.style.display = "none";
            }
            if (secondaryHotspotsLayer) {
                secondaryHotspotsLayer.clearLayers();
            }
            isSecondaryLoaded = false;
            if (secondaryHotspotBtn) {
                secondaryHotspotBtn.innerText = "Add 2nd Layer";
                secondaryHotspotBtn.style.backgroundColor = "#8e44ad";
            }
        });

    </script>
</body>
</html>
')

# Write the final HTML file
writeLines(html_content, "index.html")
print("--- MAP PREPARATION COMPLETE ---")

#source("git_auto_commit.R")