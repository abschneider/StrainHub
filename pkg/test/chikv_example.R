## Load in StrainHub
library(strainhub)

## Read in tree, metadata, and geodata
treedata <- ape::read.tree("../../data/parsimonious/chikv/chikv_westernafrica.phy")
metadata <- readr::read_csv("../../data/parsimonious/chikv/chikv_westernafrica_metadata.csv", col_names = TRUE)
geodata <- readr::read_csv("../../data/parsimonious/chikv/chikv_geo.csv", col_names = TRUE)


## Check to See Which States are available by which to generate the network
listStates(treedata,
           metadata,
           treeType = "parsimonious")


## Make the Transmission Network
graph <- makeTransNet(treedata,
                      metadata,
                      columnSelection = "Country",
                      centralityMetric = 6,
                      treeType = "parsimonious")

print(graph)


## Make Leaflet/Swoopy map
make_map(graph,
         geodata = geodata,
         columnSelection = "Country",
         basemapLayer = "Gray",
         showLabels = TRUE)


## Custom Network
library(visNetwork)


nodes <- graph$x$nodes %>%
  mutate(shape = "dot",
         color = "grey",
         font.size = 20)

colors <- c("blue", "green", "purple")

edges <- graph$x$edges %>%
  mutate(arrows = "to",
         smooth = TRUE,
         color = colors,
         width = ifelse(value == 1, 1, 4),
         value = NULL)

lnodes <- data.frame(label = c("Size:\nSource Hub Ratio"),
                     shape = c("dot"),
                     color = c("grey"))

ledges <- data.frame(color = c("grey", "green", "blue", "purple"),
                     label = c("Size:\nTransitions", "Senegal to\nCote de Ivoire", "Cote de Ivoire to\nSenegal", "Senegal to\nNigeria"),
                     arrows = c("to", "to", "to", "to"))

visNetwork(nodes, edges) %>%
  visOptions(nodesIdSelection = list(enabled = TRUE)) %>%
  visLegend(addEdges = ledges,
            addNodes = lnodes,
            useGroups = FALSE)

## Custom StrainHub-Like Map

# basemapLayer = "Imagery"
basemapLayer = "Gray"
hideArrowHead = FALSE
arrowFilled = TRUE
showLabels = TRUE
labelColor = "#000000"
showPoints = FALSE
pointColor = "#000000"
pointOpacity = 0.5

locations <- graph$x$nodes %>%
  inner_join(geodata, by = c("label" = "Country")) %>%
  inner_join(metadata, by = c("label" = "Country"))

## Colors by Host (to)
graph_df <- graph$x$edges %>%
  select(-value) %>%
  dplyr::inner_join(locations, by = c("from" = "id")) %>%
  dplyr::inner_join(locations, by = c("to" = "id"), suffix = c(".from", ".to")) %>%
  filter(label.from != label.to) %>%
  mutate(path = paste0(label.from,"->",label.to),
         stroke = as.numeric(value.from)/5,
         ## Color mosquito hosts as blue, humans as red, and mice as orange.
         color = ifelse(Host.to %in% c("Aedes_luteocephalus",
                                       "Aedes_africanus",
                                       "Aedes_furcifer",
                                       "Aedes_aegypti",
                                       "Aedes_dalzieli"), "blue",
                        ifelse(Host.to == "Homo_sapiens", "red", "orange")))


esriPlugin <- htmlDependency("leaflet.esri", "1.0.3",
                             src = normalizePath("www"),
                             script = "esri-leaflet.js"
)

swoopyPlugin <- htmlDependency("leaflet-swoopy", "3.4.1",
                               src = normalizePath("www"),
                               script = "Leaflet.SwoopyArrow.min.js"
)

registerleafletPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

header <- "function(el, x) {"
## https://esri.github.io/esri-leaflet/api-reference/layers/basemap-layer.html
basemap <- paste0("L.esri.basemapLayer('",basemapLayer,"').addTo(this);")
swoopys <- ""
for (i in 1:nrow(graph_df)){
  fromLat <- graph_df$Latitude.from[i]
  fromLong <- graph_df$Longitude.from[i]
  toLat <- graph_df$Latitude.to[i]
  toLong <- graph_df$Longitude.to[i]
  color <- graph_df$color[i]
  # color <- graph_df$host_color[i]
  arrow <- if(arrowFilled){"true"}else{"false"}
  head <- if(hideArrowHead){"true"}else{"false"}
  swoopyIter <- paste0("L.swoopyArrow([",fromLat,",",fromLong,"], [",toLat,",",toLong,"], {color: '",color,"', factor: 1.2, weight: 2, hideArrowHead: ",head,", arrowFilled: ",arrow,"}).addTo(this);")
  #print(swoopyIter)
  swoopys <- paste0(swoopys, swoopyIter)
}

# fromLocs <- graph_df %>% select(label.from, Latitude.from, Longitude.from)
# colnames(fromLocs) <- c("location", "latitude", "longitude")
# toLocs <- graph_df %>% select(label.to, Latitude.to, Longitude.to)
# colnames(toLocs) <- c("location", "latitude", "longitude")
# allLocs <- fromLocs %>% rbind(toLocs) %>% unique()

# fromLocs <- graph_df %>% select(`state or province.from`, Latitude.from, Longitude.from)
# colnames(fromLocs) <- c("location", "latitude", "longitude")
# toLocs <- graph_df %>% select(`state or province.to`, Latitude.to, Longitude.to)
# colnames(toLocs) <- c("location", "latitude", "longitude")
# allLocs <- fromLocs %>% rbind(toLocs) %>% unique()
allLocs <- readr::read_csv("locations.csv", col_names = TRUE)

labels <- ""
if (showLabels){

  for (i in 1:nrow(allLocs)){
    fromLat <- allLocs$latitude[i]
    fromLong <- allLocs$longitude[i]
    toLat <- allLocs$latitude[i]
    toLong <- allLocs$longitude[i]
    loc <- allLocs$location[i]
    labelIter <- paste0("L.swoopyArrow([",fromLat,",",fromLong,"], [",toLat,",",toLong,"], {label: '",loc,"', labelColor: '",labelColor,"', labelFontSize: 18, iconAnchor: [-5, -5], iconSize: [20, 16], factor: 0.7, weight: 0}).addTo(this);")
    #print(labelIter)
    labels <- paste0(labels, labelIter)
  }
}

points <- ""

if (showPoints){
  for (i in 1:nrow(allLocs)){
    fromLat <- allLocs$latitude[i]
    fromLong <- allLocs$longitude[i]
    toLat <- allLocs$latitude[i]
    toLong <- allLocs$longitude[i]
    loc <- allLocs$location[i]
    pointsIter <- paste0("L.circleMarker([",fromLat,",",fromLong,"], {color: '",pointColor,"', fillColor: '",pointColor,"', fillOpacity: ",pointOpacity,", stroke: 0, }).addTo(this);")
    points <- paste0(points, pointsIter)
  }
}


footer <- "}"

renderText <- paste0(header, basemap, points, swoopys, labels, footer)
#renderText <- paste0(header, swoopys, labels, footer)

leafletmap <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addScaleBar(position = "bottomleft") %>%
  setView(mean(graph_df$Longitude.from), mean(graph_df$Latitude.from), zoom = 5) %>%
  registerleafletPlugin(esriPlugin) %>%
  registerleafletPlugin(swoopyPlugin) %>%
  onRender(renderText)


leafletmap

