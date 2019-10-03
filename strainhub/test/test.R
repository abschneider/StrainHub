## NJ Example

#treedata <- read.dna("../data/neighbor_joining/chikv/ECSA_MASA_CHIKV.aln.fasta", format="fasta")
dna <- read.dna("../data/neighbor_joining/chikv/ECSA_MASA_CHIKV.aln.fasta", format="fasta")
accession <- "EF027139.1_India"
bootstrapValue <- 0.75

metadata <- readr::read_csv("../data/neighbor_joining/chikv/ECSA_MASA_metadata.csv", col_names = TRUE)
geodata <- readr::read_csv("../data/neighbor_joining/chikv/ECSA_MASA_geodata.csv", col_names = TRUE)


dna <- data("woodmouse")

# njtree <- make_nj_tree(filePath = "../data/chikv_westernafrica.aln.fasta", accession = "HM045815.1")
treedata <- NJ_build_collapse(dna = dna, accession = accession, bootstrapValue = bootstrapValue)

graph <- makeTransNet(treedata = dna,
                      metadata = metadata,
                      columnSelection = "Country",
                      centralityMetric = 1,
                      treeType = "nj",
                      rootSelection = accession)


## Baysian Example
treedata <- treeio::read.beast("../data/beast_phylogeography/batRABV.mcc.tree")
geodata <- readr::read_csv("../data/beast_phylogeography/batRABV_geo.csv", col_names = TRUE)

listStates(treedata,
           treeType = "bayesian")

graph <- makeTransNet(treedata,
                      columnSelection = "state",
                      centralityMetric = 6,
                      threshold = 0.9,
                      treeType = "bayesian")
print(graph)

## Parsimonsious Example
treedata <- ape::read.tree("../data/parsimonious/chikv/chikv_westernafrica.phy")
metadata <- readr::read_csv("../data/parsimonious/chikv/chikv_westernafrica_metadata.csv", col_names = TRUE)
geodata <- readr::read_csv("../data/parsimonious/chikv/chikv_geo.csv", col_names = TRUE)

listStates(treedata,
           metadata,
           treeType = "parsimonious")

graph <- makeTransNet(treedata,
                      metadata,
                      columnSelection = "Country",
                      centralityMetric = 6,
                      treeType = "parsimonious")



print(graph)

## graph to Dataframe

locations <- graph$x$nodes %>% inner_join(geodata, by = c("label" = colnames(geodata)[1]))

graph_df <- graph$x$edges %>%
  select(-value) %>% 
  dplyr::inner_join(locations, by = c("from" = "id")) %>% 
  dplyr::inner_join(locations, by = c("to" = "id"), suffix = c(".from", ".to")) %>% 
  mutate(path = paste0(label.from,"->",label.to),
         stroke = as.numeric(value.from)/10,
         color = RColorBrewer::brewer.pal(length(graph$x$edges), "Set1"))
  
## make globe using graph data
library(globe4r)

columnSelection <- "Site"

## Separate globe4r Steps

create_globe() %>% 
  globe_pov(graph_df$Latitude.from[1],graph_df$Longitude.from[1]) %>% 
  arcs_data(graph_df) %>% 
  arcs_start_lat("Latitude.from") %>% 
  arcs_start_lon("Longitude.from") %>% 
  arcs_end_lat("Latitude.to") %>% 
  arcs_end_lon("Longitude.to") %>%
  arcs_color("color") %>% 
  arcs_label("path") %>%
  #arcs_stroke("stroke") %>% 
  #arcs_on_hover(func = "function(data) {var globe = get_globe(data.path);}") %>% 
  #arcs_on_click(func = "function(data) {var globe = get_globe(data.path);}") %>% 
  labels_data(geodata) %>% 
  labels_lat("Latitude") %>% 
  labels_lon("Longitude") %>% 
  labels_text(columnSelection) %>% 
  labels_include_dot(include = TRUE) %>% 
  labels_dot_radius(radius = 0.3) %>% 
  #scale_labels_size() %>% 
  #scale_labels_radius() %>% 
  globe_background("#fff") %>% 
  show_atmosphere(TRUE) %>%
  show_graticules(TRUE) %>% 
  globe_img_url(url = image_url("blue-marble"))


make_globe(graph, geodata, columnSelection)

## ThreeJS Globe
library(threejs)

arcs <- graph_df %>% 
  mutate(origin_lat = Latitude.from,
         origin_long = Longitude.from,
         dest_lat = Latitude.to,
         dest_long = Longitude.to) %>% 
  select(origin_lat, origin_long, dest_lat, dest_long)

globejs(# img=image_url("blue-marble"),
        img = system.file("images/world.jpg",  package="threejs"),
        lat=graph_df$Latitude.from,
        long=graph_df$Longitude.from,
        arcs=arcs,
        arcsHeight=0.3,
        arcsLwd=2,
        arcsColor="#ffff00",
        bg = "white",
        arcsOpacity=0.15,
        atmosphere=TRUE)

## Save Graph

library(rbokeh)
widget2png(graph, "temp.png")

########################

#colorby <- treepreview$root.annotation[["state"]]

t1 <- ggtree(treepreview, ladderize = F) +
  geom_point(aes_string(size = 3)) +
  geom_text(aes(label = label),
            hjust = 0,
            position = position_nudge(x = 0.2)) +
  scale_fill_brewer(palette="Spectral") +
  scale_x_continuous(expand = c(.1, .1))

plotly::ggplotly(t1)

########################

treepreview <- ape::read.tree("../data/chikv_westernafrica.phy")
md <- read_csv("../data/chikv_westernafrica_metadata.csv")
#input$columnselection_row_last_clicked

colorby <- colnames(md)[2] %>%
  as.character()

# colorby <- md[1]
# colnames(colorby) <- "Legend"

t1 <- ggtree(treepreview, ladderize = F) %<+% md +
  geom_point(aes_string(color = colorby, size = 3)) +
  geom_text(aes(label = label),
            hjust = 0,
            position = position_nudge(x = 0.2)) +
  scale_fill_brewer(palette="Spectral") +
  scale_x_continuous(expand = c(.1, .1))

plotly::ggplotly(t1)

gr <- makeTransNet(treeFileName = "../data/chikv_westernafrica.phy",
                   csvFileName = "../data/chikv_westernafrica_metadata.csv",
                   columnSelection = 1,
                   centralityMetric = 2)

visSave(gr, file = "grfile.html", background = "transparent")
rmarkdown::pandoc_convert("grfile.html", to = "pdf")




###############################
## Map Tests

make_map(treeFileName = "../data/chikv_westernafrica.nwk.phy",
         csvFileName = "../data/chikv_geo.csv")
