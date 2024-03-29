# *strainhub* - Phylogenetic Network Visualization in R

<h3 align = "right">Colby T. Ford, Ph.D. and Adriano De Bernardi Schneider, Ph.D.</h3>

<img align="right" src="https://raw.githubusercontent.com/colbyford/StrainHub/master/img/strainhub_hex_color.png" alt="StrainHub icon" width="200">

StrainHub works to generate a pathogen transmission network graph utilizing genomic data and calculate importance of network based on centrality metrics.

## How to Install
```r
remotes::install_github("colbyford/strainhub", subdir="pkg")
```

Note: If you're running R 4.x, you may have to install some dependencies manually as they are not yet available via CRAN for the newest version of R. Simply find the troublesome package on GitHub and run `remotes::install_github("<ACCOUNT>/<REPO-NAME>")` in your R console.

## Examples

### Using a Tree File
```r
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
         columnSelection = "Country")

```

### Using a Data.Frame
You'll need to prepare a tabular file (CSV, TSV, etc.) with "from" and "to" columns (and an optional "value" column to give weights to the edges).

| **from** | **to** | **value** |
|:--------:|:------:|:---------:|
| thingA   | thingB | 5         |
| thingB   | thingD | 10        |
| thingB   | thingC | 1         |

```r
## Load in StrainHub
library(strainhub)

## Read in treedata (from CSV into a dataframe)
treedata <- read.csv("../data/dataframe/dataframe.csv", stringsAsFactors = FALSE)

## Make the Transmission Network
graph <- makeTransNet(treedata,
                      centralityMetric = 6,
                      treeType = "dataframe")
```

## Customizing your Transmission Networks
StrainHub highly depends on the [vizNetwork](https://github.com/datastorm-open/visNetwork) package. For a full list of customizations options, see the vizNetwork documentation here: https://datastorm-open.github.io/visNetwork/

<img src="https://raw.githubusercontent.com/colbyford/StrainHub/master/img/chikv_example.png" alt="Example custom Chikungunya virus network">

```r
## Using the graph object from before...
library(visNetwork)

nodes <- graph$x$nodes %>%
  mutate(shape = "dot",
         color = "grey",
         font.size = 20)

## Define new edge colors
colors <- c("blue", "green", "purple")

edges <- graph$x$edges %>%
  mutate(arrows = "to",
         smooth = TRUE,
         color = colors,
         width = ifelse(value == 1, 1, 4),
         value = NULL)

## Add a legend
lnodes <- data.frame(label = c("Size:\nSource Hub Ratio"),
                     shape = c("dot"),
                     color = c("grey"))
                     
ledges <- data.frame(color = c("grey", "green", "blue", "purple"),
                     label = c("Size:\nTransitions",
                               "Senegal to\nCote de Ivoire",
                               "Cote de Ivoire to\nSenegal",
                               "Senegal to\nNigeria"),
                     arrows = c("to", "to", "to", "to"))

## Generate the custom network using `visNetwork`
visNetwork(nodes, edges) %>%
  visOptions(nodesIdSelection = list(enabled = TRUE)) %>%
  visLegend(addEdges = ledges,
            addNodes = lnodes,
            useGroups = FALSE)
```
