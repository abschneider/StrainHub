
## StrainHub
## Required Package Setup

packages <- c("ade4",
              "adegenet",
              "ape",
              "castor",
              "colourpicker",
              "data.table",
              "dplyr",
              "DT",
              "ggplot2",
              "ggtree",
              "ggtree",
              "globe4r",
              "hashmap",
              "htmltools",
              "htmlwidgets",
              "igraph",
              "knitr",
              "leaflet",
              "magrittr",
              "markdown",
              "network",
              "plotly",
              "plyr",
              "randomcoloR",
              "rbokeh",
              "readr",
              "rhandsontable",
              "rmarkdown",
              "shiny",
              "shinyjqui",
              "shinythemes",
              "shinyWidgets",
              "stringr",
              "tibble",
              "treeio",
              "visNetwork",
              "webshot")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
