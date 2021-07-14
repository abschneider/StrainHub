
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
              "geosphere",
              "ggplot2",
              "ggtree",
              "ggtree",
              "globe4r",
              "hashmap",
              "htmltools",
              "htmlwidgets",
              "igraph",
              "import",
              "knitr",
              "leaflet",
              "magrittr",
              "markdown",
              "network",
              "phangorn",
              "plotly",
              "plyr",
              "randomcoloR",
              "rbokeh",
              "readr",
              "rhandsontable",
              "rmarkdown",
              "shiny",
              "shinycssloaders",
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
