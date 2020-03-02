<<<<<<< HEAD
=======

>>>>>>> beta
## StrainHub
## Required Package Setup

packages <- c("shiny",
              "treeio",
<<<<<<< HEAD
              "ggtree",
=======
>>>>>>> beta
              "ggplot2",
              "adegenet",
              "ade4",
              "knitr",
              "dplyr",
              "shinythemes",
<<<<<<< HEAD
=======
              "shinyWidgets",
>>>>>>> beta
              "readr",
              "ape",
              "castor",
              "visNetwork",
              "hashmap",
              "plyr",
              "network",
              "igraph",
              "data.table",
              "DT",
<<<<<<< HEAD
              "magrittr",
              "htmlwidgets",
=======
              "rhandsontable",
              "magrittr",
              "htmlwidgets",
              "globe4r",
              "randomcoloR",
              "colourpicker",
              "rbokeh",
>>>>>>> beta
              "markdown",
              "rmarkdown",
              "ggtree",
              "plotly",
<<<<<<< HEAD
=======
              "webshot",
>>>>>>> beta
              "shinyjqui")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}