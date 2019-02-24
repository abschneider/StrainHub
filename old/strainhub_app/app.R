## Shiny Web Application for Transmission Graphs - StrainHub
library(shiny)

## Load other libraries
library(shinythemes)
library(ape)
library(castor)
library(visNetwork)
library(hashmap)
library(plyr)
library(network)
library(igraph)
library(data.table)
library(DT)
library(magrittr)
library(htmlwidgets)
source("strainhub_functions.R")

## Run the application 
# shinyApp(ui = "ui.R",
#          server = "server.R")

runApp("strainhub")

#runApp('shiny', display.mode = "showcase")

#############
## SETUP
#install.packages('rsconnect')
#rsconnect::deployApp()
## Install required packages
# install.packages(c("shiny",
#                    "shinythemes",
#                    "ape",
#                    "castor",
#                    "visNetwork",
#                    "hashmap",
#                    "plyr",
#                    "network",
#                    "igraph",
#                    "data.table",
#                    "DT",
#                    "magrittr",
#                    "htmlwidgets",
#                    "markdown"))