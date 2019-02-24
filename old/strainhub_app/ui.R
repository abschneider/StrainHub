library(shiny)
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

ui <- tagList(
  # tags$head(tags$style(type="text/css", "html, body {width: 100%; height: 100%; overflow: hidden}")),
  navbarPage(
    theme = shinytheme("flatly"),
    title = "StrainHub",
    tabPanel("Network Visualizer",
             sidebarPanel(
               width = 3,
               fileInput('treefile',
                         label = '1. Choose your Tree File',
                         accept = c('text/newick', 'text/plain', '.phy', '.tre', '.tree', '.newick')),
               fileInput('csvfile',
                         label = '2. Choose your Metadata File',
                         accept = c('text/csv', 'text/plain', '.csv', '.txt')),
               actionButton("getlistbutton", label = "3. List States", class = "btn-primary"),
               br(),
               # selectInput("columnselection", "Column Selection:", 
               #             choices=c("NA")),
               dataTableOutput("columnselection"),
               br(),
               radioButtons("metricradio",
                            label ="4. Pick Centrality Metric",
                            choices = list("Indegree" = 1,
                                           "Outdegree" = 2,
                                           "Betweenness" = 3,
                                           "Closeness" = 4,
                                           "Degree" = 5,
                                           "Source Hub Ratio" = 6),
                            selected = 1),
               br(),
               actionButton("plotbutton", label = "5. Generate Network", class = "btn-primary")
             ),
             mainPanel(
               width = 9,
               tabsetPanel(
                 tabPanel("Network Plot",
                          # downloadButton("exportplot", "Export Plot"),
                          # visNetworkOutput("graphplot")
                          visNetworkOutput("graphplot", height = "600px")
                 ),
                 tabPanel("Tree Preview",
                          h4("Phylogeny Contents"),
                          plotOutput("treepreview")
                          ),
                 tabPanel("Metrics",
                          downloadButton("downloadmetrics", "Download Output Metrics"),
                          br(),
                          DT::dataTableOutput("metricstable")
                          )
               )
             )
    ),
    tabPanel("About",
             includeMarkdown("../ABOUT.md"))
  )
)