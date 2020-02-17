## StrainHub
# This is a Shiny web application.
# You can run the application by clicking the 'Run App' button above.
##

## Shiny Web Application for Transmission Graphs - StrainHub
library(shiny)

## Load libraries for BEAST Parser
library(adegenet)
library(ade4)
library(knitr)

## Load in libraries for map generation
#library(leaflet)
#library(geosphere)

## Load other libraries
library(shinythemes)
library(shinyWidgets)
library(readr)
#library(ape)
#library(castor)
#library(hashmap)
#library(plyr)
library(network)
library(visNetwork)
library(igraph)
#library(data.table)
library(DT)
library(rhandsontable)
#library(magrittr)
library(htmlwidgets)
library(htmltools)
library(rbokeh)
library(markdown)
library(rmarkdown)
#library(treeio)
library(ggplot2)
library(plotly)
library(shinyjqui)
library(shinycssloaders)
library(webshot)
source("strainhub_functions.R")

## Load in last
library(castor)
library(hashmap)
library(plyr)
library(dplyr)
library(data.table)
library(magrittr)
library(leaflet)
library(geosphere)
library(randomcoloR)
library(colourpicker)
library(globe4r)
#library(seqinr)
library(phangorn)
library(ape)
#library(ggtree) # Test
library("ggtree", pos = .Machine$integer.max)

# Define UI for application
ui <- tagList(
  #tags$head(includeScript("google-analytics.js")),
  # tags$head(tags$style(type="text/css", "html, body {width: 100%; height: 100%; overflow: hidden}")),
  navbarPage(
    windowTitle = "StrainHub",
    theme = shinytheme("flatly"),
    # theme = "uncc.css",
    title = "StrainHub",
    # footer = includeHTML("footer.html"),
    tabPanel("Home",
             # setBackgroundColor(
             #   color = "#2C3E50"
             # ),
             h2("Welcome to", align = "center"),
             h1("StrainHub", align = "center", style="font-size: 550%;"),
             sidebarPanel(style = "background-color: #FFFFFF", width = 3, position = "left"),
             mainPanel(
               style = "background-color: #FFFFFF",
               width = 6,
               br(),               
               p("StrainHub was initially designed as an open access web-based software to generate disease transmission networks and associated metrics from a combination of a phylogenetic tree and associated metadata. We are currently integrating Ybyrá, a project of software solutions for data analysis in phylogenetics in the StrainHub framework to transform it into a suite of tools for both phylogenetic and pathogen transmission network analyses.",
                 align ="center"),
               p("StrainHub is being developed as a collaborative project between researchers from the University of California San Diego and the University of North Carolina at Charlotte as an effort to create new the tools that will enable an in-depth analysis and data visualization of the spread of pathogens. ",
                 align ="center"),
               fluidRow(column(width = 6, img(src="ucsd-sm.jpg", width="200px", align="left")),
                        column(width = 6, img(src="uncc-cci.png", width="200px", align="right")))
               ),
             sidebarPanel(style = "background-color: #FFFFFF", width = 3, position = "right"),
             ),
    tabPanel("Network Visualizer",
             sidebarPanel(
               width = 3,
               selectInput("tree_input_type",
                           label = "1. Transmission Network Method",
                           choices = c("Parsimony", "BEAST Phylogeography", "Create Neighbor-Joining Tree")),
               uiOutput("inputtree"),
               uiOutput("bootstrapval"),
               div(uiOutput("metadatabuilderparams"), style="float:right"),
               br(),
               uiOutput("treeuiparams"),
               uiOutput("treerootswitch"),
               div(uiOutput("geodatabuilderparams"), style="float:right"),
               br(),
               uiOutput("geodataswitch"),

               actionButton("getlistbutton", label = "4a. List States", class = "btn-primary"),
               br(),
               br(),
               uiOutput("columnselection"),
               br(),
               selectInput("metricradio",
                            label ="5. Pick your Centrality Metric",
                            choices = list("Indegree" = 1,
                                           "Outdegree" = 2,
                                           "Betweenness" = 3,
                                           "Closeness" = 4,
                                           "Degree" = 5,
                                           "Source Hub Ratio" = 6),
                            selected = 1),
               # radioButtons("metricradio",
               #              label ="5. Pick your Centrality Metric",
               #              choices = list("Indegree" = 1,
               #                             "Outdegree" = 2,
               #                             "Betweenness" = 3,
               #                             "Closeness" = 4,
               #                             "Degree" = 5,
               #                             "Source Hub Ratio" = 6),
               #              selected = 1),
               br(),
               actionButton("plotbutton", label = "6. Generate Network", class = "btn-primary"),

               br(),
               includeHTML("footer.html"),
               p("v1.0.10", align = "right") ## Version
             ),
             mainPanel(
               width = 9,
               tabsetPanel(id = "toptabs",
                 tabPanel("Network Plot",
                          dropdownButton(
                            tags$h3("Network Settings"),
                            radioButtons("arrowedges",
                                         label = "Edge Style",
                                         choices = c("Arrows" = "TRUE", "Lines" = "FALSE"),
                                         selected = "TRUE"),
                            circle = FALSE,
                            status = "success",
                            icon = icon("gear"),
                            width = "300px",
                            tooltip = tooltipOptions(title = "Network Settings")
                          ) %>% div(style="float:left"),
                          p(" "),
                          dropdownButton(
                            tags$h3("Download Network"),
                            downloadButton("exportplothtml",
                                           "Export as HTML",
                                           style="color: white;"),
                            br(),
                            downloadButton("exportplotpng",
                                           "Export as PNG",
                                           style="color: white;"),
                            circle = FALSE,
                            status = "primary",
                            icon = icon("download"),
                            width = "300px",
                            tooltip = tooltipOptions(title = "Download Network As...")
                          ) %>% div(style="float:left; margin-left:5px;"),
                          br(),

                          jqui_resizable(visNetworkOutput("graphplot", height = "768px")) %>% withSpinner(color = "#2C3E50", type = 4)
                 ),
                 tabPanel("Tree Preview",
                          h4("Phylogeny Contents"),
                          #plotlyOutput("treepreview")
                          jqui_resizable(plotlyOutput("treepreview", height = "768px")) %>% withSpinner(color = "#2C3E50", type = 4)
                 ),
                 tabPanel("Map",
                          dropdownButton(
                            tags$h3("Map Settings"),
                            fluidRow(
                              # column(6, radioButtons("maparrowedges",
                              #                        label = "Line End Style",
                              #                        choices = c("Arrows" = "FALSE", "Lines" = "TRUE"),
                              #                        selected = "FALSE")),
                              column(6, switchInput("maparrowedges",
                                                    label = "Line End",
                                                    onLabel = "<i class=\"fas fa-minus\"></i>",
                                                    offLabel = "<i class=\"fas fa-arrows-alt-h\"></i>",
                                                    onStatus = "secondary", 
                                                    offStatus = "secondary")),
                              
                              # column(6, radioButtons("maparrowfill",
                              #                       label = "Arrow Style",
                              #                       choices = c("Filled" = "TRUE", "Unfilled" = "FALSE"),
                              #                       selected = "TRUE"))),
                              
                              column(6, switchInput("maparrowfill",
                                                    label = "Arrow Style",
                                                    onLabel = "<i class=\"fas fa-caret-left\"></i>",
                                                    offLabel = "<i class=\"fas fa-angle-right\"></i>",
                                                    value = TRUE,
                                                    onStatus = "secondary", 
                                                    offStatus = "secondary"))),
                            
                            fluidRow(
                              # column(6, radioButtons("mapshowlabels",
                              #                        label = "Location Labels",
                              #                        choices = c("Show" = "TRUE", "Hide" = "FALSE"),
                              #                        selected = "TRUE")),
                              column(6, switchInput("mapshowlabels",
                                                    label = "Location Labels",
                                                    onLabel = "On",
                                                    offLabel = "Off",
                                                    value = TRUE,
                                                    onStatus = "info", 
                                                    offStatus = "secondary")),
                              
                              # column(6, radioButtons("mapshowpoints",
                              #                        label = "Location Points",
                              #                        choices = c("Show" = "TRUE", "Hide" = "FALSE"),
                              #                        selected = "TRUE"))),
                              column(6, switchInput("mapshowpoints",
                                                    label = "Location Points",
                                                    onLabel = "On",
                                                    offLabel = "Off",
                                                    onStatus = "info", 
                                                    offStatus = "secondary"))),

                            colourInput("pointColorPicker", "Point Color", "#000000"),
                            sliderInput("pointOpacityPicker",
                                        label = "Point Opacity",
                                        min = 0, max = 1, value = 0.5),
                            colourInput("labelColorPicker", "Label Color", "#000000"),
                            selectInput("basemapselection",
                                         label = "Map Style",
                                         choices = list("Streets" = "Streets",
                                                        "Topographic" = "Topographic",
                                                        "NationalGeographic" = "NationalGeographic",
                                                        "Oceans" = "Oceans",
                                                        "Gray" = "Gray",
                                                        "DarkGray" = "DarkGray",
                                                        "Imagery" = "Imagery",
                                                        "ShadedRelief" = "ShadedRelief",
                                                        "Terrain" = "Terrain"),
                                         selected = "Gray"),
                            circle = FALSE,
                            status = "success",
                            icon = icon("gear"),
                            width = "300px",
                            tooltip = tooltipOptions(title = "Map Settings")
                          ) %>% div(style="float:left"),
                          p(" "),
                          dropdownButton(
                            tags$h3("Download Map"),
                            downloadButton("exportmaphtml",
                                           "Export as HTML",
                                           style="color: white;"),
                            br(),
                            downloadButton("exportmappng",
                                           "Export as PNG",
                                           style="color: white;"),
                            circle = FALSE,
                            status = "primary",
                            icon = icon("download"),
                            width = "300px",
                            tooltip = tooltipOptions(title = "Download Map As...")
                          ) %>% div(style="float:left; margin-left:5px;"),
                          # switchInput(
                          #   inputId = "mapswitch",
                          #   label = "<i class=\"fa fa-globe-americas\"></i>",
                          #   onLabel = "Globe",
                          #   onStatus = "success",
                          #   offLabel = "Map",
                          #   offStatus = "info"),
                          #div(downloadButton("downloadmap", "Download Map", class = "btn-outline-primary"), style="float:right;padding-top:1px;padding-bottom:1px;margin-top:20px"),
                          br(),
                          jqui_resizable(leafletOutput("mapoutput", height = "750px")) %>% withSpinner(color = "#2C3E50", type = 4)
                          #uiOutput("mapswitchoutput")
                          #jqui_resizable(globe4r::globeOutput("globeoutput", height = "768px")) %>% withSpinner(color = "#2C3E50", type = 4)
                 ),
                 tabPanel("Metrics",
                          div(downloadButton("downloadmetrics", "Download Output Metrics", class = "btn-outline-primary"), style="float:right"),
                          br(),
                          DT::dataTableOutput("metricstable")
                 )
               )
             ),
             icon = icon("chevron-right", lib = "glyphicon")
             ),
    tabPanel("About",
             sidebarPanel(style = "background-color: #FFFFFF", width = 2, position = "left"),
             mainPanel(width = 8,
                       includeMarkdown("https://github.com/abschneider/StrainHub/raw/master/ABOUT.md")
                       ),
             sidebarPanel(style = "background-color: #FFFFFF", width = 2, position = "right"),
             icon = icon("question"))#,
    # tabPanel(title="Standalone",
    #          icon = icon("github"))
    # tabPanel(title=HTML("<br></a></li><li><a href='https://github.com/abschneider/StrainHub' target='_blank'>Standalone"),
    #          icon = icon("github")),
    # tabPanel(title=HTML("tab3</a></li><li><a href='mailto:adebernardischneider@ucsd.edu' target='_blank'>Contact"),
    #          icon = icon("envelope"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ## Initialize metadata reactive object
  rv <- reactiveValues(metadata = data.frame(NULL),
                       metrics = data.frame(NULL))
                       #metrics = DT::datatable(NULL))
  
  output$inputtree <- renderUI({
    if (is.null(input$tree_input_type))
      return()
    
    switch(input$tree_input_type,
           "Parsimony" = fileInput('treefile',
                                   label = '2. Choose your Tree File',
                                   accept = c('text/newick', 'text/plain', '.phy', '.tre', '.tree', '.newick', '.nwk')),
           "BEAST Phylogeography" = fileInput('treefile',
                                              label = '2. Choose your Tree File',
                                              accept = c('text/newick', 'text/plain', '.phy', '.tre', '.tree', '.newick', '.nwk')),
           "Create Neighbor-Joining Tree" = fileInput('treefile',
                          label = '2a. Choose your Sequence File',
                          accept = c('text/fasta', 'text/plain', '.fasta', '.afa', '.fna', '.ffn'))
    )
  })
  
  output$bootstrapval <- renderUI({
    if (is.null(input$tree_input_type))
      return()
    
    switch(input$tree_input_type,
           "Create Neighbor-Joining Tree" = sliderInput("bootstrapvalue",
                                                        label = "2b. Bootstrap Value",
                                                        min = 1, max = 100, value = 25)
    )
  })
  
  
  ## Show/Hide Maps based on Tree Type Input
  # observe({
  #   req(input$tree_input_type)
  #   if (input$tree_input_type != "BEAST Phylogeography") {
  #     hideTab(inputId = "toptabs", target = "Map")
  #   }
  #   else showTab(inputId = "toptabs", target = "Map")
  # })
  
  ## Change UI Requirements based on Tree Type Input
  output$treeuiparams <- renderUI({
    if (is.null(input$tree_input_type))
      return()

    switch(input$tree_input_type,
           "Parsimony" = fileInput('csvfile',
                                   label = '3a. Choose your Metadata File',
                                   accept = c('text/csv', 'text/plain', '.csv', '.txt')),
           "BEAST Phylogeography" = sliderInput("threshold",
                                                label = "3a. State Probability Threshold",
                                                min = 0, max = 1, value = 0.9),
           "Create Neighbor-Joining Tree" = fileInput('csvfile',
                                                      label = '3a. Choose your Metadata File',
                                                      accept = c('text/csv', 'text/plain', '.csv', '.txt'))
           )
  })
  
  
  ## Metadata Editor
  output$metadatabuilderparams <- renderUI({
    if (is.null(input$tree_input_type))
      return()
    
    if (is.null(input$csvfile$datapath))
      return()
    
    switch(input$tree_input_type,
           "Parsimony" = actionButton("metadatabuilder",
                                      label = "Edit Metadata",
                                      icon = icon("wrench", lib = "font-awesome"),
                                      class = "btn-secondary",
                                      style = 'padding-top:1px;padding-bottom:1px;margin-top:20px'),
           
           "Create Neighbor-Joining Tree" = actionButton("metadatabuilder",
                                                         label = "Edit Metadata",
                                                         icon = icon("wrench", lib = "font-awesome"),
                                                         class = "btn-secondary",
                                                         style = 'padding-top:1px;padding-bottom:1px;margin-top:20px')
    )
  })
  
  output$editmeta <- renderRHandsontable({
    rhandsontable(rv$metadata, stretchH = "all") %>% 
      hot_table(highlightCol = TRUE,
                highlightRow = TRUE) %>% 
      hot_context_menu(allowRowEdit = TRUE,
                       allowColEdit = TRUE,
                       customOpts = list(
                         csv = list(name = "Download to CSV",
                                    ## from: http://jrowen.github.io/rhandsontable/#customizing
                                    callback = htmlwidgets::JS(
                                      "function (key, options) {
                                      var csv = csvString(this, sep=',', dec='.');

                                      var link = document.createElement('a');
                                      link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                                        encodeURIComponent(csv));
                                      link.setAttribute('download', 'data.csv');

                                      document.body.appendChild(link);
                                      link.click();
                                      document.body.removeChild(link);}"))))
    
  })

  # output$editmetaDT <- DT::renderDataTable(DT::datatable(metadata(), editable = TRUE))
  
  observeEvent(input$metadatabuilder, {
    showModal(modalDialog(
      title = "Metadata Builder",
      size = "l",
      easyClose = TRUE,
      footer = tagList(actionButton("metasave",
                              label = "Save Changes",
                              icon = icon("save", lib = "font-awesome"),
                              class = "btn-success"),
                 modalButton("Close")),
      rHandsontableOutput("editmeta")#,
      # dataTableOutput("editmetaDT")
      
    ))
  })
  
  ## Save Metadata Changes
  observeEvent(input$metasave, {
    rv$metadata <- isolate(as_tibble(hot_to_r(input$editmeta)))
  })
  

  ## Geodata Editor
  output$geodatabuilderparams <- renderUI({
    if (is.null(input$tree_input_type))
      return()
    
    if (is.null(input$geodatafile$datapath))
      return()
    
    switch(input$tree_input_type,
           "Parsimony" = actionButton("geodatabuilder",
                                      label = "Edit Geodata",
                                      icon = icon("wrench", lib = "font-awesome"),
                                      class = "btn-secondary",
                                      style = 'padding-top:1px;padding-bottom:1px;margin-top:20px'),
           
           "BEAST Phylogeography" = actionButton("geodatabuilder",
                                      label = "Edit Geodata",
                                      icon = icon("wrench", lib = "font-awesome"),
                                      class = "btn-secondary",
                                      style = 'padding-top:1px;padding-bottom:1px;margin-top:20px'),
           
           "Create Neighbor-Joining Tree" = actionButton("geodatabuilder",
                                                         label = "Edit Geodata",
                                                         icon = icon("wrench", lib = "font-awesome"),
                                                         class = "btn-secondary",
                                                         style = 'padding-top:1px;padding-bottom:1px;margin-top:20px')
    )
  })
  
  output$editgeo <- renderRHandsontable({
    rhandsontable(rv$geodata, stretchH = "all") %>% 
      hot_table(highlightCol = TRUE,
                highlightRow = TRUE) %>% 
      hot_context_menu(allowRowEdit = TRUE,
                       allowColEdit = TRUE,
                       customOpts = list(
                         csv = list(name = "Download to CSV",
                                    ## from: http://jrowen.github.io/rhandsontable/#customizing
                                    callback = htmlwidgets::JS(
                                      "function (key, options) {
                                      var csv = csvString(this, sep=',', dec='.');

                                      var link = document.createElement('a');
                                      link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                                        encodeURIComponent(csv));
                                      link.setAttribute('download', 'data.csv');

                                      document.body.appendChild(link);
                                      link.click();
                                      document.body.removeChild(link);}"))))
    
  })
  
  observeEvent(input$geodatabuilder, {
    showModal(modalDialog(
      title = "Geodata Builder",
      size = "l",
      easyClose = TRUE,
      footer = tagList(actionButton("geosave",
                                    label = "Save Changes",
                                    icon = icon("save", lib = "font-awesome"),
                                    class = "btn-success"),
                       modalButton("Close")),
      rHandsontableOutput("editgeo")
    ))
  })
  
  ## Save Geodata Changes
  observeEvent(input$geosave, {
    rv$geodata <- isolate(as_tibble(hot_to_r(input$editgeo)))
  })
  
  ## Show/Hide Tree Root Selection or Posterior Probability Threshold
  output$treerootswitch <- renderUI({
    if (is.null(input$tree_input_type))
      return()
    
    switch(input$tree_input_type,
           "BEAST Phylogeography" = sliderInput("posterior",
                                                label = "3b. Posterior Probability Threshold",
                                                min = 0, max = 1, value = 0.9),
           
           "Create Neighbor-Joining Tree" = textInput("rootselect",
                                                      label = "3b. Taxa ID for Rooting Tree",
                                                      value = "e.g. HM045815.1")
    )
  })
  
  ## Show/Hide Geodata File Input
  output$geodataswitch <- renderUI({
    if (is.null(input$tree_input_type))
      return()
    
    switch(input$tree_input_type,
           "Parsimony" = fileInput('geodatafile',
                                   label = '3b. Choose your Geodata File',
                                   accept = c('text/csv', 'text/plain', '.csv', '.txt')),
           
           "BEAST Phylogeography" = fileInput('geodatafile',
                                              label = '3c. Choose your Geodata File',
                                              accept = c('text/csv', 'text/plain', '.csv', '.txt')),
           
           "Create Neighbor-Joining Tree" = fileInput('geodatafile',
                                                      label = '3c. Choose your Geodata File',
                                                      accept = c('text/csv', 'text/plain', '.csv', '.txt'))
    )
  }) 
  
  options(shiny.usecairo = TRUE)
  ## List State Column Choices
  availablecolumns <- eventReactive(input$getlistbutton, {
    if(input$tree_input_type == "Parsimony"){
      validate(need(input$csvfile != "",  "\n3a. Please upload the accompanying metadata file."))
      availablecolumns <- listStates(metadata = rv$metadata,
                                     treeType = "parsimonious")
      
    } else if(input$tree_input_type == "BEAST Phylogeography"){
      validate(need(input$treefile != "", "\n2. Please upload a tree file."))
      availablecolumns <- listStates(treedata = treedata(),
                                     treeType = "bayesian")
      
    } else if(input$tree_input_type == "Create Neighbor-Joining Tree"){
      validate(need(input$csvfile != "",  "\n3a. Please upload the accompanying metadata file."))
      availablecolumns <- listStates(metadata = rv$metadata,
                                     treeType = "nj")
    }
  })
  
  output$columnselection <- renderUI({
    selectInput("columnselection", "4b. Choose your State", choices = availablecolumns()$`Column`)
  })
  

  ## Viz Settings
  output$settings <- renderUI({
    actionButton("settings",
                 label = "",
                 icon = icon("cog", lib = "font-awesome"),
                 class = "btn-secondary")
  })

  # observeEvent(input$settings, {
  #   showModal(modalDialog(
  #     title = "Settings",
  #     size = "s",
  #     easyClose = TRUE,
  #     radioButtons("arrowedges",
  #                  label = "Edge Style", 
  #                  choices = c("Arrows" = "TRUE", "Lines" = "FALSE"),
  #                  selected = "TRUE")
  #   ))
  # })
  
  ## Show Map or Globe
  # output$mapswitchoutput <- renderUI({
  #   switch(input$mapswitch,
  #          "Map" = jqui_resizable(leafletOutput("mapoutput", height = "768px")) %>% withSpinner(color = "#2C3E50", type = 4),
  #          "Globe" = jqui_resizable(globe4r::globeOutput("globeoutput", height = "768px")) %>% withSpinner(color = "#2C3E50", type = 4)
  #   )
  # })
  
  ## Load in tree data
  treedata <- eventReactive(input$treefile, {
    if(input$tree_input_type == "Parsimony"){
      ape::read.tree(input$treefile$datapath)
    }
    else if(input$tree_input_type == "BEAST Phylogeography"){
      treeio::read.beast(input$treefile$datapath)
    }
    else if(input$tree_input_type == "Create Neighbor-Joining Tree"){
      read.dna(input$treefile$datapath, format="fasta")
    }
  })
  
  ## Load in metadata
  # metadata <- eventReactive(input$csvfile, {
  #   readr::read_csv(input$csvfile$datapath, col_names = TRUE)
  # })
  observeEvent(input$csvfile, {
    rv$metadata <- readr::read_csv(input$csvfile$datapath, col_names = TRUE)
    })
  
  ## Load in geodata
  # geodata <- eventReactive(input$geodatafile, {
  #   readr::read_csv(input$geodatafile$datapath, col_names = TRUE)
  # })
  observeEvent(input$geodatafile, {
    rv$geodata <- readr::read_csv(input$geodatafile$datapath, col_names = TRUE)
  })
  
  
  ## Network Viz
  graph <- eventReactive(input$plotbutton, {
    if(input$tree_input_type == "Parsimony"){
      validate(
        need(input$treefile$datapath != "", "\n2. Please upload a tree file."),
        need(input$csvfile$datapath != "",  "\n3a. Please upload the accompanying metadata file."),
        need("Accession" %in% colnames(rv$metadata),  "\nWarning: `Accession` column not found in the metadata file. Maybe you need to rename your existing ID column?")
      )
      validate(
        need(input$columnselection != "",  "\n4a. List the states from your metadata and pick one to use."),
        need(input$columnselection %in% getUsableColumns(treedata = treedata(),
                                                         metadata = rv$metadata),
             "\n4b. Make sure to select a state column. (Must not contain all identical values.)")
      )
      
      graph <- makeTransNet(treedata = treedata(),
                            metadata = rv$metadata,
                            columnSelection = input$columnselection,
                            # columnSelection = input$columnselection_row_last_clicked,
                            centralityMetric = input$metricradio,
                            treeType = "parsimonious")
      # height = paste0(0.75*session$clientData$output_graph_width,"px")
      
    } else if(input$tree_input_type == "BEAST Phylogeography"){
      validate(
        need(input$treefile != "", "\n2. Please upload a tree file."),
        need(input$columnselection != "",  "\n4a. List the states from your phylogeography metadata and pick one to use.")
      )
      # validate(
      #   need(input$columnselection %in% getUsableColumns(treedata = treedata(),
      #                                                    metadata = rv$metadata),
      #        "\n4b. Make sure to select a state column. (Must not contain all identical values.)")
      # )
      
      graph <-  makeTransNet(treedata = treedata(),
                             columnSelection = input$columnselection,
                             # columnSelection = input$columnselection_row_last_clicked,
                             centralityMetric = input$metricradio,
                             threshold = input$threshold,
                             threshold2 = input$posterior,
                             treeType = "bayesian")
      # height = paste0(0.75*session$clientData$output_graph_width,"px")
      
    } else if(input$tree_input_type == "Create Neighbor-Joining Tree"){
      validate(
        need(input$treefile != "", "\n2. Please upload a fasta file."),
        need(input$csvfile != "",  "\n3a. Please upload the accompanying metadata file."),
        need("Accession" %in% colnames(rv$metadata),  "\nWarning: `Accession` column not found in the metadata file. Maybe you need to rename your existing ID column?")
      )
      validate(
        need(input$columnselection != "",  "\n4a. List the states from your metadata and pick one to use."),
        need(input$rootselect != "e.g. HM045815.1", "\n3b. Make sure you enter the Taxa ID of the desired tree root.")
        # need(input$columnselection %in% getUsableColumns(treedata = treedata(),
        #                                                  metadata = rv$metadata),
        #      "\n4b. Make sure to select a state column. (Must not contain all identical values.)")
      )
      
      graph <-  makeTransNet(treedata = treedata(),
                             metadata = rv$metadata,
                             columnSelection = input$columnselection,
                             # columnSelection = input$columnselection_row_last_clicked,
                             centralityMetric = input$metricradio,
                             threshold = input$threshold,
                             rootSelection = input$rootselect,
                             bootstrapValue = input$bootstrapvalue,
                             treeType = "nj")
      # height = paste0(0.75*session$clientData$output_graph_width,"px")
      
    }
    
    
    
  })
  
  # output$graphplot <- renderPlot({print(graph())})
  output$graphplot <- renderVisNetwork({print(graph() %>%
                                                visEdges(arrows = list(to = list(enabled = as.logical(input$arrowedges),
                                                                                 scaleFactor = 0.75)),
                                                         arrowStrikethrough = FALSE))})
  # output$graphplot <- renderVisNetwork({print(graph() %>% 
  #                                               visExport(type = "png",
  #                                                         background = "#00FFFFFF",
  #                                                         style = 'class = "btn-outline-primary"'))})
  
  
  ## Export Plot
  # output$exportplothtml <- downloadHandler(
  #   filename = function() {
  #     paste0(input$treefile, "_StrainHub_network.html")
  #   },
  #   content = function(file) {
  #     htmlwidgets::saveWidget(graph() %>%
  #                               visEdges(arrows = list(to = list(enabled = as.logical(input$arrowedges),
  #                                                                scaleFactor = 0.75))) %>% 
  #                               visInteraction(navigationButtons = FALSE) %>%
  #                               visOptions(width = 1920,
  #                                          height = 1080,
  #                                          autoResize = TRUE),
  #                             # title = paste0(input$treefile, "- Network Generated by StrainHub"),
  #                             file = file)
  #   }
  # )
  output$exportplothtml <- downloadHandler(
    filename = function() {
      paste0(input$treefile, "_StrainHub_network.html")
    },
    content = function(file) {
      m <- graph() %>%
        visEdges(arrows = list(to = list(enabled = as.logical(input$arrowedges),
                                         scaleFactor = 0.75))) %>% 
        visInteraction(navigationButtons = FALSE) %>%
        visOptions(width = 1920,
                   height = 1080,
                   autoResize = TRUE)
      
      htmlwidgets::saveWidget(m, "tempplot.html", selfcontained = TRUE)
      file.copy("tempplot.html", file)
    }
  )
  
  # output$exportplotpng <- downloadHandler(
  #   filename = function() {
  #     paste0(input$treefile, "_StrainHub_network.png")
  #   },
  #   content = function(file) {
  #     
  #     rbokeh::widget2png(graph() %>%
  #                         visInteraction(navigationButtons = FALSE) %>%
  #                          visEdges(arrows = list(to = list(enabled = as.logical(input$arrowedges),
  #                                                           scaleFactor = 0.75))) %>% 
  #                               visOptions(width = 2000,
  #                                          height = 2000,
  #                                          autoResize = TRUE),
  #                             # title = paste0(input$treefile, "- Network Generated by StrainHub"),
  #                             file = file)
  #   }
  # )
  
  output$exportplotpng <- downloadHandler(
    filename = function() {
      paste0(input$treefile, "_StrainHub_network.png")
    },
    content = function(file) {
      m <- graph() %>%
        visEdges(arrows = list(to = list(enabled = as.logical(input$arrowedges),
                                         scaleFactor = 0.75))) %>% 
        visInteraction(navigationButtons = FALSE) %>%
        visOptions(width = 1920,
                   height = 1080,
                   autoResize = TRUE)
      
      htmlwidgets::saveWidget(m, "tempplot.html", selfcontained = TRUE)
      
      ## HD: 
      webshot::webshot("tempplot.html", file = "tempplot.png", vwidth = 1920, vheight = 1080)
      
      ## 4K:
      #webshot::webshot("tempplot.html", file = "tempplot.png", vwidth = 3840, vheight = 2160, zoom = 2)
      
      file.copy("tempplot.png", file)
    }
  )
  
  ## Export Map
  
  # output$exportmappng <- downloadHandler(
  #   filename = function() {
  #     paste0(input$treefile, "_StrainHub_map.png")
  #   },
  #   content = function(file) {
  #     
  #     mapview::mapshot(make_map(graph(),
  #                               rv$geodata,
  #                               input$columnselection,
  #                               basemapLayer = input$basemapselection,
  #                               hideArrowHead = as.logical(input$maparrowedges),
  #                               arrowFilled = as.logical(input$maparrowfill),
  #                               showLabels = as.logical(input$mapshowlabels),
  #                               labelColor = input$labelColorPicker,
  #                               showPoints = as.logical(input$mapshowpoints),
  #                               pointColor = input$pointColorPicker,
  #                               pointOpacity = input$pointOpacityPicker),
  #                      file = file)
  #   }
  # )
  
  output$exportmappng <- downloadHandler(
    filename = function() {
      paste0(input$treefile, "_StrainHub_map.png")
    },
    content = function(file) {
      
      #m <- leafletProxy("mapoutput")
      m <- make_map(graph(),
                    rv$geodata,
                    input$columnselection,
                    basemapLayer = input$basemapselection,
                    hideArrowHead = as.logical(input$maparrowedges),
                    arrowFilled = as.logical(input$maparrowfill),
                    showLabels = as.logical(input$mapshowlabels),
                    labelColor = input$labelColorPicker,
                    showPoints = as.logical(input$mapshowpoints),
                    pointColor = input$pointColorPicker,
                    pointOpacity = input$pointOpacityPicker)
      
      htmlwidgets::saveWidget(m, "tempmap.html", selfcontained = TRUE)
      ## HD:
      webshot::webshot("tempmap.html", file = "tempmap.png", vwidth = 1920, vheight = 1080)
      
      ## 4K:
      # webshot::webshot("tempmap.html", file = "tempmap.png", vwidth = 3840, vheight = 2160, zoom = 2)

      file.copy("tempmap.png", file)
    }
  )
  
  output$exportmaphtml <- downloadHandler(
    filename = function() {
      paste0(input$treefile, "_StrainHub_map.html")
    },
    content = function(file) {
      
      #m <- leafletProxy("mapoutput")
      m <- make_map(graph(),
                    rv$geodata,
                    input$columnselection,
                    basemapLayer = input$basemapselection,
                    hideArrowHead = as.logical(input$maparrowedges),
                    arrowFilled = as.logical(input$maparrowfill),
                    showLabels = as.logical(input$mapshowlabels),
                    labelColor = input$labelColorPicker,
                    showPoints = as.logical(input$mapshowpoints),
                    pointColor = input$pointColorPicker,
                    pointOpacity = input$pointOpacityPicker)
      
      htmlwidgets::saveWidget(m, "tempmap.html", selfcontained = TRUE)
      
      file.copy("tempmap.html", file)
    }
  )
  
  ## Tree File Preview
  
  output$treepreview <- eventReactive(input$plotbutton, {
    output$treepreview <- renderPlotly({
      # df <- read.csv(input$treefile$datapath)
      if (input$tree_input_type == "Parsimony"){
        # treepreview <- ape::read.tree(input$treefile$datapath)
        treepreview <- treedata()
        #return(treepreview)
        #plot(treepreview)
        #ggtree(treepreview) + geom_tiplab()
        
        # md <- read_csv(input$csvfile$datapath)
        #input$columnselection_row_last_clicked
        #colorby <- availablecolumns %>%
        colorby <- input$columnselection
        
        import::from(ggtree, `%<+%`, ggtree)
        
        t1 <- ggtree(treepreview, ladderize = T) %<+% rv$metadata +
          geom_point(aes_string(color = colorby, size = 3)) +
          geom_text(aes(label = label),
                    hjust = 0,
                    #angle = -90,
                    position = position_nudge(x = 0.5)) +
          ggtitle(paste0("Phylogeny of `", input$treefile$name, "`"),
                  subtitle = "Generated by StrainHub") +
          scale_fill_brewer(palette="Spectral") +
          scale_x_continuous(expand = c(.1, .1)) # + scale_x_reverse() + coord_flip()
        
        plotly::ggplotly(t1, tooltip = c("label", "colour"))
        
      } else if(input$tree_input_type == "BEAST Phylogeography"){
        
        # treepreview <- treeio::read.beast(input$treefile$datapath)
        treepreview <- treedata()
        
        colorby <- input$columnselection
        
        # annotationdf <- lapply(treepreview$annotations, data.frame, stringsAsFactors = FALSE) %>% 
        #   dplyr::bind_rows() %>% 
        #   dplyr::select_(colorby) %>% 
        #   slice(1:length(treepreview$tip.label)) %>% 
        #   as_tibble()
        
        # annotationdf <- cbind(taxa = treepreview$tip.label, annotationdf)
        
        import::from(ggtree, `%<+%`, ggtree)
        
        t1 <- ggtree(treepreview, ladderize = T) %<+% treepreview@data +
          geom_point(aes_string(color = colorby, size = 3)) +
          geom_text(aes(label = label),
                    hjust = 0,
                    position = position_nudge(x = 0.5)) +
          ggtitle(paste0("Phylogeny of `", input$treefile$name, "`"),
                  subtitle = "Generated by StrainHub") +
          scale_fill_brewer(palette="Spectral") +
          scale_x_continuous(expand = c(.1, .1))
        
        plotly::ggplotly(t1, tooltip = c("label", "colour"))
        
      } else if(input$tree_input_type == "Create Neighbor-Joining Tree"){
        
        # treepreview <- make_nj_tree(filePath = input$treefile$datapath, accession = input$rootselect)
        treepreview <- NJ_build_collapse(dna = treedata(),
                                         accession = input$rootselect,
                                         bootstrapValue = input$bootstrapvalue)
        
        # md <- read_csv(input$csvfile$datapath)

        colorby <- input$columnselection
        
        import::from(ggtree, `%<+%`, ggtree)
        
        t1 <- ggtree(treepreview, ladderize = F) %<+% rv$metadata +
          geom_point(aes_string(color = colorby, size = 3)) +
          geom_text(aes(label = label),
                    hjust = 0,
                    position = position_nudge(x = 0.5)) +
          ggtitle(paste0("Phylogeny of `", input$treefile$name, "`"),
                  subtitle = "Generated by StrainHub") +
          scale_fill_brewer(palette="Spectral") +
          scale_x_continuous(expand = c(.1, .1))
        
        plotly::ggplotly(t1, tooltip = c("label", "colour"))
        
      }
    })
  })
  
  ## Map Output
  output$mapoutput <- eventReactive(input$plotbutton, {
    if (input$tree_input_type == "Parsimony"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file."),
        need(input$csvfile != "",  "\n3a. Please upload the accompanying metadata file."),
        need("Accession" %in% colnames(rv$metadata),  "\nWarning: `Accession` column not found in the metadata file. Maybe you need to rename your existing ID column?"), 
        need(input$geodatafile != "",  "\n3b. Please upload the accompanying geodata file."),
        need(input$columnselection %in% colnames(rv$geodata),
             "\n4b. The current selected state doesn't match any columns in the geodata file. Please select a different column.")
      )
      output$mapoutput <- leaflet::renderLeaflet({
        make_map(graph(),
                 rv$geodata,
                 input$columnselection,
                 basemapLayer = input$basemapselection,
                 hideArrowHead = as.logical(input$maparrowedges),
                 arrowFilled = as.logical(input$maparrowfill),
                 showLabels = as.logical(input$mapshowlabels),
                 labelColor = input$labelColorPicker,
                 showPoints = as.logical(input$mapshowpoints),
                 pointColor = input$pointColorPicker,
                 pointOpacity = input$pointOpacityPicker)
        })
      
    } else if(input$tree_input_type == "BEAST Phylogeography"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file."),
        need(input$geodatafile != "",  "\n3b. Please upload the accompanying geodata file.")
      )
      output$mapoutput <- leaflet::renderLeaflet({
        make_map(graph(),
                 rv$geodata,
                 input$columnselection,
                 basemapLayer = input$basemapselection,
                 hideArrowHead = as.logical(input$maparrowedges),
                 arrowFilled = as.logical(input$maparrowfill),
                 showLabels = as.logical(input$mapshowlabels),
                 labelColor = input$labelColorPicker,
                 showPoints = as.logical(input$mapshowpoints),
                 pointColor = input$pointColorPicker,
                 pointOpacity = input$pointOpacityPicker)
        })
      
    } else if(input$tree_input_type == "Create Neighbor-Joining Tree"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file."),
        need(input$geodatafile != "",  "\n3b. Please upload the accompanying geodata file.")
      )
      output$mapoutput <- leaflet::renderLeaflet({
        make_map(graph(),
                 rv$geodata,
                 input$columnselection,
                 basemapLayer = input$basemapselection,
                 hideArrowHead = as.logical(input$maparrowedges),
                 arrowFilled = as.logical(input$maparrowfill),
                 showLabels = as.logical(input$mapshowlabels),
                 labelColor = input$labelColorPicker,
                 showPoints = as.logical(input$mapshowpoints),
                 pointColor = input$pointColorPicker,
                 pointOpacity = input$pointOpacityPicker)
        })
    }
  })
  
  ## Map Settings
  # output$mapsettings <- renderUI({
  #   actionButton("mapsettings",
  #                label = "",
  #                icon = icon("cog", lib = "font-awesome"),
  #                class = "btn-secondary")
  # })
    
  ## Globe Output
  output$globeoutput <- eventReactive(input$plotbutton, {
    if (input$tree_input_type == "Parsimony"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file."),
        need(input$csvfile != "",  "\n3a. Please upload the accompanying metadata file."),
        need("Accession" %in% colnames(rv$metadata),  "\nWarning: `Accession` column not found in the metadata file. Maybe you need to rename your existing ID column?"), 
        need(input$geodatafile != "",  "\n3b. Please upload the accompanying geodata file."),
        need(input$columnselection %in% colnames(rv$geodata),
            "\n4b. The current selected state doesn't match any columns in the geodata file. Please select a different column.")
      )
      output$globeoutput <- render_globe({make_globe(graph(), rv$geodata, input$columnselection)})
      
    } else if(input$tree_input_type == "BEAST Phylogeography"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file."),
        need(input$geodatafile != "",  "\n3b. Please upload the accompanying geodata file.")
      )
      output$globeoutput <- render_globe({make_globe(graph(), rv$geodata, input$columnselection)})
      
    } else if(input$tree_input_type == "Create Neighbor-Joining Tree"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file."),
        need(input$geodatafile != "",  "\n3b. Please upload the accompanying geodata file.")
      )
      output$globeoutput <- render_globe({make_globe(graph(), rv$geodata, input$columnselection)})
    }
    })
  
  
  
  ## Metrics File Output
  # metrics <- eventReactive(input$plotbutton, {
  #   if (input$tree_input_type == "Parsimony"){
  #     validate(
  #       need(input$treefile != "", "\n1. Please upload a tree file."),
  #       need(input$csvfile != "",  "\n2a. Please upload the accompanying metadata file."),
  #       # need(input$columnSelection != "",  "\n3. List the columns and pick one to use.")
  #       if (exists("input$treefile") & exists("input$csvfile")){
  #         # need(!input$input$columnselection %in% getUsableColumns(treeFileName = input$treefile$datapath,
  #         #                                                         csvFileName = input$csvfile$datapath),
  #         #      "\n3. Please select a different column. This column has all identical values.")
  #       }
  #     )
  #   } else if(input$tree_input_type == "BEAST Phylogeography"){
  #     validate(
  #       need(input$treefile != "", "\n2. Please upload a tree file.")
  #     )
  #   }
  #   
  #   metrics <- DT::datatable(read.csv("StrainHub_metrics.csv"),
  #                            colnames = c("Metastates",
  #                                         "Degree",
  #                                         "Indegree",
  #                                         "Outdegree",
  #                                         "Betweeness",
  #                                         "Closeness",
  #                                         "Source Hub Ratio"),
  #                            options = list(autoWidth = TRUE,
  #                                           initComplete = JS(
  #                                             "function(settings, json) {",
  #                                             "$(this.api().table().header()).css({'background-color': '#2d3e4f', 'color': '#fff'});",
  #                                             "}")))
  # })
  # output$metricstable <- eventReactive(input$plotbutton, {
  #   output$metricstable <- DT::renderDataTable(rv$metrics)
  # })
  
  observeEvent(input$plotbutton, {
  # observe({
    if (input$tree_input_type == "Parsimony"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file."),
        need(input$csvfile != "",  "\n2a. Please upload the accompanying metadata file."),
        # need(input$columnSelection != "",  "\n3. List the columns and pick one to use.")
        if (exists("input$treefile") & exists("input$csvfile")){
          # need(!input$input$columnselection %in% getUsableColumns(treeFileName = input$treefile$datapath,
          #                                                         csvFileName = input$csvfile$datapath),
          #      "\n3. Please select a different column. This column has all identical values.")
        }
      )
    } else if(input$tree_input_type == "BEAST Phylogeography"){
      validate(
        need(input$treefile != "", "\n2. Please upload a tree file.")
      )
    }
    
    ## Clear out old metrics
    #rv$metrics <- DT::datatable(NULL)
    
    ## Read in new metrics
    # rv$metrics <- DT::datatable(read.csv("StrainHub_metrics.csv"),
    #                          colnames = c("Metastates",
    #                                       "Degree",
    #                                       "Indegree",
    #                                       "Outdegree",
    #                                       "Betweeness",
    #                                       "Closeness",
    #                                       "Source Hub Ratio"),
    #                          options = list(autoWidth = TRUE,
    #                                         initComplete = JS(
    #                                           "function(settings, json) {",
    #                                           "$(this.api().table().header()).css({'background-color': '#2d3e4f', 'color': '#fff'});",
    #                                           "}")))
    # 
    # output$metricstable <- DT::renderDataTable(rv$metrics, options = list(processing = FALSE))
    
    rv$metrics <- read.csv("StrainHub_metrics.csv")
    
    output$metricstable <- DT::renderDataTable(DT::datatable(rv$metrics),
                                               options = list(processing = FALSE,
                                                              autoWidth = TRUE,
                                                              colnames = c("Metastates",
                                                                           "Degree",
                                                                           "Indegree",
                                                                           "Outdegree",
                                                                           "Betweeness",
                                                                           "Closeness",
                                                                           "Source Hub Ratio"),
                                                              initComplete = JS(
                                                                "function(settings, json) {",
                                                                "$(this.api().table().header()).css({'background-color': '#2d3e4f', 'color': '#fff'});",
                                                                "}")))
    
  })
  
  proxy = dataTableProxy('metricstable')
  observeEvent(input$plotbutton, {
    #replaceData(proxy, rv$metrics, rownames = FALSE)
    reloadData(proxy, rv$metrics)
  })
  
  # output$metricstable <- renderTable({
  #   metrics <- read.csv(paste0(input$treefile$datapath,"_metrics.csv"))
  #   return(metrics)
  # })
  
  # Downloadable CSV of metrics
  output$downloadmetrics <- downloadHandler(
    filename = function() {
      paste0(input$treefile, "_StrainHub_metrics.csv")
    },
    content = function(file) {
      write.csv(read.csv("StrainHub_metrics.csv"), file, row.names = FALSE)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#############
## SETUP
#install.packages('rsconnect')
#rsconnect::deployApp()
## Install required packages
# source("install_packages.R")

