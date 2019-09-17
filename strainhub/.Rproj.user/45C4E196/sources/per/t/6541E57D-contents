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
#library(dplyr)

## Load in libraries for NJ tree builder
#library(phangorn)
#library(seqinr)

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
library(rbokeh)
library(markdown)
library(rmarkdown)
#library(phytools)###
#library(ggtree)###
library(treeio)
library(ggplot2)
library(plotly)
library(shinyjqui)
library(shinycssloaders)
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
library(seqinr)
library(phangorn)
library(ape)


# Define UI for application
ui <- tagList(
  # tags$head(tags$style(type="text/css", "html, body {width: 100%; height: 100%; overflow: hidden}")),
  navbarPage(
    theme = shinytheme("flatly"),
    # theme = "uncc.css",
    title = "StrainHub",
    # footer = includeHTML("footer.html"),
    tabPanel("Network Visualizer",
             sidebarPanel(
               width = 3,
               selectInput("tree_input_type",
                           label = "1. Transmission Network Method",
                           choices = c("Parsimony", "BEAST Phylogeography", "Create Neighbor-Joining Tree")),
               uiOutput("inputtree"),
               uiOutput("treeuiparams"),
               div(uiOutput("metadatabuilderparams"), style="float:right"),
               # div(style="display: inline-block",
               #     uiOutput("treeuiparams"),
               #     uiOutput("metadatabuilderparams")),
               uiOutput("treerootswitch"),
               uiOutput("geodataswitch"),
               # fileInput('csvfile',
               #           label = '2. Choose your Metadata File',
               #           accept = c('text/csv', 'text/plain', '.csv', '.txt')),
               actionButton("getlistbutton", label = "4. List States", class = "btn-primary"),
               br(),
               # selectInput("columnselection", "Column Selection:", 
               #             choices=c("NA")),
               br(),
               #dataTableOutput("columnselection"),
               uiOutput("columnselection"),
               br(),
               radioButtons("metricradio",
                            label ="5. Pick Centrality Metric",
                            choices = list("Indegree" = 1,
                                           "Outdegree" = 2,
                                           "Betweenness" = 3,
                                           "Closeness" = 4,
                                           "Degree" = 5,
                                           "Source Hub Ratio" = 6),
                            selected = 1),
               br(),
               actionButton("plotbutton", label = "6. Generate Network", class = "btn-primary"),
               # div(uiOutput("settings"), style="float:right"),
               br(),
               includeHTML("footer.html"),
               p("v1.0.3", align = "right") ## Version
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
                                           "Export as HTML"),
                            downloadButton("exportplotpng",
                                           "Export as PNG"),
                            circle = FALSE,
                            status = "primary",
                            icon = icon("download"),
                            width = "300px",
                            tooltip = tooltipOptions(title = "Download Network As...")
                          ) %>% div(style="float:left"),
                          br(),
                          # visNetworkOutput("graphplot", height = "auto")
                          jqui_resizable(visNetworkOutput("graphplot", height = "700px")) %>% withSpinner(color = "#2C3E50", type = 4)
                 ),
                 tabPanel("Tree Preview",
                          h4("Phylogeny Contents"),
                          #plotlyOutput("treepreview")
                          jqui_resizable(plotlyOutput("treepreview", height = "700px")) %>% withSpinner(color = "#2C3E50", type = 4)
                 ),
                 tabPanel("Map",
                          div(downloadButton("downloadmap", "Download Map", class = "btn-outline-primary"), style="float:right"),
                          br(),
                          jqui_resizable(leafletOutput("mapoutput", height = "700px")) %>% withSpinner(color = "#2C3E50", type = 4)
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
             includeMarkdown("https://github.com/abschneider/StrainHub/raw/master/ABOUT.md"),
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
  rv <- reactiveValues(metadata = data.frame(NULL))
  
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
                          label = '2. Choose your Sequence File',
                          accept = c('text/fasta', 'text/plain', '.fasta', '.afa', '.fna', '.ffn'))
    )
  })
  
  ## Show/Hide Maps based on Tree Type Input
  observe({
    req(input$tree_input_type)
    if (input$tree_input_type != "BEAST Phylogeography") {
      hideTab(inputId = "toptabs", target = "Map")
    }
    else showTab(inputId = "toptabs", target = "Map")
  })
  
  ## Change UI Requirements based on Tree Type Input
  output$treeuiparams <- renderUI({
    if (is.null(input$tree_input_type))
      return()

    switch(input$tree_input_type,
           "Parsimony" = fileInput('csvfile',
                                   label = '3. Choose your Metadata File',
                                   accept = c('text/csv', 'text/plain', '.csv', '.txt')),
           "BEAST Phylogeography" = sliderInput("threshold",
                                                label = "3. Probability Threshold",
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
    
    switch(input$tree_input_type,
           "Parsimony" = actionButton("metadatabuilder",
                                      label = "Edit Metadata",
                                      icon = icon("wrench", lib = "font-awesome"),
                                      class = "btn-secondary"),
           
           "Create Neighbor-Joining Tree" = actionButton("metadatabuilder",
                                                         label = "Edit Metadata",
                                                         icon = icon("wrench", lib = "font-awesome"),
                                                         class = "btn-secondary")
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
  

  ## Show/Hide Tree Root Selection
  output$treerootswitch <- renderUI({
    if (is.null(input$tree_input_type))
      return()
    
    switch(input$tree_input_type,
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
           "Create Neighbor-Joining Tree" = fileInput('geodatafile',
                                                      label = '3c. Choose your Geodata File',
                                                      accept = c('text/csv', 'text/plain', '.csv', '.txt'))
    )
  })
  
  options(shiny.usecairo = TRUE)
  ## List State Column Choices
  availablecolumns <- eventReactive(input$getlistbutton, {
    if(input$tree_input_type == "Parsimony"){
      availablecolumns <- listStates(metadata = rv$metadata,
                                     treeType = "parsimonious")
      
    } else if(input$tree_input_type == "BEAST Phylogeography"){
      availablecolumns <- listStates(treedata = treedata(),
                                     treeType = "bayesian")
      
    } else if(input$tree_input_type == "Create Neighbor-Joining Tree"){
      availablecolumns <- listStates(metadata = rv$metadata,
                                     treeType = "nj")
    }
  })
  
  output$columnselection <- renderUI({
    selectInput("columnselection", "Choose your State", choices = availablecolumns()$`Column`)
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
  geodata <- eventReactive(input$geodatafile, {
    readr::read_csv(input$geodatafile$datapath, col_names = TRUE)
  })
  
  
  ## Network Viz
  graph <- eventReactive(input$plotbutton, {
    if(input$tree_input_type == "Parsimony"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file."),
        need(input$csvfile != "",  "\n2. Please upload the accompanying metadata file."),
        # need(input$columnSelection != "",  "\n3. List the columns and pick one to use.")
        if (exists("input$treefile") & exists("input$csvfile")){
          need(!input$input$columnselection %in% getUsableColumns(treeFileName = input$treefile$datapath,
                                                                  csvFileName = input$csvfile$datapath),
               "\n3. Please select a different column. This column has all identical values.")
        }
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
        need(input$treefile != "", "\n1. Please upload a tree file."),
        # need(input$columnSelection != "",  "\n3. List the columns and pick one to use.")
        if (exists("input$treefile") & exists("input$csvfile")){
          #need(!input$input$columnselection_row_last_clicked %in% getUsableColumns(treeFileName = input$treefile$datapath),
          #     "\n3. Please select a different column. This column has all identical values.")
        }
      )
      
      graph <-  makeTransNet(treedata = treedata(),
                             columnSelection = input$columnselection,
                             # columnSelection = input$columnselection_row_last_clicked,
                             centralityMetric = input$metricradio,
                             threshold = input$threshold,
                             treeType = "bayesian")
      # height = paste0(0.75*session$clientData$output_graph_width,"px")
      
    } else if(input$tree_input_type == "Create Neighbor-Joining Tree"){
      validate(
        need(input$treefile != "", "\n1. Please upload a fasta file."),
        # need(input$columnSelection != "",  "\n3. List the columns and pick one to use.")
        if (exists("input$treefile") & exists("input$csvfile")){
          #need(!input$input$columnselection_row_last_clicked %in% getUsableColumns(treeFileName = input$treefile$datapath),
          #     "\n3. Please select a different column. This column has all identical values.")
        }
      )
      
      graph <-  makeTransNet(treedata = treedata(),
                             metadata = rv$metadata,
                             columnSelection = input$columnselection,
                             # columnSelection = input$columnselection_row_last_clicked,
                             centralityMetric = input$metricradio,
                             threshold = input$threshold,
                             rootSelection = input$rootselect,
                             treeType = "nj")
      # height = paste0(0.75*session$clientData$output_graph_width,"px")
      
    }
    
    
    
  })
  
  # output$graphplot <- renderPlot({print(graph())})
  output$graphplot <- renderVisNetwork({print(graph() %>%
                                                visEdges(arrows = list(to = list(enabled = as.logical(input$arrowedges),
                                                                                 scaleFactor = 0.75))))})
  # output$graphplot <- renderVisNetwork({print(graph() %>% 
  #                                               visExport(type = "png",
  #                                                         background = "#00FFFFFF",
  #                                                         style = 'class = "btn-outline-primary"'))})
  
  
  ## Export Plot
  output$exportplothtml <- downloadHandler(
    filename = function() {
      paste0(input$treefile, "_StrainHub_network.html")
    },
    content = function(file) {
      htmlwidgets::saveWidget(graph() %>%
                                visEdges(arrows = list(to = list(enabled = as.logical(input$arrowedges),
                                                                 scaleFactor = 0.75))) %>% 
                                visInteraction(navigationButtons = FALSE) %>%
                                visOptions(width = 1920,
                                           height = 1080,
                                           autoResize = TRUE),
                              # title = paste0(input$treefile, "- Network Generated by StrainHub"),
                              file = file)
    }
  )
  
  output$exportplotpng <- downloadHandler(
    filename = function() {
      paste0(input$treefile, "_StrainHub_network.png")
    },
    content = function(file) {
      
      rbokeh::widget2png(graph() %>%
                          visInteraction(navigationButtons = FALSE) %>%
                           visEdges(arrows = list(to = list(enabled = as.logical(input$arrowedges),
                                                            scaleFactor = 0.75))) %>% 
                                visOptions(width = 2000,
                                           height = 2000,
                                           autoResize = TRUE),
                              # title = paste0(input$treefile, "- Network Generated by StrainHub"),
                              file = file)
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
                                         bootstrapValue = 80)
        
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
    if (input$tree_input_type == "Create Neighbor-Joining Tree"){
      rootedTree <- NJ_build_collapse(dna = treedata(),
                                      accession = input$rootselect,
                                      bootstrapValue = 80)
      
      parsedInfo <- parse_metaandtree(treePath = rootedTree,
                                      metadata = rv$metadata)
                        
      
      Edge_list <- parsimony_ancestral_reconstruction(accessioncharacter = parsedInfo$accessioncharacter,
                                                      country = parsedInfo$country,
                                                      characterlabels = parsedInfo$characterlabels,
                                                      rootedTree = rootedTree)
      
      output$mapoutput <- renderLeaflet({
        make_nj_map(geodata = geodata(),
                    transmissionpath = Edge_list,
                    linecolor = "red",
                    circlecolor = "grey")
      })
    } else {
      # validate(
      #   need(input$treefile != "", "\n1. Please upload a tree file."),
      #   need(input$csvfile != "",  "\n2. Please upload the accompanying metadata file."),
      # ),
      output$mapoutput <- renderLeaflet({
        make_map(treefile = treedata(),
                 metadata = rv$metadata)
      })
    }
    
  })
  
  
  ## Metrics File Output
  metrics <- eventReactive(input$plotbutton, {
    if (input$tree_input_type == "Parsimony"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file."),
        need(input$csvfile != "",  "\n2. Please upload the accompanying metadata file."),
        # need(input$columnSelection != "",  "\n3. List the columns and pick one to use.")
        if (exists("input$treefile") & exists("input$csvfile")){
          # need(!input$input$columnselection %in% getUsableColumns(treeFileName = input$treefile$datapath,
          #                                                         csvFileName = input$csvfile$datapath),
          #      "\n3. Please select a different column. This column has all identical values.")
        }
      )
    } else if(input$tree_input_type == "BEAST Phylogeography"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file.")
      )
    }
    
    metrics <- DT::datatable(read.csv("StrainHub_metrics.csv"),
                             colnames = c("Metastates",
                                          "Degree",
                                          "Indegree",
                                          "Outdegree",
                                          "Betweeness",
                                          "Closeness",
                                          "Source Hub Ratio"),
                             options = list(autoWidth = TRUE,
                                            initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#2d3e4f', 'color': '#fff'});",
                                              "}")))
    
  })
  
  output$metricstable <- eventReactive(input$plotbutton, {
    output$metricstable <- DT::renderDataTable({metrics()})
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
      write.csv(read.csv(paste0(input$treefile$datapath,"_StrainHub_metrics.csv")), file, row.names = FALSE)
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

