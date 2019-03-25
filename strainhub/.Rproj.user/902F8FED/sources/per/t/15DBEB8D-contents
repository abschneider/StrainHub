## StrainHub
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
##

## Shiny Web Application for Transmission Graphs - StrainHub
library(shiny)

## Load libraries for BEAST Parser
library(OutbreakTools)
library(ggplot2)
library(adegenet)
library(ade4)
library(knitr)
library(dplyr)

## Load other libraries
library(shinythemes)
library(readr)
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
library(markdown)
library(rmarkdown)
library(ggtree)
library(plotly)
source("strainhub_functions.R")

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
                           label = "1. Pick your Tree Type",
                           choices = c("Parsimonious", "Bayesian")),
               fileInput('treefile',
                         label = '2. Choose your Tree File',
                         accept = c('text/newick', 'text/plain', '.phy', '.tre', '.tree', '.newick', '.nwk')),
               uiOutput("treeuiparams"),
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
               includeHTML("footer.html")
             ),
             mainPanel(
               width = 9,
               tabsetPanel(
                 tabPanel("Network Plot",
                          div(downloadButton("exportplot", "Export Plot", class = "btn-outline-primary"), style="float:right"),
                          # visNetworkOutput("graphplot", height = "auto")
                          visNetworkOutput("graphplot", height = "750px")
                 ),
                 tabPanel("Tree Preview",
                          h4("Phylogeny Contents"),
                          plotlyOutput("treepreview")
                          #plotOutput("treepreview", height = "600px")
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
             #includeMarkdown("https://github.com/abschneider/transmission_graphs/raw/master/ABOUT.md"),
             includeMarkdown("https://github.com/supramap/transmission_graphs/raw/master/ABOUT.md"),
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
  
  output$treeuiparams <- renderUI({
    if (is.null(input$tree_input_type))
      return()

    switch(input$tree_input_type,
           "Parsimonious" = fileInput('csvfile',
                                     label = '3. Choose your Metadata File',
                                     accept = c('text/csv', 'text/plain', '.csv', '.txt')),
           "Bayesian" = sliderInput("threshold",
                                    label = "3. Choose your Probability Threshold",
                                    min = 0, max = 1, value = 0.9)
           )
  })
  
  #options(shiny.usecairo = TRUE)
  ## List State Column Choices
  availablecolumns <- eventReactive(input$getlistbutton, {
    if(input$tree_input_type == "Parsimonious"){
      availablecolumns <- listStates(csvFileName = input$csvfile$datapath, treeType = "parsimonious")
    } else if(input$tree_input_type == "Bayesian"){
      availablecolumns <- listStates(treeFileName = input$treefile$datapath, treeType = "bayesian")
    }
    
  })
  
  output$columnselection <- renderUI({
    selectInput("columnselection", "Choose your State", choices = availablecolumns()$`Column`)
  })
  
  # observe({
  #   updateSelectInput("columnselection", choices = availablecolumns())
  # })
  
  # output$columnselection <- DT::renderDataTable({DT::datatable(availablecolumns(),
  #                                                              rownames = FALSE,
  #                                                              colnames = c("Index",
  #                                                                           "State"),
  #                                                              options = list(dom = 't',
  #                                                                             autoWidth = TRUE,
  #                                                                             initComplete = JS(
  #                                                                               "function(settings, json) {",
  #                                                                               "$(this.api().table().header()).css({'background-color': '#2d3e4f', 'color': '#fff'});",
  #                                                                               "}")),
  #                                                              selection = 'single')})
  
  
  # observeEvent(availablecolumns(), {
  #   updateSelectInput(session = session,
  #                     inputId = "columnselection",
  #                     choices = availablecolumns()$Column)
  # })
  
  # output$settings <- renderUI({
  #   actionButton("settings", label = "", icon = icon("cog", lib = "font-awesome"), class = "btn-warning")
  # })
  # 
  # observeEvent(input$settings, {
  #   showModal(modalDialog(
  #     title = "Settings",
  #     size = "s",
  #     easyClose = TRUE,
  #     numericInput("plotheight", label = "Plot Height", value = 750, min = 500, max = 2000, step = 1)
  #   ))
  # })
  
  ## Network Viz
  graph <- eventReactive(input$plotbutton, {
    if(input$tree_input_type == "Parsimonious"){
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
      
      graph <-  makeTransNet(treeFileName = input$treefile$datapath,
                             csvFileName = input$csvfile$datapath,
                             columnSelection = input$columnselection,
                             # columnSelection = input$columnselection_row_last_clicked,
                             centralityMetric = input$metricradio,
                             treeType = "parsimonious")
      # height = paste0(0.75*session$clientData$output_graph_width,"px")
      
    } else if(input$tree_input_type == "Bayesian"){
      validate(
        need(input$treefile != "", "\n1. Please upload a tree file."),
        # need(input$columnSelection != "",  "\n3. List the columns and pick one to use.")
        if (exists("input$treefile") & exists("input$csvfile")){
          #need(!input$input$columnselection_row_last_clicked %in% getUsableColumns(treeFileName = input$treefile$datapath),
          #     "\n3. Please select a different column. This column has all identical values.")
        }
      )
      
      graph <-  makeTransNet(treeFileName = input$treefile$datapath,
                             columnSelection = input$columnselection,
                             # columnSelection = input$columnselection_row_last_clicked,
                             centralityMetric = input$metricradio,
                             threshold = input$threshold,
                             treeType = "bayesian")
      # height = paste0(0.75*session$clientData$output_graph_width,"px")
      
    }
    
    
    
  })
  
  # output$graphplot <- renderPlot({print(graph())})
  output$graphplot <- renderVisNetwork({print(graph())})
  
  
  ## Export Plot
  output$exportplot <- downloadHandler(
    filename =  function() {
      paste0(input$treefile, "_StrainHub_network.html")
    },
    # content is a function with argument file. content writes the plot to the device
    # content = function(file){
    #   visNetwork::visSave(graph(), file) ## Works
    #   #visNetwork::visExport(graph(), type = "pdf", name = file)
    #   # ggsave(file, plot = graph(), device = "pdf")
    #   # grDevices::pdf(file = file)
    #   # print(graph())
    #   # dev.off()
    # }
    
    content = function(file){
      # cairo_pdf(filename = file,
      #           width = 18,
      #           height = 10,
      #           pointsize = 12,
      #           family = "sans",
      #           bg = "transparent",
      #           antialias = "subpixel",
      #           fallback_resolution = 600)
      # 
      # #visExport(graph(), type = "pdf")
      # 
      # dev.off()
      visSave(graph(), file = file, background = "transparent")
      rmarkdown::pandoc_convert(file, to = "pdf")
    },
    contentType = "application/pdf"
    
  )
  
  
  ## Tree File Preview
  
  
  output$treepreview <- eventReactive(input$plotbutton, {
    output$treepreview <- renderPlotly({
      # df <- read.csv(input$treefile$datapath)
      if (input$tree_input_type == "Parsimonious"){
        treepreview <- ape::read.tree(input$treefile$datapath)
        #return(treepreview)
        #plot(treepreview)
        #ggtree(treepreview) + geom_tiplab()
        
        md <- read_csv(input$csvfile$datapath)
        #input$columnselection_row_last_clicked
        #colorby <- availablecolumns %>%
        colorby <- colnames(md)[input$columnselection] %>% 
          as.character()
        
        t1 <- ggtree(treepreview, ladderize = F) %<+% md +
          geom_point(aes_string(color = colorby, size = 3)) +
          geom_text(aes(label = label),
                    hjust = 0,
                    position = position_nudge(x = 0.5)) +
          ggtitle(paste0("Phylogeny of `", input$treefile$name, "`"),
                  subtitle = "Generated by StrainHub") +
          scale_fill_brewer(palette="Spectral") +
          scale_x_continuous(expand = c(.1, .1))
        
        plotly::ggplotly(t1, tooltip = c("label", "colour"))
        
      } else if(input$tree_input_type == "Bayesian"){
        
        treepreview <- OutbreakTools::read.annotated.nexus(input$treefile$datapath)
        #return(treepreview)
        #plot(treepreview)
        #ggtree(treepreview) + geom_tiplab()
        
        # md <- read_csv(input$csvfile$datapath)
        # #input$columnselection_row_last_clicked
        # #colorby <- availablecolumns %>%
        # colorby <- colnames(md)[input$columnselection] %>% 
        #   as.character()
        
        t1 <- ggtree(treepreview, ladderize = T) +
          geom_point(aes_string(size = 3)) +
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
  
  
  ## Metrics File Output
  metrics <- eventReactive(input$plotbutton, {
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
    metrics <- DT::datatable(read.csv(paste0(input$treefile$datapath,"_StrainHub_metrics.csv")),
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

