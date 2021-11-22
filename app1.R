library(shiny)
library(treeio)
library(ggtree)
library(ggplot2)
library(ggimage)
library(aplot)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)

#devtools::install_github("YuLab-SMU/ggtree")
source('draw-functions.R')

debug <- FALSE
if (debug) {
    library(reactlog)
    reactlog_enable()
    # once app has closed, display reactlog from shiny
    shiny::reactlogShow()
} 
if (FALSE) {
    library(reactlog)
    reactlog_disable()
}

ui <- dashboardPage(
    dashboardHeader(title = "Tree Data Visualization"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem('Introduction', tabName = 'introduction', icon = icon('book-open')),
            menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
            menuItem('Rearrangement', tabName = 'rearrangements', icon = icon('chart-bar')),
            menuItem("Plots", tabName = "plot", icon = icon("images")),
            menuItem('Contact', tabName = 'contact', icon = icon('envelope'))
        ),
        collapsed = FALSE
    ),
    
    dashboardBody(
        useShinyjs(),
        tags$head(
            tags$style(HTML("
                      .shiny-html-output { overflow-x: scroll; }
                      " )
            )
        ),
        tabItems(
            tabItem(tabName = "introduction",
                    'Tree data visualization is a R shiny app for 
                    viewing tree data and its plots by simply uploading 
                    tree data and csv columns in Data Upload, and then 
                    the plots would be shown in Plots. Only .nwk is accepted 
                    for tree data upload. Only csv is accepted for both 
                    heat map and bar plot data upload. Users are also 
                    welcomed to download the plots if needed.'
            ),
            
            tabItem(tabName = "upload",
                    fluidRow(
                        column(width = 4,
                               box(title = 'Step 1: Tree', width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   fileInput("treefile",
                                             "Tree", 
                                             buttonLabel = "Upload",
                                             accept = ".nwk",
                                             multiple = FALSE)
                               )),
                        
                        column(width = 4,
                               box(title = 'Step 2: Heatmap', width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fileInput("hmfile",
                                             "Heatmap",
                                             buttonLabel = "Upload",
                                             accept = ".csv",
                                             multiple = TRUE)
                               )),
                               
                        column(width = 4,
                               box(title = 'Step 2: Bar Plot', width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fileInput("barfile",
                                             "Bar Plot",
                                             buttonLabel = "Upload",
                                             accept = ".csv",
                                             multiple = TRUE)
                               )
                        )),
                   
                        box(title = "Upload Status", width = NULL, 
                            status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            verbatimTextOutput("uploadStatusText")
                        )
                    
            ),
            
            tabItem(tabName = 'rearrangements',
                    fluidRow(column(12,
                                    # datatable(
                                    #         barfile, extensions = 'RowReorder',
                                    #         options = list(rowReorder = TRUE, order = list(c(0 , 'asc')))
                                    # )
                    ))),
            
            tabItem(tabName = "plot",
                    fluidRow(column(12, uiOutput("figure"))),
                    hr(),
                    fluidRow(
                        column(12,
                               box(id = "download.buttons",
                                   title = "Download",
                                   status = "primary", 
                                   solidHeader = TRUE,
                                   downloadButton("downloadpng", 'Download-png'),
                                   br(),
                                   downloadButton("downloadjpg", 'Download-jpg'),
                                   br(),
                                   downloadButton("downloadpdf", 'Download-pdf')))),
                    
                    fluidRow(column(12,
                                    box(id = "plot.advanced.option",
                                        title = "Advanced options",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        numericInput("plot.width", "Plot width on screen", 500, min = 500)))),
 
            ),
            
            tabItem(tabName = 'contact',
                    "Authors: Xiaowei Zhan, Jennifer Liu.\nGithub: https://github.com/jliuob/Tree.git"
            )
        )
    ))

server <- function(input, output, session) {
    ## this stores all data
    v <- reactiveValues(l = list())
    
    # ## UI customization
    #   shinyjs::disable('hmfile')
    #   shinyjs::disable('barfile')
    #   observeEvent(input$treefile, {
    #       enable('hmfile')
    #       enable('barfile')
    #   })
    
    
    # Data upload panel --------------------------------------------------------------
    output$uploadStatusText <- renderText({
        l <- v$l
        nTree <- length(Filter(function(x){x$type == "tree"}, l))
        nHeatmap <- length(Filter(function(x){x$type == "heatmap"}, l))
        nBar <- length(Filter(function(x){x$type == "barplot"}, l))
        
        sprintf("Uploaded %d tree plot(s), %d heatmap(s), and %d bar plot(s)",
                nTree, nHeatmap, nBar)
    })
    observeEvent(input$treefile, {
        print("obs event: tree")
        if (!is.null(input$treefile)) {
            v$l[[length(v$l) + 1]] <- list(type = 'tree',
                                           data = read.tree(input$treefile$datapath))
            reset("treefile")
            cat("input updated by adding a tree", length(v$l), "done\n")
        }
    })
    observeEvent(input$hmfile, {
        print("obs event: heatmap")
        if (!is.null(input$hmfile)) {
            v$l[[length(v$l) + 1]] <-  list(type = 'heatmap',
                                            data = read.csv(input$hmfile$datapath))
            reset("heatmap")
        }
    })
    observeEvent(input$barfile, {
        print("obs event: barplot")
        if (!is.null(input$barfile)) {
            v$l[[length(v$l) + 1]] <- list(type = 'barplot',
                                           data = read.csv(input$barfile$datapath))
            reset("barplot")
        }
    })    
    
    # Show figure -------------------------------------------------------------
    plotWidth <- reactive({
        if (is.null(input$plot.width)) {
            500 * max(1, length(v$l))
        } else {
            input$plot.width
        }
    })
    plotInput <- reactive({
        draw(v$l)
    })  
    output$figure <- renderUI({
        plotOutput("ggplot", width = plotWidth())
    })
    
    output$ggplot <- renderPlot({
        draw(v$l)
    }, res = 96)
    
    get.raster.size <- reactive({
        size = 8
        w.h.ratio = max(1, length(v$l))
        list(height = size / w.h.ratio, width = size)
    })
    output$downloadpng <- downloadHandler(
        filename = function() {
            paste0(input$treefile, ".png")
        },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "png",
                   width = get.raster.size()$width, 
                   height = get.raster.size()$height)
        }
    )
    
    output$downloadjpg <- downloadHandler(
        filename = function() {
            paste0(input$treefile, ".jpg")
        },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "jpg",
                   width = get.raster.size()$width, 
                   height = get.raster.size()$height)
        }
    )
    get.pdf.size <- reactive({
        size = 8
        w.h.ratio = max(1, length(v$l))
        list(height = size / w.h.ratio, width = size)
    })
    output$downloadpdf <- downloadHandler(
        filename = function() {
            paste0(input$treefile, ".pdf")
        },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "pdf", 
                   width = get.pdf.size()$width, 
                   height = get.pdf.size()$height)
        }
    )
}

shinyApp(ui, server)
