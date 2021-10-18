library(shiny)
library(treeio)
library(ggtree)
library(ggplot2)
library(ggimage)
library(aplot)
library(bslib)
library(thematic)
library(shinydashboard)
library(shinyjs)
#devtools::install_github("YuLab-SMU/ggtree")
source('draw-functions.R')

aList = list()

ui <- dashboardPage(
    
    dashboardHeader(title = "Tree Data Visualization"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem('Introduction', tabName = 'introduction', icon = icon('book-open')),
            menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
            menuItem("Plots", tabName = "plot", icon = icon("th"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "introduction",
                    'Tree data visualization is a R shiny app for 
                    viewing tree data and its plots by simply uploading 
                    tree data and csv columns in Data Upload, and then 
                    the plots would be shown in Plots. Only .nwk is accepted 
                    for tree data upload. Only .csv is accepted for both 
                    heat map and bar plot data upload. Users are also 
                    welcomed to download the plots if needed.'
                    ),
            
            tabItem(tabName = "upload",
                    fluidRow(
                        column(width = 6,
                        box(title = 'Tree', width = NULL,
                            status = "primary", solidHeader = TRUE,
                            fileInput("treefile", 
                                         "", 
                                         buttonLabel = "Upload",
                                         accept = ".nwk")
                        )),
                    
                        column(width = 6,
                        box(title = 'Heatmap', width = NULL,
                            status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            shinyjs::useShinyjs(),
                            fileInput("hmfile",
                                            "",
                                            buttonLabel = "Upload",
                                            accept = ".csv")
                        ),
                        
                        box(title = 'Bar Plot', width = NULL,
                            status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            shinyjs::useShinyjs(),
                            fileInput("barfile",
                                           "",
                                           buttonLabel = "Upload",
                                           accept = ".csv",)
                        )
                    )
            )),
        
            tabItem(tabName = "plot",
                fluidRow(column(12, plotOutput("figure"))),
                fluidRow(column(width = 2, offset = 5, 
                                downloadButton("download", 'Download'))),
        )
    )
))

server <- function(input, output, session) {
    
    plotInput <- reactive({
        draw(g())
    })
    
    g <- reactive({
        # list
        cat("g is called,", length(aList), '\n')
        print(aList)
        if (!is.null(input$treefile)) {
            aList[[length(aList) + 1]] = list(type = 'tree',
                                              data = read.tree(input$treefile$datapath))
        }
        if (!is.null(input$hmfile)) {
            aList[[length(aList) + 1]] = list(type = 'heatmap',
                                              data = read.csv(input$hmfile$datapath))
        }
        if (!is.null(input$barfile)) {
            aList[[length(aList) + 1]] = list(type = 'barplot',
                                              data = read.csv(input$barfile$datapath))
        }
        print(aList)
    })
    
    shinyjs::disable('hmfile')
    shinyjs::disable('barfile')
    
    observeEvent(
        req (input$treefile),
        shinyjs::enable('hmfile'),
        shinyjs::enable('barfile'))
    
    output$figure <- renderPlot({
        draw(g())
    }, res = 96)
    
    output$download <- downloadHandler(
        filename = function() {
            paste0(input$treefile, ".png")
        },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "png")
        }
    )
}

shinyApp(ui, server)
