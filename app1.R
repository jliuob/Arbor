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
            menuItem('Rearrangement', tabName = 'rearrangements', icon = icon('chart-bar')),
            menuItem("Plots", tabName = "plot", icon = icon("images")),
            menuItem('Contact', tabName = 'contact', icon = icon('envelope'))
        )
    ),
    
    dashboardBody(
        useShinyjs(),
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
                        column(width = 6,
                        box(title = 'Step 1: Tree', width = NULL,
                            status = "primary", solidHeader = TRUE,
                            fileInput("treefile",
                                      "Tree", 
                                      buttonLabel = "Upload",
                                      accept = ".nwk")
                        )),
                    
                        column(width = 6,
                        box(title = 'Step 2: Heatmap', width = NULL,
                            status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            fileInput("hmfile",
                                      "Heatmap",
                                      buttonLabel = "Upload",
                                      accept = ".csv",
                                      multiple = TRUE)
                        ),
                        
                        box(title = 'Step 2: Bar Plot', width = NULL,
                            status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            fileInput("barfile",
                                      "Bar Plot",
                                      buttonLabel = "Upload",
                                      accept = ".csv",
                                      multiple = TRUE)
                        )
                    )
            )),
            
            # tabItem(tabName = 'rearrangements',
            #         barfile<-as.data.frame('barfile'),
            #         datatable(
            #                 barfile, extensions = 'RowReorder',
            #                 options = list(rowReorder = TRUE, order = list(c(0 , 'asc')))
            #         )),

            tabItem(tabName = "plot",
                fluidRow(column(12, plotOutput("figure"))),
                fluidRow(column(width = 2, offset = 5, 
                                downloadButton("download", 'Download'))),
            ),
            
            tabItem(tabName = 'contact',
                    "Authors: Xiaowei Zhan, Jennifer Liu.\nGithub: https://github.com/jliuob/Tree.git"
                    )
    )
))

server <- function(input, output, session) {
    
    shinyjs::disable('hmfile')
    shinyjs::disable('barfile')
    observeEvent(input$treefile, {
        enable('hmfile')
        enable('barfile')
    })

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
            # input$barfile=NULL
        }
        print(aList)
    })

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
