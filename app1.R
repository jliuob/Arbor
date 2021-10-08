library(shiny)
library(treeio)
library(ggtree)
library(ggplot2)
library(ggimage)
library(aplot)
library(bslib)
library(thematic)
devtools::install_github("YuLab-SMU/ggtree")
source('draw-functions.R')

aList = list()

mytheme <- bs_theme(bootswatch = "cerulean",
                    base_font = font_google("Barlow"))

ui <- fluidPage(
    # Application title
    titlePanel("Tree Data Visualization"),
    # themes
    theme = mytheme,
    
    fluidRow(
        column(
            4,
            fileInput("treefile", "Tree", buttonLabel = "Upload",
                      accept = ".nwk")
        ),
        
        column(
            4,
            fileInput(
                "hmfile",
                "Heatmap",
                buttonLabel = "Upload",
                accept = ".csv"
            )
        ),
        column(
            4,
            fileInput(
                "barfile",
                "Bar Plot",
                buttonLabel = "Upload",
                accept = ".csv",
            )
        )
    ),
    
    fluidRow(column(12, plotOutput("figure"))),
    
    fluidRow(column(width = 2, offset = 5, 
                    downloadButton("download", 'Download'))),
)


server <- function(input, output, session) {
    thematic::thematic_shiny()
    
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
