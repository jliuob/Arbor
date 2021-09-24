library(shiny)
library(treeio)
library(ggtree)
library(ggplot2)
library(ggimage)
library(aplot)
devtools::install_github("YuLab-SMU/ggtree")
source('draw-functions.R')

aList = list()

ui <- fluidPage(
    # Application title
    titlePanel("Tree Data Visualization"),
    
    fluidRow(
        column(
            4,
            fileInput("treefile", "Tree", buttonLabel = "Upload",
                      accept = ".nwk")
        ),
        # multiple=T: upload multiple files
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
                accept = ".csv"
            )
        )
    ),
    fluidRow(column(12, plotOutput("figure"))),
    # TODO download
    downloadButton("download", 'Download')
)

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
