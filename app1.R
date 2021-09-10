library(shiny)
library(treeio)
library(ggtree)
library(ggplot2)
library(ggimage)
library(aplot)
devtools::install_github("YuLab-SMU/ggtree")
source('draw-functions.R')

aList=list()

ui <- fluidPage(
  # Application title
  titlePanel("Tree Data Visualization"),
  
  fluidRow(
    column(4,
           fileInput("treefile", "Tree", buttonLabel = "Upload", 
                     accept = ".nwk")),  # multiple=T: upload multiple files
    column(4,
           fileInput("hmfile", "Heatmap", buttonLabel = "Upload", 
                     accept = ".csv")),  
    column(4,
           fileInput("barfile", "Bar Plot", buttonLabel = "Upload", 
                     accept = ".csv")),
            selectInput('headers','Headers',choice=unique(barfile))),
  fluidRow(
    column(12, plotOutput("tree"))
  )
)

server <- function(input, output, session) { 
  output$tree <- renderPlot({

    g<-reactive({   # list
      if (!is.null(input$treefile)){
        aList[[length(aList)+1]]=list(type='tree', data=read.tree(input$treefile$datapath))
      }
      
      if (!is.null(input$hmfile)){
        aList[[length(aList)+1]]=list(type='heat map', data=read.csv(input$hmfile$datapath))
      }
      
      if (!is.null(input$barfile)){
        aList[[length(aList)+1]]=list(type='barplot', data=read.csv(input$hmfile$datapath))
      }
      
      print(aList)
    })
    
    draw(g())
    
  }, res = 96)
}

shinyApp(ui, server)


