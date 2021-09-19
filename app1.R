library(shiny)
library(treeio)
library(ggtree)
library(ggplot2)
library(ggimage)
library(aplot)
devtools::install_github("YuLab-SMU/ggtree")
source('draw-functions.R')

aList=list()

uploads <- tabsetPanel(
  id = "uploads",
  type = 'hidden',
  tabPanel("file",
           fileInput("hmfile", "Heatmap", buttonLabel = "Upload", 
                     accept = ".csv"),
           fileInput("barfile", "Bar Plot", buttonLabel = "Upload", 
                     accept = ".csv")
  )
)

ui <- fluidPage(
  # Application title
  titlePanel("Tree Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("treefile", "Tree", buttonLabel = "Upload", 
                accept = ".nwk")),  # multiple=T: upload multiple files
      uploads,
    ),
  mainPanel(
    plotOutput("tree")
  )
)
  # TODO download
  # downloadButton("Download")
  # ...

server <- function(input, output, session) { 
  
  output$tree <- renderPlot({
    bar <- reactiveValues(mydata=NULL)
    observeEvent(input$barfile, {
      bar$mydata<-read.csv(file=input$barfile$datapath)
      freezeReactiveValue(input, "selectedcolx")
      freezeReactiveValue(input, "selectedcoly")
      updateSelectInput(inputId = 'selectedcolx', 
                        label = 'Select x column', 
                        choices  = colnames(bar$mydata))
      updateSelectInput(inputId = 'selectedcoly', 
                        label = 'Select y column', 
                        choices  = colnames(bar$mydata))
    })

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
