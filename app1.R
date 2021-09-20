library(shiny)
library(treeio)
library(ggtree)
library(ggplot2)
library(ggimage)
library(aplot)
library(shinyjs)
devtools::install_github("YuLab-SMU/ggtree")
source('draw-functions.R')

aList=list()

ui <- fluidPage(
  useShinyjs(),
  
  # Application title
  titlePanel("Tree Data Visualization"),
  
  fluidRow(
    column(4,
           fileInput("treefile", "Tree", buttonLabel = "Upload", 
                     accept = ".nwk")),  # multiple=T: upload multiple files
    column(4,
           disabled(
            fileInput("hmfile", "Heatmap", buttonLabel = "Upload", 
                       accept = ".csv"))),  
      column(4,
             disabled(
               fileInput("barfile", "Bar Plot", buttonLabel = "Upload", 
                                accept = ".csv")))
    ),
  fluidRow(
    column(12, plotOutput("figure"))
  )
)
  # TODO download
  # downloadButton("Download")
  # ...

server <- function(input, output, session) { 
  observeEvent(input$treefile,{
    enable('hmfile')
    enable('barfile')
  })
  
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

