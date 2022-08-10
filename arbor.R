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
library(purrr)
library(BiocManager)
options(repos = BiocManager::repositories())

#devtools::install_github("YuLab-SMU/ggtree")
source('draw-functions.R') 

select.header.labels = c("X-axis", "Y-axis")
select.header.values = c("x", "y")
example.file.name = "example.csv"

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

# Module UI function
csvFileUI <- function(id,
                      label = "Upload a CSV file") {
    # `NS(id)` returns a namespace function, which was save as `ns` and will
    # invoke later.
    ns <- NS(id)
    tagList(tabsetPanel(
        id = ns("csv.upload.panel"),
        type = "hidden",
        
        tabPanel(
            ns("upload.panel"),
            fileInput(ns("file"), label),
            actionLink(ns("load.example"), "Use example"),
            br(),
            downloadLink(ns("download.example"), "Download example"),
            #hr(),
            #actionButton(ns("next.button"), "Next")
        ),
        tabPanel(
            ns("option.panel"),
            h3("Graph options"),
            uiOutput(ns("list.for.select.header")),
            br(),
            h3("CSV file options"),
            checkboxInput(ns("heading"), "Has heading", value = TRUE),
            hr(),
            actionButton(ns("back.button"), "Back"),
            actionButton(ns("upload.button"), "Upload")
        )
    ))
}

# Server ------------------------------------------------------------------
# the reactive flow
# upload.file, csv read options -> d -> selectInput components -> v
#
# Module server function
csvFileServer <- function(id,
                          parent,
                          select.header.labels = c("X-axis", "Y-axis"),
                          select.header.values = c("x", "y"),
                          example.file.name = "example.csv",
                          upload.button = NULL) {
    moduleServer(id,
                 ## Below is the module function
                 function(input, output, session) {
                     ns <- session$ns
                     useExample <- reactiveVal(FALSE)
                     observeEvent(input$load.example, {
                         cat("useExample set to TRUE\n")
                         useExample(TRUE)
                         updateTabsetPanel(
                             session = parent,
                             inputId = ns("csv.upload.panel"),
                             selected = ns("option.panel")
                         )
                     })
                     n.upload.button.pressed <- reactiveVal(0)
                     
                     d <- reactive({
                         cat("d start\n")
                         if (useExample()) {
                             read.csv(example.file.name, header = input$heading)
                         } else {
                             req(input$file)
                             print(input$file)
                             read.csv(input$file$datapath, header = input$heading)
                         }
                     })
                     v <- reactive({
                         cat("v <- reactive()\n")
                         if (useExample()) {
                             cat("useExample = TRUE\n")
                             ret <- select.header.values
                         } else {
                             cat("useExample = FALSE\n")
                             ret <-
                                 map_chr(select.header.labels, ~ input[[(.x)]] %||% "")
                         }
                         cat("v = ")
                         print(ret)
                         cat("\n")
                         ret
                     })
                     observeEvent(input$file, {
                         ns <- session$ns
                         cat("useExample set to FALSE\n")
                         useExample(FALSE)
                         
                         updateTabsetPanel(
                             session = parent,
                             inputId = ns("csv.upload.panel"),
                             selected = ns("option.panel")
                         )
                         n = colnames(d())
                         map(
                             select.header.labels,
                             ~ updateSelectInput(
                                 session = parent,
                                 inputId = ns(.x),
                                 choices = n
                             )
                         )
                     })
                     observeEvent(input$back.button, {
                         # back button clicked
                         updateTabsetPanel(
                             session = parent,
                             inputId = ns("csv.upload.panel"),
                             selected = ns("upload.panel")
                         )
                     })
                     observeEvent(input$upload.button, {
                         n.upload.button.pressed(n.upload.button.pressed() + 1)
                         if (!is.null(upload.button)) {
                             upload.button$n <- input$upload.button
                         }
                         message("rv upload.button pressed", upload.button$n)
                         message("button inside module pressed")
                     })
                     output$list.for.select.header = renderUI({
                         # create selectInput components
                         cat('generate selectInput components\n')
                         header = colnames(d())
                         if (useExample()) {
                             cat("use example\n")
                             map2(
                                 select.header.labels,
                                 select.header.values,
                                 ~ selectInput(ns(.x), .x,
                                               choices = header,
                                               selected = .y)
                             )
                         } else {
                             cat("not use example\n")
                             map(select.header.labels,
                                 ~ selectInput(ns(.x), .x,
                                               choices = header))
                         }
                     })
                     
                     output$download.example <- downloadHandler(
                         filename = function() {
                             return(example.file.name)
                         },
                         content = function(file) {
                             file.copy(example.file.name, file)
                         }
                     )
                     
                     # Return the reactive that yields the data frame
                     ret = list(
                         data = d,
                         choices = select.header.labels,
                         values = v
                     )
                     print(ret)
                     return(ret)
                 })
}


ui <- dashboardPage(
    dashboardHeader(title = "Tree Data Visualization"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem('Introduction', tabName = 'introduction', icon = icon('book-open')),
            menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
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
                                   csvFileUI("hmfile"),
                                   hr(),
                                   tableOutput("table.hm")
                               )),
                               
                        column(width = 4,
                               box(title = 'Step 2: Bar Plot', width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   csvFileUI("barfile"),
                                   hr(),
                                   tableOutput("table.bar")
                               )
                        )),
                   
                        box(title = "Upload Status", width = NULL, 
                            status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            verbatimTextOutput("uploadStatusText")
                        )
                    
            ),
            
            
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
                                        title = "Advanced Options",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        numericInput("plot.width", "Plot width on screen", 500, min = 500)))),

            ),
            
            tabItem(tabName = 'contact',
                    "Authors: Xiaowei Zhan, Jennifer Liu.\nGithub: https://github.com/jliuob/Arbor"
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
    
    # Data upload panel --------------------------------------------------------
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
    
    # Rearrangement ------------------------------------------------------------
    upload.button.hm <- reactiveValues(n=NULL)
    upload.button.bar <- reactiveValues(n=NULL)
    
    ret.hm <- csvFileServer(
        "hmfile",
        session,
        select.header.labels = select.header.labels,
        select.header.values = select.header.values,
        example.file.name = example.file.name,
        upload.button = upload.button.hm
    )
    output$table.hm <- renderTable({
        head(ret.hm$data())
    })
    output$choices <- renderText({
        ret.hm$choices
    })
    output$values <- renderText({
        ret.hm$values()
    })
    observeEvent(upload.button.hm$n, {
        message("Upload button pressed!")
        print("obs event: heatmap.plot")
        if (!is.null(ret.hm$data())) {
          # print("ret.hm", ret.hm)
            v$l[[length(v$l) + 1]] <- list(type = 'heatmap',
                                           data = ret.hm$data(),
                                           x = ret.hm$values()[1],
                                           y = ret.hm$values()[2])
            reset("heatmap")
        }
    })
    
    ret.bar <- csvFileServer(
        "barfile",
        session,
        select.header.labels = select.header.labels,
        select.header.values = select.header.values,
        example.file.name = example.file.name,
        upload.button = upload.button.bar
    )
    output$table.bar <- renderTable({
        head(ret.bar$data())
    })
    output$choices <- renderText({
        ret.bar$choices
    })
    output$values <- renderText({
        ret.bar$values()
    })
    observeEvent(upload.button.bar$n, {
        message("Upload button pressed!")
        print("obs event: barplot")
        if (!is.null(ret.bar$data())) {
            v$l[[length(v$l) + 1]] <- list(type = 'barplot',
                                           data = ret.bar$data())
            reset("barplot")
        }
    })
    
    # Show figure -------------------------------------------------------------
    plotWidth <- reactive({
        if (is.null(input$plot.width)) {
            800 * max(1, length(v$l))
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
        size = 20
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
        size = 20
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
