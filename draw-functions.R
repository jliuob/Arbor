library(shiny)
library(treeio)
library(ggtree)
library(ggplot2)
library(ggimage)
library(aplot)
source('helper.R')

drawtree <- function (tr) {
  ggtree(tr, layout="roundrect", color="firebrick", size=2, ladderize=TRUE) +
    geom_tiplab(colour="firebrick", size=10, align=TRUE) + 
    geom_tippoint(color="orange", size=2) +
    geom_rootedge(color="firebrick", size=2)
}

drawhm <- function (hm){
  print(hm)
  ggplot(hm, aes(x=Category, y=Label)) + 
    geom_tile(aes(fill=Value)) + scale_fill_viridis_c()
}

drawbar <- function (bar){
  ggplot(bar, aes(Label, Value)) + 
    geom_col(aes(fill=Label)) + 
    geom_text(aes(label=Label, y= Value)) + 
    coord_flip() 
  
    ggplot(bar, aes(input$selectedcolx, input$selectedcoly)) + 
      geom_col(aes(fill=input$selectedcolx)) + 
      geom_text(aes(label=input$selectedcolx, y=input$selectedcoly)) + 
      coord_flip()
}
  ## TODO: need to deal with names with flexibility

draw1 <- function(data) {
  if (data$type=='tree') {
    drawtree(data$data)
  } else if (data$type=='heatmap') {
    drawhm(data$data)
  } else if (data$type=='barplot') {
    drawbar(data$data)
  }
}

draw <- function (data) {
  ## TODO: sort the list so the tree plot is always the first
  sort.list
  #new list
  th = theme(legend.position = "top")
  print(data)
  if (length(data) == 0) {
    g <- ggplot(data.frame(x = 0, y = 0, 
                           text = "No data yet")) + geom_text(aes(x = x, y= y, label = text))
  } else if (length(data)==1){
    g <- draw1(data[[1]])
  } else if (length(data)>1) {
    ## TODO: only keep one figure for tree plot
    # replace by the new tree
    ## TODO: make sure there is at least one tree plot
      g <- draw1(data[[1]])
      row.order = get_taxa_order(g) # from top to bottom
      for (i in 2:length(data)) {
        data[[i]]$data$Label = factor(data[[i]]$data$Label, level = rev(row.order))
        g2 <- draw1(data[[i]])
        g2 <- g2 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
        g <- g + g2 * th
      }
  }
  g
}

if (T) {
  testDraw <- function() {
    # setwd("~/Downloads/Jennifer/")
    d = list(
      list(type = 'tree', data = read.tree("tree.nwk")),
      list(type = 'heatmap', data = read.csv("heatmap.csv")),
      list(type = 'barplot', data = read.csv('bar.csv')))
    draw(d[1:3])
  }
  testDraw()
  
  
}
