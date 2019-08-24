#----------#
# packages #
#----------#

library(shiny)
library(igraph)
library(DT)

#---------------#
# userInterface #
#---------------#
#userが利用する画面の規定

shinyUI(navbarPage(
  "会計可視化",
  tabPanel(
    "データの入力",
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css", type = "text/css"),
      tags$script(src = "drag.js")
    ),
    sidebarLayout(sidebarPanel(
      h3("赤線にデータをドロップ"),
      div(
        id = "drop-area",
        ondragover = "f1(event)",
        ondrop = "f2(event)"
      )
    ),
    mainPanel(DT::dataTableOutput("table")))
    
  ),
  
  tabPanel(
    "可視化",
    
    
    titlePanel("会計可視化"),
    fluidRow(column(4,
                    
                    
                    
                    dataTableOutput("smallTable")),
             column(8,
                    
                    #グラフの表示部分
                    mainPanel(
                      plotOutput("distPlot")
                      
                      
                    ))),
    
    fluidRow(column(4,
                    
                    sliderInput(
                      "bins",
                      "",
                      min = 0,
                      max = 20,
                      value = 0
                    )
    )
             )
  )
  
))