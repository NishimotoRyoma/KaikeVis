#----------#
# packages #
#----------#

library(shiny)
library(igraph)
library(DT)
library(shinyjs)
library(colourpicker)

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
      h4("赤線にデータをドロップ"),
      div(
        id = "drop-area",
        ondragover = "f1(event)",
        ondrop = "f2(event)"
      )
    ),
    mainPanel(h4(textOutput("tableText")),DT::dataTableOutput("table")))
    
  ),
  
  tabPanel(
    "可視化",
    
    
    titlePanel("会計可視化"),
    fluidRow(column(3,
                    
                    
                    
                    dataTableOutput("smallTable")),
             column(1),
             column(8,
                    
                    #グラフの表示部分
                    mainPanel(
                      plotOutput("distPlot")
                      
                      
                    ))),
    
    fluidRow(column(3,
                    
                    numericInput(
                      "eCurve",
                      "矢印の曲率(0以上)",
                      value=0.1,
                      min=0,
                      step=0.1
                      
                      
                      
                    )
                   ),
             column(2),
             
             column(7,
                    
                    sliderInput(
                      "bins",
                      "",
                      min = 0,
                      max = 20,
                      value = 0
                    ),
                    actionButton("do1","残高表示/非表示")
                    )
             
             
             
             )
  ),
  
  tabPanel(
    "詳細設定",
    
    titlePanel("詳細設定"),
    
    fluidRow(column(4,
                    colourInput(
                      "vColor",
                      "ノードの色",
                      value="gold",
                      palette="square",
                      allowTransparent=TRUE
                      )
                    ),
             column(4,
                    colourInput(
                      "eColor",
                      "エッジの色",
                      value="black",
                      palette="square",
                      allowTransparent=TRUE
                    )
             )
      
    )
  )
  
))