#----------#
# packages #
#----------#

library(shiny)
library(igraph)
library(DT)
library(shinyjs)
library(colourpicker)
library(rhandsontable)

#---------------#
# userInterface #
#---------------#
#userが利用する画面の規定

shinyUI(
navbarPage(
"会計可視化",
tabPanel("初期値の入力",
         fluidRow(column(3),
                  column(6,
                         align="center",
                         h4("ダブルクリックで金額の初期値(init)を変更できる")),
                  column(3)
         ),
         column(4),
         column(4,
                rHandsontableOutput("dynamicText")),
         column(4)
),
tabPanel(
"データの入力",
tags$head(
tags$link(
rel = "stylesheet",
href = "styles.css",
type = "text/css"
),
tags$script(src = "drag.js")
),

fluidRow(
  column(3,
h4("赤線にデータをドロップ"),
div(
id = "drop-area",
ondragover = "f1(event)",
ondrop = "f2(event)"
)

),
column(9,
h4(textOutput("tableText")),
DT::dataTableOutput("table")
)
)
),

tabPanel(
"可視化",
titlePanel("会計可視化"),

fluidRow(
column(3,
dataTableOutput("smallTable")
),

column(1),

column(8,
#グラフの表示部分
mainPanel(
plotOutput("distPlot")
)
)
),

fluidRow(
column(3,
numericInput(
"eCurve",
"矢印の曲率(0以上)",
value=0.1,
min=0,
step=0.1
)
),

column(6,
sliderInput(
"bins",
"",
min = 0,
max = 20,
value = 0
),
actionButton("do1","残高表示/非表示")

),

column(3)

)
),

tabPanel(
"詳細設定",
titlePanel("詳細設定"),
fluidRow(
column(4,
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
),

column(4,
colourInput(
"vfColor",
"ノードのフレームの色",
value="grey",
palette="square",
allowTransparent=TRUE
)
)
),

fluidRow(
column(4,
colourInput(
"vlColor",
"ノードのラベルの色",
value="black",
palette="square",
allowTransparent=TRUE
)
),

column(4,
colourInput(
"elColor",
"エッジのラベルの色",
value="blue",
palette="square",
allowTransparent=TRUE
)
)
),

fluidRow(
column(4,
numericInput(
"vSize",
"ノードのサイズ(0以上)",
value=20,
min=0,
step=1
)
),

column(4,
numericInput(
"upper",
"金額の上限(入力値以上のエッジを省略)",
value=9999999999,
min=0,
step=1
)
),

column(4,
numericInput(
"lower",
"金額の下限(入力値以下のエッジを省略)",
value=-0.00000001,
min=0,
step=1
)
)
),

fluidRow(
column(4,
selectInput(
"layoutIndex",
"ノードの配置",
choices=list(
"Star" = 1,
"Circle" = 2,
"Nicely" = 3,
"Random" = 4,
"Tree" = 5,
"Sphere" = 6
),
selected = 1
)
)
)
)
)
)
