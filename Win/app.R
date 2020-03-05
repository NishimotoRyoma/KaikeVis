#----------#
# packages #
#----------#

library(shiny)
library(igraph)
library(DT)
library(shinyjs)
library(colourpicker)
library(rhandsontable)


#-----------#
# webServer #
#-----------#
#server内の振る舞いに関するコード
#shinyServer(){...}において、Server内の動きを規定する

server <-shinyServer(function(input,output,session){
  
  
  ###########見本データの作成############
  
  #------------#
  # sampleData #
  #------------#
  #testData : 見本データ
  #testDataと同じ形のデータを入力する必要がある。
  #credit,debit,edgelabel,Date
  
  #credit : generic type(現在日本語非対応)
  #         ノードの始点( 支出点 )
  #debit : generic type(現在日本語非対応)
  #         ノードの終点( 収入点 )
  #edgelabel : numeric type
  #         取引の増減額
  #Date : yyyy-mm-dd
  #         日付。例えば1994-09-19などの形
  
  
  #------------#
  # parameters #
  #------------#
  #dataNum : int
  #         見本データの個数
  #nodeLabel : a~oのアルファベット
  #           ノードの作成。今回はa~oと名前をつけておいた
  #credit,debit,edgeLabel,dateSeq : 先ほど説明したデータ
  dataNum = 20;
  nodeLabel = letters[1:15];
  credit = c();
  debit = c();
  edgeLabel = c();
  dateSeq = c();
  
  #---------#
  # process #
  #---------#
  #データの加工と作成
  for(i in 1:dataNum)
  {
    #credit,debit,edgeLabelの作成
    temp = sample(nodeLabel,2);
    credit[i] <- temp[1];
    debit[i] <- temp[2];
    edgeLabel[i] <- sample(seq(100,900,100),1);
  }
  
  initDate = as.Date("2019-01-01");
  for(i in 1:(dataNum/5))
  {
    #Dateの作成
    initDate = initDate+1;
    dateSeq[((i-1)*5+1):((i)*5)] <- rep(initDate,5);
  }
  dateSeq = as.character(as.Date(dateSeq,origin="1970-01-01"));
  
  #サンプル用データ(testData) : dataframeとして保存  
  testData = data.frame(
    debit=debit,
    credit=credit,
    edgeLabel=edgeLabel,
    Date=dateSeq
  );
  #作成した見本データをuiへ渡すためにrenderDataTableを通して変数tableへ格納
  output$table <- renderDataTable(testData);
  
  #uiのタイトル
  output$tableText=renderText({"以下のような形式のデータセットを用いる"});
  output$dynamicText <- renderRHandsontable({rhandsontable(data.frame(node="",init=0))});
  ###########見本データの作成終わり############
  
  ###########データアップロード後の振る舞い############
  
  #-----------#
  # csvUpload #
  #-----------#
  #CSVをアップロードしたあとに発生する処理を記述する。
  #アップロードしたファイルはUIのmydataに保存される。
  #observeEvent(input$mydata,{...})内の処理はmydataにデータが入って初めて実行される
  observeEvent(
    input$mydata,{
      output$tableText=renderText({"アップロード済みデータ"});
      #-------------#
      # outputTable #
      #-------------#
      #CSVをアップロードした場合に横にアップロードしたものを出力する
      name <- names(input$mydata);
      csv_file <- reactive(read.csv(input$mydata$datapath,sep=input$separator,fileEncoding = input$encode));
      output$table <- DT::renderDataTable(csv_file());
      
      #--------------------#
      # graphVisualization #
      #--------------------#
      #アップロードしたcsvファイルを次のタブで可視化する
      #csvファイルをDataに入れる
      #Data <- csv_file();
      Data <- read.csv(input$mydata$datapath,sep=input$separator,fileEncoding = input$encode)
      
      
      #uiから受け取ったbins情報をもとに階級幅の生成
      time_ <- unique(Data$Date);
      
      
      #####プロットの生成
      #####まず最も利用されるノードを求めて、一番前に置く(中心点になる)
      #NodeLabels : アップロードしたデータに利用されているノードのラベル
      NodeLabels=unique(c(levels(Data$credit),levels(Data$debit)));
      tempMat=table(Data[c("credit","debit")])
      #NodeNum : それぞれのノードの出現回数
      NodeNum=c();
      for(i in NodeLabels)
      {
        temp1=c();
        temp2=c();
        if(sum(rownames(tempMat)==i)>0)
        {
          temp1 <- sum(tempMat[i,]);
          temp1[is.na(temp1)] <- 0;
        }
        if(sum(colnames(tempMat)==i)>0)
        {
          temp2 <- sum(tempMat[,i]);
          temp2[is.na(temp2)] <- 0;
        }
        if(is.null(temp1)) temp1=0;
        if(is.null(temp2)) temp2=0;
        
        NodeNum[which(NodeLabels==i)] = sum(c(temp1,temp2));
      }
      
      #NodeLabelsの順序変換
      tempLabels=c();
      tempLabels[1] <- NodeLabels[which.max(NodeNum)];
      tempLabels[2:length(NodeLabels)] <- NodeLabels[-which.max(NodeNum)];
      NodeLabels=tempLabels;
      
      #固定ノードの生成
      nL=c();
      for(i in NodeLabels)
      {
        nL[which(NodeLabels==i)] <- i;
      }
      
      g=graph(c("None","None"),isolates=nL);
      g=delete_edges(g,E(g)[1]);
      g=delete_vertices(g,"None");
      
      #layoutListの生成
      layoutList=list();
      layoutList[[1]]=layout_as_star(g);
      layoutList[[2]]=layout_in_circle(g);
      layoutList[[3]]=layout_nicely(g);
      layoutList[[4]]=layout_randomly(g);
      layoutList[[5]]=layout_as_tree(g);
      layoutList[[6]]=layout_on_sphere(g);
      l=layoutList[[as.numeric(input$layoutIndex)]];
      
      
      #ノードのラベルの決定
      nodeListDataFrame=data.frame(node=nL,init=rep(0,length(nL)));
      nodeList=nodeListDataFrame$init;
      labelName=paste(V(g)$name,"\n",sep="");
      labelName=paste(labelName,nodeList,sep="");
      labelName=paste(labelName,"円",sep="");
      V(g)$label=labelName;
      
      
      graph_=list();
      graph_[[1]] <- g; 
      name_=c(" ",as.character(as.Date(time_,origin="1970-01-01")));
      Data_=list();
      Data_[[1]] <- data.frame(credit=c(),debit=c(),edgeLabel=c());
      
      #動的に値を変更できるrhandsontableでノードリストを引き渡す
      output$dynamicText <- renderRHandsontable({rhandsontable(nodeListDataFrame)})
      
      
      #fullDataを作っておく
      fullData=data.frame(credit=c(),debit=c(),edgeLabel=c())
      num=0
      Cashlist=NodeLabels
      inCashlist=rep(0,length(NodeLabels))
      outCashlist=rep(0,length(NodeLabels))
      
      for(nl0 in NodeLabels)
      {
        index=which(NodeLabels==nl0)
        tempData=Data[Data$credit==nl0,]
        inCashlist[index]=sum(tempData[,3])
        tempData=Data[Data$debit==nl0,]
        outCashlist[index]=sum(tempData[,3])
      }
      
      for(nl1 in NodeLabels)
      {
        for(nl2 in NodeLabels)
        {
          if(dim(Data[Data$credit==nl1,])[1]>0)
          {
            tempData=Data[Data$credit==nl1,]
            
            if(dim(tempData[tempData$debit==nl2,])[1]>0)
            {
              num=num+1
              tempData=tempData[tempData$debit==nl2,]
              fullData[num,"credit"]=nl1
              fullData[num,"debit"]=nl2
              fullData[num,"edgeLabel"]=sum(tempData[,3])
            }
          }
        }
      }
      
      
      #observe({...})内の処理は中のinputが変更されるたびに再実行される
      observe({
        #-----------#
        # inputList #
        #-----------#
        #以下のinput変数が変更された場合、再実行される
        input$eCurve
        input$eColor
        input$elcolor
        input$vColor
        input$vlcolor
        input$vfColor
        input$do1
        input$do2
        input$vSize
        input$upper
        input$lower
        input$layoutIndex
        input$dynamicText
        input$selector
        input$intdist
        
        #初期リストがuiで変更された場合にそれを反映する
        nodeListData=hot_to_r(input$dynamicText)
        nodeList=nodeListData$init;
        if((length(nodeList)==1)&(nodeList[1]==0)){
          nodeList=rep(0,length(NodeLabels))
        }
        labelName=paste(V(g)$name,"\n",sep="");
        labelName=paste(labelName,nodeList,sep="");
        labelName=paste(labelName,"円",sep="");
        V(g)$label=labelName;
        graph_[[1]] <- g;
        name_=c(" ",as.character(as.Date(time_,origin="1970-01-01")));
        Data_[[1]] <- data.frame(credit=c(),debit=c(),edgeLabel=c());
        
        
        #upper以上の額のエッジは出力しない
        upper=input$upper
        if(is.null(upper)==FALSE)
        {
          if(is.na(upper)==FALSE)
          {
            Data=Data[Data$edgeLabel<upper,]
          }
        }
        #lower以下の額のエッジは出力しない
        lower=input$lower
        if(is.null(lower)==FALSE){
          if(is.na(lower)==FALSE){
            Data=Data[Data$edgeLabel>lower,]
          }
        }
        #グラフの生成
        i = 1;
        for(t in time_)
        {
          Data_[[which(time_==t)+1]] <- Data[Data$Date==t,1:3];
          if(sum(Data$Date==t)>0)
          {
            num = 0;
            for(i in 1:dim(Data)[1])
            {
              if(Data$Date[i]==t)
              {
                num = num+1;
                #factorの水準値が出てくるのでcharに変換
                g = add_edges(g,c(as.character(Data$credit[i]),as.character(Data$debit[i])));
                E(g)$label[num] = Data$edgeLabel[i];
                
                nodeList[which(V(g)$name==as.character(Data$credit[i]))] = nodeList[which(V(g)$name==as.character(Data$credit[i]))] - Data$edgeLabel[i];
                nodeList[which(V(g)$name==as.character(Data$debit[i]))] = nodeList[which(V(g)$name==as.character(Data$debit[i]))] + Data$edgeLabel[i];
              }
            }
            
            if(input$do1%%2==0)
            {
              labelName <- paste(V(g)$name,"\n",sep="");
              labelName <- paste(labelName,nodeList,sep="");
              labelName <- paste(labelName,"円",sep="");
              V(g)$label <- labelName;
            }
            else
            {
              V(g)$label <- V(g)$name;
            }
          }
          graph_[[which(time_==t)+1]] <- g;
          g <- delete_edges(g,E(g));
        }
        
        
        #uiでinputしたeCurve,color,sizeを反映させる
        eCurve=input$eCurve
        eColor=input$eColor
        elColor=input$elColor
        
        vColor=input$vColor
        vlColor=input$vlColor
        vfColor=input$vfColor
        vSize=input$vSize
        
        
        
        if(input$do2%%2==0){
          output$myImage <- renderImage({
            width  <- session$clientData$output_myImage_width
            height <- session$clientData$output_myImage_height
            pixelratio <- session$clientData$pixelratio
            outfile <- tempfile(fileext='.png')
            png(outfile, width=width*pixelratio, height=height*pixelratio,
                res=72*pixelratio)
            
            plot(
              graph_[[input$bins+1]],
              layout=l,
              edge.arrow.size=0.5,
              edge.curved=eCurve,
              edge.color=eColor,
              edge.label.color=elColor,
              vertex.color=adjustcolor(c(vColor),alpha=0.8),
              vertex.size=vSize,
              vertex.frame.color=vfColor,
              vertex.label.color=vlColor,
              vertex.label.cex=1.0,
              vertex.label.dist=0,
              main=name_[input$bins+1],
              vertex.label.family="HiraKakuProN-W6"
            )
            
            dev.off()
            
            
            list(src = outfile,
                 width = width,
                 height = height,
                 alt = "This is alternate text")
            
            
            
          },deleteFile = TRUE)
          
          output$smallTable <- DT::renderDataTable(
            Data_[[input$bins+1]],
            options=list(pageLength=4)
          )
        }
        else
        {
          ###総計入力###
          output$smallTable <- DT::renderDataTable(
            fullData,
            options=list(pageLength=4)
          )
          nodeList=nodeListData$init
          
          ###総計のプロット作成
          for(i in 1:dim(fullData)[1])
          {
            #factorの水準値が出てくるのでcharに変換
            g = add_edges(g,c(as.character(fullData$credit[i]),as.character(fullData$debit[i])));
            E(g)$label[i] = fullData$edgeLabel[i];
            
          }
          if(input$sizeselector=="入金額"){
            node.size<-setNames((outCashlist)*0.7,V(g)$name)
          }
          if(input$sizeselector=="利用額"){
            node.size<-setNames((inCashlist)*0.7,V(g)$name)
          }
          if(input$sizeselector=="残額"){
            zangaku=outCashlist+nodeList-inCashlist
            for(i in zangaku){
              index=which(zangaku==i)
              if(i<0){
                zangaku[index]=0
              }
            }
            node.size<-setNames(zangaku*0.7,V(g)$name)
          }
          
          
          if(input$do1%%2==0)
          {
            #labelName <- paste(V(g)$name,"\n",sep="");
            #labelName <- paste(labelName,nodeList,sep="");
            #labelName <- paste(labelName,"円",sep="");
            labelName=V(g)$name;
            V(g)$label <- labelName;
          }
          else
          {
            V(g)$label <- V(g)$name;
          }
          
          ##plot
          
          ##自作レイアウト
          selector=input$selector
          selectdist=input$intdist
          g3=g
          deletelist=rep(0,length(V(g3)$name))
          if(sum(labelName==selector)==1){
            
            gdis=distances(g3,mode="out")[selector,]
            
            cdisMax=max(gdis[gdis!=Inf])
            self_layout=matrix(0,length(V(g3)$name),2)
            
            #現金に入る
            gdis2=distances(g3,mode="in")[selector,]
            
            cdisMax2=max(gdis2[gdis2!=Inf])
            notnum=9.0
            for(l in 0:cdisMax2){
              inCashlist=which(gdis2==l)
              if(l<=selectdist){
                ynum=length(inCashlist)
                ynumM=ynum
                for(i in inCashlist)
                {
                  if(cdisMax2==0){
                    self_layout[i,1]=0
                  }
                  else{
                    self_layout[i,1] = -12
                  }
                  self_layout[l,2] = ynum*(8/ynumM)
                  ynum= ynum-1
                  
                  
                }
              }
              else{
                deletelist[inCashlist]=1
              }
            }
            
            for(i in 0:cdisMax)
            {
              #現金から距離iになる勘定科目の取り出し
              cwdist=which(gdis==i)
              if(i<=selectdist){
                ynum=length(cwdist)
                ynumM=ynum
                
                for(l in cwdist)
                {
                  self_layout[l,1] = -(cdisMax-i)*(10/cdisMax)
                  self_layout[l,2] = ynum*(8/ynumM)
                  ynum= ynum-1
                  
                  if(i==0) {
                    self_layout[l,1] = -10
                    self_layout[l,2] = 3*(8/5)
                  }
                }
              }
              else{
                deletelist[cwdist]=1
              }
            }
            
            
            deletelist[intersect(which(gdis==Inf),which(gdis2==Inf))]=1
            
            g3=g3-V(g3)[which(deletelist>0)]
            #g3=g3-V(g3)$name[which(deletelist>0)]
            #g3=g3-V(g3)$label[which(deletelist>0)]
            self_layout=self_layout[-which(deletelist>0),]
            
            output$myImage <- renderImage({
              width  <- session$clientData$output_myImage_width
              height <- session$clientData$output_myImage_height
              pixelratio <- session$clientData$pixelratio
              outfile <- tempfile(fileext='.png')
              png(outfile, width=width*pixelratio, height=height*pixelratio,
                  res=72*pixelratio)
              par(family="HiraKakuProN-W6",plt=c(0, 1, 0, 1))
              plot(
                g3,
                layout=self_layout,
                edge.arrow.size=0.5,
                edge.curved=eCurve,
                edge.color=eColor,
                edge.label.color=elColor,
                vertex.color=adjustcolor(c(vColor),alpha=0.8),
                vertex.size=as.matrix(node.size),
                vertex.frame.color=vfColor,
                vertex.label.color=vlColor,
                vertex.label.cex=1.0,
                vertex.label.dist=0,
                main="総計",
                vertex.label.family="HiraKakuProN-W6"
              )
              
              dev.off()
              
              
              list(src = outfile,
                   width = width,
                   height = height,
                   alt = "This is alternate text")
              
              
              
            },deleteFile = TRUE)
            
          }
          else{
            output$myImage <- renderImage({
              width  <- session$clientData$output_myImage_width
              height <- session$clientData$output_myImage_height
              pixelratio <- session$clientData$pixelratio
              outfile <- tempfile(fileext='.png')
              png(outfile, width=width*pixelratio, height=height*pixelratio,
                  res=72*pixelratio)
              par(family="HiraKakuProN-W6",plt=c(0, 1, 0, 1))
              plot(0)
              dev.off()
              
              list(src = outfile,
                   width = width,
                   height = height,
                   alt = "This is alternate text")
              
              
              
            },deleteFile = TRUE)
            
          }
          
        }
        
        
      })
    })
})
###########アップロード後の振る舞い終わり############



#---------------#
# userInterface #
#---------------#
#userが利用する画面の規定

ui <- shinyUI(
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
      fileInput('mydata', 'CSVファイルを選択',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )),
      radioButtons("separator","セパレータ: ",choices = c(",",";",":"), selected=",",inline=TRUE),
      radioButtons("encode","文字コード: ",choices = c("utf8","Shift-JIS","cp932"), selected="Shift-JIS",inline=TRUE),
      column(9,
             h4(textOutput("tableText")),
             DT::dataTableOutput("table")
      )
    )
    ,
    
    tabPanel(
      "可視化",
      titlePanel("会計可視化"),
      
      fluidRow(
        column(3),
        
        column(9,
               #グラフの表示部分
               mainPanel(
                 imageOutput("myImage")
               )
        )
      ),
      
      fluidRow(
        column(3,
               sliderInput(
                 "bins",
                 "時間",
                 min = 0,
                 max = 20,
                 value = 0
               ),
               numericInput(
                 "eCurve",
                 "矢印の曲率(0以上)",
                 value=0.1,
                 min=0,
                 step=0.1
               )
               
        ),
        
        column(6,
               dataTableOutput("smallTable")
               
        ),
        
        column(3,
               actionButton("do1","残高表示/非表示"),
               actionButton("do2","総計表示/個別期間表示"),
               radioButtons("selector","表示選択: ",choices = c("現金","売上","販管費","有価証券","その他収益","その他費用"), selected="現金",inline=TRUE),
               radioButtons("sizeselector","サイズ設定基準: ",choices = c("利用額","入金額","残額"), selected="入金額",inline=TRUE),
               numericInput(
                 "intdist",
                 "距離",
                 value=1,
                 min=0,
                 step=1
               )
        )
        
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



shinyApp(ui = ui, server = server)