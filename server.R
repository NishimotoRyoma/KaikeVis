#----------#
# packages #
#----------#

library(shiny)
library(igraph)
library(DT)


#-----------#
# webServer #
#-----------#
#server内の振る舞いに関するコード
#shinyServer(){...}において、Server内の動きを規定する

shinyServer(function(input,output){
  
  
  ###########見本データの作成############
  
  #------------#
  # sampleData #
  #------------#
  #testData : 見本データ
  #testDataと同じ形のデータを入力する必要がある。
  #nodeS,nodeF,edgelabel,Date
  
  #nodeS : generic type(現在日本語非対応)
  #         ノードの始点( 支出点 )
  #nodeF : generic type(現在日本語非対応)
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
  #nodeS,nodeF,edgeLabel,dateSeq : 先ほど説明したデータ
  dataNum=20
  nodeLabel=letters[1:15];
  nodeS=c()
  nodeF=c()
  edgeLabel=c()
  dateSeq=c()  
  
  #---------#
  # process #
  #---------#
  #データの加工と作成
  for(i in 1:dataNum){
    temp=sample(nodeLabel,2);
    nodeS[i]=temp[1];
    nodeF[i]=temp[2];
    edgeLabel[i]=sample(seq(100,900,100),1);
  }
  
  initDate=as.Date("2019-01-01")
  for(i in 1:(dataNum/5)){
    initDate=initDate+1
    dateSeq[((i-1)*5+1):((i)*5)]=rep(initDate,5)
  }
  dateSeq = as.character(as.Date(dateSeq,origin="1970-01-01"))
  
  #testData : dataframeとして保存
  testData=data.frame(nodeS=nodeS,nodeF=nodeF,edgeLabel=edgeLabel,Date=dateSeq)
  
  #uiへ渡すためにrenderDataTableを通して変数tableへ格納
  output$table <- renderDataTable(testData)
  
  ###########見本データの作成終わり############
  
  ###########アップロード後の振る舞い############
  
  #-----------#
  # csvUpload #
  #-----------#
  #CSVをアップロードしたあとに発生する処理を記述する。
  #アップロードしたファイルはUIのmydataに保存される。
  #observeEvent(input$mydata,{...})内の処理はmydataにデータが入って初めて実行される
  observeEvent(input$mydata,{
    
    #-------------#
    # outputTable #
    #-------------#
    #CSVをアップロードした場合に横にアップロードしたものを出力する
    name <- names(input$mydata)
    csv_file <- reactive(read.csv(text=input$mydata[[name]]))
    output$table <- DT::renderDataTable(csv_file())

    #--------------------#
    # graphVisualization #
    #--------------------#
    #アップロードしたcsvファイルを次のタブで可視化する
    
    Data=csv_file()

  
  #uiから受け取ったbins情報をもとに階級幅の生成
  time_ <- unique(Data$Date)
  #output$lt <- length(time_)

  
  #プロットの生成
  
  #最も利用されるノードを求めて、一番前に置く(中心点になる)
  NodeLabels=unique(c(levels(Data$nodeS),levels(Data$nodeF)))
  tempMat=table(Data[c("nodeS","nodeF")])
  NodeNum=c()
  for(i in NodeLabels){
    temp1=c()
    temp2=c()
    if(sum(rownames(tempMat)==i)>0){
      temp1 <- sum(tempMat[i,])
      temp1[is.na(temp1)] <- 0
    }
    if(sum(colnames(tempMat)==i)>0){    
      temp2 <- sum(tempMat[,i])
      temp2[is.na(temp2)] <- 0
    }
    if(is.null(temp1)) temp1=0
    if(is.null(temp2)) temp2=0
    
    NodeNum[which(NodeLabels==i)] = sum(c(temp1,temp2))
  }
  
  tempLabels=c()
  tempLabels[1]=NodeLabels[which.max(NodeNum)]
  tempLabels[2:length(NodeLabels)]=NodeLabels[-which.max(NodeNum)]
  NodeLabels=tempLabels
  
  #固定ノードの生成
  nL=c()
  for(i in NodeLabels){
    nL[which(NodeLabels==i)]=i;
  }
  
  g=graph(c("None","None"),isolates=nL)
  g=delete_edges(g,E(g)[1])
  g=delete_vertices(g,"None")
  l=layout_as_star(g)
  
  
  
  
  #ノードのラベルの決定
  nodeList=rep(0,length(nL))
  labelName=paste(V(g)$name,"\n(",sep="")
  labelName=paste(labelName,nodeList,sep="")
  labelName=paste(labelName,"yen)",sep="")
  V(g)$label=labelName
  
  
  graph_=list()
  graph_[[1]]=g
  name_=c(" ",as.character(as.Date(time_,origin="1970-01-01")))
  Data_=list()
  Data_[[1]]=data.frame(nodeS=c(),nodeF=c(),edgeLabel=c())
  
  #グラフの生成
  i=1
  for(t in time_){
    Data_[[which(time_==t)+1]]=Data[Data$Date==t,1:3]
    num=0
    for(i in 1:dim(Data)[1]){
      if(Data$Date[i]==t){
        num=num+1
        
        ##factorの水準値が出てくるのでcharに変換
        g=add_edges(g,c(as.character(Data$nodeS[i]),as.character(Data$nodeF[i])))
        E(g)$label[num] = Data$edgeLabel[i]
      
        nodeList[which(V(g)$name==as.character(Data$nodeS[i]))]=nodeList[which(V(g)$name==as.character(Data$nodeS[i]))]-Data$edgeLabel[i]
        nodeList[which(V(g)$name==as.character(Data$nodeF[i]))]=nodeList[which(V(g)$name==as.character(Data$nodeF[i]))]+Data$edgeLabel[i]
      }
    }
    labelName=paste(V(g)$name,"\n(",sep="")
    labelName=paste(labelName,nodeList,sep="")
    labelName=paste(labelName,"yen)",sep="")
    V(g)$label=labelName
    graph_[[which(time_==t)+1]] <- g
    g=delete_edges(g,E(g))
  }

  
  
  output$distPlot <- renderPlot({
    par(family="HiraKakuProN-W3")
    plot(graph_[[input$bins+1]],layout=l,edge.arrow.size=0.5, vertex.color=adjustcolor(c("gold"),alpha=0.8), vertex.size=20, vertex.frame.color="grey", vertex.label.color="black", vertex.label.cex=1.0, vertex.label.dist=0,main=name_[input$bins+1])

  })
  
  output$smallTable <- DT::renderDataTable({
    Data_[[input$bins+1]]
  })
  
  }) 
})

###########アップロード後の振る舞い終わり############