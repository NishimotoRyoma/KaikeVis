#----------#
# packages #
#----------#

library(shiny)
library(igraph)
library(DT)
library(shinyjs)
library(colourpicker)


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
  dataNum = 20;
  nodeLabel = letters[1:15];
  nodeS = c();
  nodeF = c();
  edgeLabel = c();
  dateSeq = c();
  
  #---------#
  # process #
  #---------#
  #データの加工と作成
  for(i in 1:dataNum)
  {
    #nodeS,nodeF,edgeLabelの作成
    temp = sample(nodeLabel,2);
    nodeS[i] <- temp[1];
    nodeF[i] <- temp[2];
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
    nodeS=nodeS,
    nodeF=nodeF,
    edgeLabel=edgeLabel,
    Date=dateSeq
    );
  
  #作成した見本データをuiへ渡すためにrenderDataTableを通して変数tableへ格納
  output$table <- renderDataTable(testData);
  
  #uiのタイトル
  output$tableText=renderText({"以下のような形式のデータセットを用いる"});
  
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
        input$vSize
        input$upper
        input$lower
        input$layoutIndex
    
        #-------------#
        # outputTable #
        #-------------#
        #CSVをアップロードした場合に横にアップロードしたものを出力する
        name <- names(input$mydata);
        csv_file <- reactive(read.csv(text=input$mydata[[name]]));
        output$table <- DT::renderDataTable(csv_file());

        #--------------------#
        # graphVisualization #
        #--------------------#
        #アップロードしたcsvファイルを次のタブで可視化する
        #csvファイルをDataに入れる
        Data <- csv_file();

  
        #uiから受け取ったbins情報をもとに階級幅の生成
        time_ <- unique(Data$Date);


        #####プロットの生成
        #####まず最も利用されるノードを求めて、一番前に置く(中心点になる)
        #NodeLabels : アップロードしたデータに利用されているノードのラベル
        NodeLabels=unique(c(levels(Data$nodeS),levels(Data$nodeF)));
        tempMat=table(Data[c("nodeS","nodeF")])
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
        nodeList=rep(0,length(nL));
        labelName=paste(V(g)$name,"\n",sep="");
        labelName=paste(labelName,nodeList,sep="");
        #labelName=paste(labelName,"yen",sep="");
        V(g)$label=labelName;
  
  
        graph_=list();
        graph_[[1]] <- g;
        name_=c(" ",as.character(as.Date(time_,origin="1970-01-01")));
        Data_=list();
        Data_[[1]] <- data.frame(nodeS=c(),nodeF=c(),edgeLabel=c());
        
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
                g = add_edges(g,c(as.character(Data$nodeS[i]),as.character(Data$nodeF[i])));
                E(g)$label[num] = Data$edgeLabel[i];
        
                nodeList[which(V(g)$name==as.character(Data$nodeS[i]))] = nodeList[which(V(g)$name==as.character(Data$nodeS[i]))] - Data$edgeLabel[i];
                nodeList[which(V(g)$name==as.character(Data$nodeF[i]))] = nodeList[which(V(g)$name==as.character(Data$nodeF[i]))] + Data$edgeLabel[i];
              }
            }
          
            if(input$do1%%2==0)
            {
              labelName <- paste(V(g)$name,"\n",sep="");
              labelName <- paste(labelName,nodeList,sep="");
              #labelName <- paste(labelName,"yen",sep="");
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
  
        output$distPlot <- renderPlot({
          par(family="HiraKakuProN-W3");
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
            main=name_[input$bins+1]
          )


        })
  
        output$smallTable <- DT::renderDataTable(
          Data_[[input$bins+1]],
          options=list(pageLength=5)
        )
  
    }) 
  })
})
###########アップロード後の振る舞い終わり############