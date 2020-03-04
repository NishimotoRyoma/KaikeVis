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
  
  ###########データアップロード後の振る舞い############
  #-----------#
  # csvUpload #
  #-----------#
  #CSVをアップロードしたあとに発生する処理を記述する。
  #アップロードしたファイルはUIのmydataに保存される。
  #observeEvent(input$mydata,{...})内の処理はmydataにデータが入って初めて実行される
  observeEvent(input$mydata,{
    
    
    
      output$tableText=renderText({"アップロード済みデータ1"});
      output$tableText2=renderText({"アップロード済みデータ2"});
      #-------------#
      # outputTable #
      #-------------#
      #CSVをアップロードした場合に横にアップロードしたものを出力する
      output$table <- renderTable({
        req(input$mydata)
        upload = list()
          upload <- read.csv(
            file = input$mydata[[1, 'datapath']])
        return(upload)
      })
      
      output$table2 <- renderTable({
        req(input$mydata)
        upload = list()
        upload <- read.csv(
          file = input$mydata[[2, 'datapath']])
        return(upload)
      })
      
      #csvファイルをDataに入れる
      Data <- read.csv(input$mydata[[1, 'datapath']],sep=input$separator,fileEncoding = input$encode)
      Data2 <- read.csv(input$mydata[[2, 'datapath']],sep=input$separator,fileEncoding = input$encode)
  
  
  #------------#
  # データ加工 #
  #------------#
  
  observeEvent(input$action,{
    
    #置換関数
    replacelabel=function(x,ad,adla){
      if(sum(ad==x)>0){
        return(adla)
      }
      else{
        return(x)
      }
    }
    
    #変換
    adall=c(input$ad1,input$ad2,input$ad3,input$ad4,input$ad5,input$ad6,input$ad7)
    adlaall=c(input$adla1,input$adla2,input$adla3,input$adla4,input$adla5,input$adla6,input$adla7)
    
    for(i in 1:7){
        ad1=adall[i]
        adla1=adlaall[i]
        ad1List=strsplit(ad1,",")
    
        if(length(intersect(levels(Data$credit),ad1List[[1]]))>0){
          levels(Data$credit)=append(levels(Data$credit),adla1)
          for(i in 1:length(Data$credit)){
            Data$credit[i]=replacelabel(Data$credit[i],ad1List[[1]],adla1)
          }
        }
    
    
    
        if(length(intersect(levels(Data$debit),ad1List[[1]]))>0){
          levels(Data$debit)=append(levels(Data$debit),adla1)
          for(i in 1:length(Data$debit)){
            Data$debit[i]=replacelabel(Data$debit[i],ad1List[[1]],adla1)
          }
        }
    
        if(length(intersect(levels(Data2$credit),ad1List[[1]]))>0){
          levels(Data2$credit)=append(levels(Data2$credit),adla1)
          for(i in 1:length(Data2$credit)){
            Data2$credit[i]=replacelabel(Data2$credit[i],ad1List[[1]],adla1)
          }
        }
    
        if(length(intersect(levels(Data2$debit),ad1List[[1]]))>0){
          levels(Data2$debit)=append(levels(Data2$debit),adla1)
          for(i in 1:length(Data2$debit)){
            Data2$debit[i]=replacelabel(Data2$debit[i],ad1List[[1]],adla1)
          }
        }
      }
    
    #変換後の受け渡し
    #levelで使われていないものを落とす
    Data$credit=Data$credit[,drop=TRUE]
    Data$debit=Data$debit[,drop=TRUE]
    Data2$credit=Data2$credit[,drop=TRUE]
    Data2$debit=Data2$debit[,drop=TRUE]
    #UIへ
    output$ttable=renderTable(Data)
    output$ttable2=renderTable(Data2)
    
  
    #ダウンロードの作成
    output$out_down1 <- downloadHandler(
      filename = function() {
        paste("Data1", "csv", sep = ".")
      },
      
      content = function(file) {
        
        write.csv(Data,
                    file)
      }
    )
    
    output$out_down2 <- downloadHandler(
      filename = function() {
        paste("Data2", "csv", sep = ".")
      },
      
      content = function(file) {
        
        write.csv(Data2,
                  file)
      }
    )
    
    
    
    
    
  
  #--------------#
  # ノードの固定 #
  #--------------#
  
  observe({
  #Data,Data2で用いられている勘定科目をすべて取り出す。
  Dcre=levels(Data$credit)
  Ddebi=levels(Data$debit)
  D2cre=levels(Data2$credit)
  D2debi=levels(Data2$debit)
  creditLabels=unique(c(Dcre,D2cre))
  debitLabels=unique(c(Ddebi,D2debi))
  NodeLabels=unique(c(creditLabels,debitLabels))
  Dataall=rbind(Data,Data2)
  
  #edgeを整理する
  fulledgeList=data.frame(credit=c(),debit=c())
  num=0
  for(nl1 in NodeLabels)
  {
    for(nl2 in NodeLabels)
    {
      if(dim(Dataall[Dataall$credit==nl1,])[1]>0)
      {
        tempData=Dataall[Dataall$credit==nl1,]
        
        if(dim(tempData[tempData$debit==nl2,])[1]>0)
        {
          num=num+1
          fulledgeList[num,"credit"]=nl1
          fulledgeList[num,"debit"]=nl2
        }
      }
    }
  }
  
  
  #売上から距離1のものを取り出す
  UriedgeList1=fulledgeList[(fulledgeList$credit==input$fixright2),]
  UriedgeList2=fulledgeList[(fulledgeList$debit==input$fixright2),]
  UriedgeList=rbind(UriedgeList1,UriedgeList2)

  #売上と関係ない部分を取り出す
  NUedgeList1=fulledgeList[(fulledgeList$credit!=input$fixright2),]
  NUedgeList2=NUedgeList1[(NUedgeList1$debit!=input$fixright2),]
  NUedgeList=NUedgeList2
  
  #それぞれのグラフオブジェクトの作成
  gNU=graph(t(as.matrix(apply(NUedgeList,MARGIN=c(1,2),FUN=as.character))),directed=TRUE)
  NodeLabelsNU=V(gNU)$name
  
  gU=graph(t(as.matrix(apply(UriedgeList,MARGIN=c(1,2),FUN=as.character))),directed=TRUE)
  NodeLabelsU=V(gU)$name
  
  #座標の設定
  #グラフの両端
  xlim=c(0,10)
  ylim=c(0,10)
  
  #売上部分の座標
  self_layoutU=matrix(-10,length(NodeLabelsU),2)
  self_layoutU[which(NodeLabelsU==input$fixright2),1]=0
  self_layoutU[which(NodeLabelsU==input$fixright2),2]=5
  
  num=0
  for(i in NodeLabelsU){
    if(i==input$fixright2){}
    else{
      self_layoutU[which(NodeLabelsU==i),2]=5-num
      num=num+1
    }
  }
  
  gU=delete.edges(gU,E(gU))
  

  
  #非売上部分
  #現金該当物
  targ=input$fixleft
  gdis=distances(gNU,mode="out")[targ,]
  cdisMax=max(gdis[gdis!=Inf])
  self_layoutNU=matrix(0,length(NodeLabelsNU),2)
  
  for(i in 0:cdisMax)
  {
    #現金から距離iになる勘定科目の取り出し
    cwdist=which(gdis==i)

      ynum=length(cwdist)
      ynumM=ynum
      for(l in cwdist)
      {
        self_layoutNU[l,1] = -(cdisMax-i+1)*(10/(cdisMax+2))
        self_layoutNU[l,2] = ynum*(7/ynumM)
        ynum= ynum-1
        if(i==0) self_layoutNU[l,2] = 5
      }
  }
  
  cwdistInf=which(gdis==Inf)
  num=0
  for(l in cwdistInf){
    self_layoutNU[l,1]=-(8/length(cwdistInf))*num+1
    self_layoutNU[l,2]=10
  }
  
  leftList=strsplit(input$fixleft2,",")

  num=0
  for(i in NodeLabelsNU){
    if(i==input$fixright){
      self_layoutNU[which(NodeLabelsNU==i),1]=0
      self_layoutNU[which(NodeLabelsNU==i),2]=5
    }
    for(l in leftList[[1]]){
      if(i==l){
        self_layoutNU[which(NodeLabelsNU==i),1]=-(cdisMax+2)*(10/(cdisMax+2))
        self_layoutNU[which(NodeLabelsNU==i),2]=(10/(length(leftList[[1]])))*num
        num=num+1
      }
    }
  }
  
  

  
  gNU=delete.edges(gNU,E(gNU))
  
  
  
  #------#
  # plot #
  #------#
  
  #企業1
  #売上
  UData1=Data[(Data$credit==input$fixright2),2:1]
  UData2=Data[(Data$debit==input$fixright2),2:1]
  UData=rbind(UData1,UData2)
  gU1=add.edges(gU,t(as.matrix(apply(UData,MARGIN=c(1,2),FUN=as.character))))
  
  #非売上
  NUData1=Data[(Data$credit!=input$fixright2),1:3]
  NUData2=NUData1[(NUData1$debit!=input$fixright2),c(2:1,3)]
  NUData=NUData2
  gNU1=add.edges(gNU,t(as.matrix(apply(NUData[,1:2],MARGIN=c(1,2),FUN=as.character))))
  
  inCashlist1=rep(0,length(V(gNU1)$name))
  outCashlist1=rep(0,length(V(gNU1)$name))
  
  for(l in V(gNU1)$name){
    index=which(V(gNU1)$name==l)
    tempData=NUData[NUData$credit==l,]
    inCashlist1[index]=sum(tempData[,3])
    tempData=NUData[NUData$debit==l,]
    outCashlist1[index]=sum(tempData[,3])
  }
  
  

  #企業2
  #売上
  UData1=Data2[(Data2$credit==input$fixright2),2:1]
  UData2=Data2[(Data2$debit==input$fixright2),2:1]
  UData=rbind(UData1,UData2)
  gU2=add.edges(gU,t(as.matrix(apply(UData,MARGIN=c(1,2),FUN=as.character))))
  
  #非売上
  NUData1=Data2[(Data2$credit!=input$fixright2),1:3]
  NUData2=NUData1[(NUData1$debit!=input$fixright2),c(2:1,3)]
  NUData=NUData2
  gNU2=add.edges(gNU,t(as.matrix(apply(NUData[,1:2],MARGIN=c(1,2),FUN=as.character))))
  
  inCashlist2=rep(0,length(V(gNU2)$name))
  outCashlist2=rep(0,length(V(gNU2)$name))
  
  for(l in V(gNU2)$name){
    index=which(V(gNU2)$name==l)
    tempData=NUData[NUData$credit==l,]
    inCashlist2[index]=sum(tempData[,3])
    tempData=NUData[NUData$debit==l,]
    outCashlist2[index]=sum(tempData[,3])
  }
  
  
  #選択外の削除
  selector=input$selector
  selectdist=input$intdist
  deletelist=rep(0,length(V(gNU)$name))
  
  #細かい設定
  #uiでinputしたeCurve,color,sizeを反映させる
  eCurve=input$eCurve
  eColor=input$eColor
  elColor=input$elColor
  
  vColor=input$vColor
  vlColor=input$vlColor
  vfColor=input$vfColor
  vSize=input$vSize
  
  if(input$sizeselector=="入金額"){
    node.size1<-setNames((outCashlist1)*0.7,V(gNU)$name)
    node.size2<-setNames((outCashlist2)*0.7,V(gNU)$name)
  }
  if(input$sizeselector=="利用額"){
    node.size1<-setNames((inCashlist1)*0.7,V(gNU)$name)
    node.size2<-setNames((inCashlist2)*0.7,V(gNU)$name)
  }
  if(input$sizeselector=="残額"){
    zangaku1=outCashlist1-inCashlist1
    for(i in zangaku1){
      index=which(zangaku1==i)
      if(i<0){
        zangaku1[index]=0
      }
    }
    
    zangaku2=outCashlist2-inCashlist2
    for(i in zangaku2){
      index=which(zangaku2==i)
      if(i<0){
        zangaku2[index]=0
      }
    }
    
    node.size1<-setNames(zangaku1*0.7,V(gNU)$name)
    node.size2<-setNames(zangaku2*0.7,V(gNU)$name)
  }
  

  
  if(sum(V(gNU)$name==selector)==1){
    
    #レイアウト修正
    gdis=distances(gNU1,mode="out")[selector,]
    
    cdisMax=max(gdis[gdis!=Inf])
    for(i in 0:cdisMax)
    {
      cwdist=which(gdis==i)
      if(i>selectdist){
        deletelist[cwdist]=1
      }
    }
    
    gdis2=distances(gNU1,mode="in")[selector,]
    cdisMax2=max(gdis2[gdis2!=Inf])
    for(l in 0:cdisMax2){
      inCashlist=which(gdis2==l)
      if(l>selectdist){
        deletelist[inCashlist]=1
      }
    }
    deletelist[intersect(which(gdis==Inf),which(gdis2==Inf))]=1
    gNU1=gNU1-V(gNU1)[which(deletelist>0)]
    #gNU2=gNU2-V(gNU2)[which(deletelist>0)]
    self_layoutNU1=self_layoutNU[-which(deletelist>0),]
    node.size1=node.size1[-which(deletelist>0)]
    
    
    
    gdis=distances(gNU2,mode="out")[selector,]
    
    cdisMax=max(gdis[gdis!=Inf])
    for(i in 0:cdisMax)
    {
      cwdist=which(gdis==i)
      if(i>selectdist){
        deletelist[cwdist]=1
      }
    }
    
    gdis2=distances(gNU2,mode="in")[selector,]
    cdisMax2=max(gdis2[gdis2!=Inf])
    for(l in 0:cdisMax2){
      inCashlist=which(gdis2==l)
      if(l>selectdist){
        deletelist[inCashlist]=1
      }
    }
    deletelist[intersect(which(gdis==Inf),which(gdis2==Inf))]=1
    gNU2=gNU2-V(gNU2)[which(deletelist>0)]
    self_layoutNU2=self_layoutNU[-which(deletelist>0),]
    node.size2=node.size2[-which(deletelist>0)]
    
    #画像引き渡し
    output$myImageNU1 <- renderImage({
      width  <- session$clientData$output_myImageNU1_width
      height <- session$clientData$output_myImageNU1_height
      pixelratio <- session$clientData$pixelratio
      outfile <- tempfile(fileext='.png')
      png(outfile, width=width*pixelratio, height=height*pixelratio,
          res=72*pixelratio)
      par(family="HiraKakuProN-W6",plt=c(0, 1, 0, 1))
      plot(
        gNU1,
        layout=self_layoutNU1,
        edge.arrow.size=0.5,
        edge.curved=eCurve,
        edge.color=eColor,
        edge.label.color=elColor,
        vertex.color=adjustcolor(c(vColor),alpha=0.8),
        vertex.size=as.matrix(node.size1),
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
    
    
    output$myImageNU2 <- renderImage({
      width  <- session$clientData$output_myImageNU2_width
      height <- session$clientData$output_myImageNU2_height
      pixelratio <- session$clientData$pixelratio
      outfile <- tempfile(fileext='.png')
      png(outfile, width=width*pixelratio, height=height*pixelratio,
          res=72*pixelratio)
      par(family="HiraKakuProN-W6",plt=c(0, 1, 0, 1))
      plot(
        gNU2,
        layout=self_layoutNU2,
        edge.arrow.size=0.5,
        edge.curved=eCurve,
        edge.color=eColor,
        edge.label.color=elColor,
        vertex.color=adjustcolor(c(vColor),alpha=0.8),
        vertex.size=as.matrix(node.size2),
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
    output$myImageNU1 <- renderImage({
      width  <- session$clientData$output_myImageNU1_width
      height <- session$clientData$output_myImageNU1_height
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
    
    output$myImageNU2 <- renderImage({
      width  <- session$clientData$output_myImageNU2_width
      height <- session$clientData$output_myImageNU2_height
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
  
  output$myImageU1 <- renderImage({
    width  <- session$clientData$output_myImageU1_width
    height <- session$clientData$output_myImageU1_height
    pixelratio <- session$clientData$pixelratio
    outfile <- tempfile(fileext='.png')
    png(outfile, width=width*pixelratio, height=height*pixelratio,
        res=72*pixelratio)
    par(family="HiraKakuProN-W6",plt=c(0, 1, 0, 1))
    plot(
      gU1,
      layout=self_layoutU,
      edge.arrow.size=0.5,
      edge.curved=eCurve,
      edge.color=eColor,
      edge.label.color=elColor,
      vertex.color=adjustcolor(c(vColor),alpha=0.8),
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
  
  
  output$myImageU2 <- renderImage({
    width  <- session$clientData$output_myImageU2_width
    height <- session$clientData$output_myImageU2_height
    pixelratio <- session$clientData$pixelratio
    outfile <- tempfile(fileext='.png')
    png(outfile, width=width*pixelratio, height=height*pixelratio,
        res=72*pixelratio)
    par(family="HiraKakuProN-W6",plt=c(0, 1, 0, 1))
    plot(
      gU2,
      layout=self_layoutU,
      edge.arrow.size=0.5,
      edge.curved=eCurve,
      edge.color=eColor,
      edge.label.color=elColor,
      vertex.color=adjustcolor(c(vColor),alpha=0.8),
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
    
  })
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
    tabPanel(
      "データの入力",
      fluidRow(
      
      column(12,fileInput('mydata', 'CSVファイルを選択(同時選択で複数アップロード可)',
                multiple = TRUE,
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )),
      radioButtons("separator","セパレータ: ",choices = c(",",";",":"), selected=",",inline=TRUE),
      radioButtons("encode","文字コード: ",choices = c("utf8","Shift-JIS","cp932"), selected="Shift-JIS",inline=TRUE),
      
      ),
      fluidRow(
      column(4,
             h4(textOutput("tableText")),
             #DT::dataTableOutput("table")
             tableOutput("table")
      ),
      column(3),
      column(4,
             h4(textOutput("tableText2")),
             #DT::dataTableOutput("table")
             tableOutput("table2")
      )
      )
    )
    ,
    ),
    
    tabPanel("データ加工",
      
             fluidRow(
             h4("勘定科目の修正"),
            column(3,
    h5("統一したい勘定科目をコンマ区切りで入力してください。"),
    textInput("ad1", label="売上", value = "売上高,売上"),
    textInput("ad2", label="売上原価", value = "売上原価,仕入れ,仕入"),
    textInput("ad3", label="現金預金", value = "現金預金,現金,預金"),
    textInput("ad4", label="販管費", value = "一般管理費,販売費および一般管理費,販売費及び一般管理費"),
    textInput("ad5", label="有価証券", value = "売買目的有価証券,有価証券,その他有価証券,満期保有目的有価証券"),
    textInput("ad6", label="その他収益", value = "受取利息,有価証券売買益"),
    textInput("ad7", label="その他費用", value = "減価償却費,手形売却損")
            ),
    column(3,
           h5("統一に用いる勘定科目名を入力してください。"),
           textInput("adla1",label="売上", value = "売上"),
           textInput("adla2",label="売上原価", value = "売上原価"),
           textInput("adla3",label="現金預金", value = "現金"),
           textInput("adla4",label="販管費", value = "販管費"),
           textInput("adla5",label="有価証券", value = "有価証券"),
           textInput("adla6",label="その他収益", value = "その他収益"),
           textInput("adla7",label="その他費用", value = "その他費用")
           ),
    column(3,actionButton("action","加工の実行"),
           downloadButton("out_down1", "企業１ダウンロード"),
            downloadButton("out_down2", "企業2ダウンロード")
    )
    
             ),
    fluidRow(
    
    column(4,
           h4("変換後データ(企業1)"),
           #DT::dataTableOutput("table")
           tableOutput("ttable")
    ),
    column(3),
    column(4,
           h4("変換後データ(企業2)"),
           #DT::dataTableOutput("table")
           tableOutput("ttable2")
    )
    
    )
      
    ),
    
    tabPanel("可視化設定",
             column(3,
                    h5("現金預金に該当する勘定科目をで入力してください。グラフの左端に表示されます。"),
                    textInput("fixleft", label="現金預金", value = "現金")
             ),
             column(3,
                    h5("売掛金など現金以外で資金源に該当する勘定科目をコンマ区切りで入力してください。グラフの左端に表示されます。"),
                    textInput("fixleft2", label="売掛金等", value = "売掛金,受取手形")
             ),
             column(3,
                    h5("売上原価に相当する勘定科目を入力してください。グラフの右端に表示されます。"),
                    textInput("fixright", label="売上原価", value = "売上原価")
             ),
             column(3,
                    h5("売上に相当する勘定科目を入力してください。グラフの右端に表示されます。"),
                    textInput("fixright2", label="売上", value = "売上")
             )
                    
             
             
             
      ),
    
    tabPanel(
      "可視化",
      titlePanel("会計可視化"),

      fluidRow(column(6,h4("企業１")),column(6,h4("企業2"))),
      
      fluidRow(
        column(6,
               #グラフの表示部分
               mainPanel(
                 imageOutput("myImageNU1")
               )
        ),
      column(6,
             #グラフの表示部分
             mainPanel(
               imageOutput("myImageNU2")
             )
      ) 
        
      ),
        
      fluidRow(
        column(6,
               #グラフの表示部分
               mainPanel(
                 imageOutput("myImageU1")
               )
        ),
        column(6,
               #グラフの表示部分
               mainPanel(
                 imageOutput("myImageU2")
               )
        )
        
      ),
      
      fluidRow(
        column(3,
               radioButtons("selector","表示選択: ",choices = c("現金","売上","販管費","有価証券","その他収益","その他費用"), selected="現金",inline=TRUE),
               radioButtons("sizeselector","サイズ設定基準: ",choices = c("利用額","入金額","残額"), selected="入金額",inline=TRUE),
               numericInput(
                 "intdist",
                 "距離",
                 value=1,
                 min=0,
                 step=1
               ))
        
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
      )
    )
  )
)



shinyApp(ui = ui, server = server)