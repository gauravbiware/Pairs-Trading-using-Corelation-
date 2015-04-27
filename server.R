# server.R
#Date 5th Mar 2015 


library(quantmod)
library(dygraphs)
source('GetTradeData.R', local=TRUE)


shinyServer(function(input, output) {
 
    dataInput1<-reactive({
      if(input$get==0)return(NULL)
      datainput1<-GETSYMBOl1(input$symb1,src=input$radio,from=input$dates[1],to=input$dates[2])
    })
      
  dataInput2<-reactive({
    if(input$get==0)return(NULL)
    datainput2<-GETSYMBOl1(input$symb2,src=input$radio,from=input$dates[1],to=input$dates[2])
  })
  
  dataInput3<-reactive({
    if(input$get==0)return(NULL)
    datainput3<-GETSYMBOl3(input$symb3,src=input$radio,from=input$dates[1],to=input$dates[2])
  })
  
  
  dataInput4<-reactive({
    if(input$get==0)return(NULL)
    datainput4<-GETSYMBOl4(input$symb4,src=input$radio,from=input$dates[1],to=input$dates[2])
  })
  
  Chosen_stock_1_dataseries<-reactive({
    if(input$get==0)return(NULL)
    isolate({ 
      chosen_stock1<-as.character(ChooseStocks1())
      if(chosen_stock1==input$symb1){dataInput1()}
      else {
        if(chosen_stock1==input$symb2){dataInput2()}
        else {
          if(chosen_stock1==input$symb3){dataInput3()} else {dataInput4()}
        }
      }
    })
  })
  
  Chosen_stock_2_dataseries<-reactive({
    if(input$get==0)return(NULL)
    isolate({ 
      chosen_stock2<-as.character(ChooseStocks2())
      if(chosen_stock2==input$symb1){dataInput1()}
      else {
        if(chosen_stock2==input$symb2){dataInput2()}
        else {
          if(chosen_stock2==input$symb3){dataInput3()} else {dataInput4()}
        }
      }
    })
  })
  
  
  
  output$covarmatrix<-renderTable({
    CovarMatFunction()
  })
  
  CovarMatFunction=reactive({
    if(input$get==0)return(NULL) 
    stck1<-dataInput1()[,4]
    colnames(stck1)<-input$symb1
    stck2<-dataInput2()[,4]
    colnames(stck2)<-input$symb2
    stck3<-dataInput3()[,4]
    colnames(stck3)<-input$symb3
    stck4<-dataInput4()[,4]
    colnames(stck4)<-input$symb4
    stckmatrix<-cbind(stck1,stck2,stck3,stck4)
    covmatrix<-cor(stckmatrix)
    covmatrix
  })
  
  output$covarmatrix_results<-renderPrint({
    matrix1<-CovarMatFunction()
    highvalue<-max(matrix1[matrix1!=max(matrix1)])
    highvaluerounded<-round(highvalue,digit=3)
    paste("The highest corelation value is",highvaluerounded)
      
  })
  
  Chosen_stock_1=reactive({
    matrix1<-CovarMatFunction()
    highvalue<-max(matrix1[matrix1!=max(matrix1)])
    #highvaluerounded<-round(highvalue,digit=3)
    loc<-which(matrix1 == highvalue, arr.ind = TRUE)
    rownames(stckmatrix)[loc[1]]
   # rownames
    })
  
  Chosen_stock_2=reactive({
    matrix1<-CovarMatFunction()
    highvalue<-max(matrix1[matrix1!=max(matrix1)])
    #highvaluerounded<-round(highvalue,digit=3)
    loc<-which(matrix1 == highvalue, arr.ind = TRUE)
    colnames(stckmatrix)[loc[2]]
    #colnames
    })
  
  ChooseStocks1=reactive ({
    matrix1<-CovarMatFunction()
    highvalue<-max(matrix1[matrix1!=max(matrix1)])
    highvaluerounded<-round(highvalue,digit=3)
    loc<-which(matrix1 == highvalue, arr.ind = TRUE)
    aa<-colnames(matrix1)[loc[2]]
    stck_sel1<-as.character(aa)
    })
  
  ChooseStocks2=reactive ({
    matrix1<-CovarMatFunction()
    highvalue<-max(matrix1[matrix1!=max(matrix1)])
    highvaluerounded<-round(highvalue,digit=3)
    loc<-which(matrix1 == highvalue, arr.ind = TRUE)
    bb<-rownames(matrix1)[loc[1]]
    stck_sel1<-as.character(bb)
  })
  
  output$chosenstocks<-renderText({
    stck_sel1<-as.character(ChooseStocks1())
    stck_sel2<-as.character(ChooseStocks2())
    paste("Chosen stocks are",stck_sel1,"and",stck_sel2)
  })
  
  

  output$plot1 <- renderDygraph({
    if(input$get==0)return(NULL) 
    combinedseries<-cbind(Chosen_stock_1_dataseries(),Chosen_stock_2_dataseries())
    head(combinedseries)
    
    chosen_stock1closedata<-Chosen_stock_1_dataseries()[,4]
    chosen_stock1_returns<-diff(log(chosen_stock1closedata))*100
    
    chosen_stock2closedata<-Chosen_stock_2_dataseries()[,4]
    chosen_stock2_returns<-diff(log(chosen_stock2closedata))*100 
    
    combinedseries<-cbind(chosen_stock1_returns,chosen_stock2_returns)
    chosen_stock1_name<-ChooseStocks1()
    chosen_stock2_name<-ChooseStocks2()
    
   colnames(combinedseries)<-c(chosen_stock1_name,chosen_stock2_name)
    
    dygraph(combinedseries, main = "Daily % Stock Returns") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 1) %>%
      dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE) 
      #dySeries("chosen_stock1_name", label = "Daily Returns % is",color = "green")%>%
      #dySeries("chosen_stock1_name", label = "Daily Returns % is",color = "blue")
          
    
   #chartSeries(Chosen_stock_1_dataseries(), theme = chartTheme("white"), 
      #type = "line", TA = NULL,name="Stock values for the selected duration")
     
  })

output$OHLC1<-renderTable({
  if(input$get==0)return(NULL)
  seriesHi(round( Chosen_stock_1_dataseries()))
  })

output$plot2 <- renderPlot({
  if(input$get==0)return(NULL)  
  chartSeries(Chosen_stock_2_dataseries(), theme = chartTheme("white"), 
              type = "line", TA = NULL,name='Stock values for the selected duration')
  
})

output$OHLC2<-renderTable({
  if(input$get==0)return(NULL)
  seriesHi(Chosen_stock_2_dataseries())
})

output$corelation<-renderPrint({
  CorelFunc()
 })

CorelFunc=reactive({
  if(input$get==0)return(NULL) 
  stck1<-(Cl(Chosen_stock_1_dataseries()))
  stck2<-(Cl(Chosen_stock_2_dataseries()))
  cor(stck1,stck2)
  })

output$coreldecision<-renderText({
  if(input$get==0)return(NULL) 
  corvalue=CorelFunc()
  if(corvalue>0.6){
    {"There is a stong Positive corelation since Corelation Coefficient is more than 0.6"}}
    else{
      if((corvalue<0.6)&(corvalue>0))
      {"There is weak positive corelation  since Corelation Coefficient is less than 0.6"}
      else
        {if(corvalue<0){"The two chosen stocks are negatively corelated"}}
      }
    })
  

HedgeFunc=reactive({
  if(input$get==0)return(NULL) 
  model<-lm((diff(Cl(Chosen_stock_1_dataseries()))[-1])~(diff(Cl(Chosen_stock_2_dataseries()))[-1]) -1 )  #Create the linear model. [-1] means model without intercept
  summary(model)
  })

output$HedgeRatio<-renderPrint({
HedgeFunc()
  })

output$HRCoefficient<-renderPrint({
  HedgeFunc()$coefficient[1]
  })

output$HistSpread <- renderPlot({
  if(input$get==0)return(NULL) 
  spreadT<-(Cl(Chosen_stock_1_dataseries()))-(HedgeFunc()$coefficient[1])*(Cl(Chosen_stock_2_dataseries()))
  #compute the stats of the spread
  meanSP<-as.numeric(mean(spreadT,na.rm=TRUE))
  sdSP<-as.numeric(sd(spreadT,na.rm=TRUE))
  upperThr<-meanSP+1*sdSP
  lowerThr<-meanSP-1*sdSP
  #Now we plot the spread with the lower and upper bounds
  plot(spreadT,main="Histogram of the Spread of the selected stocks for chosen time period")
  abline(h=meanSP,col="red",lwd=2)
  abline(h=upperThr,col="blue",lwd=2)
  abline(h=lowerThr,col="blue",lwd=2)
  })

output$DistSpread <- renderPlot({
  if(input$get==0)return(NULL) 
  spreadT<-(Cl(Chosen_stock_1_dataseries()))-(HedgeFunc()$coefficient[1])*(Cl(Chosen_stock_2_dataseries()))
  meanSP<-as.numeric(mean(spreadT,na.rm=TRUE))
  #Plot for the distribution of the spread
  hist(spreadT,col="blue",breaks=100,main="Distribution of the Spread of the selected stocks for chosen time period")
  abline(h=meanSP,col="red",lwd=2)
  
})

output$TradingPoints <-renderPlot({
  spreadT<-(Cl(Chosen_stock_1_dataseries()))-(HedgeFunc()$coefficient[1])*(Cl(Chosen_stock_2_dataseries()))
  meanSP<-as.numeric(mean(spreadT,na.rm=TRUE))
  sdSP<-as.numeric(sd(spreadT,na.rm=TRUE))
  upperThr<-meanSP+1*sdSP
  lowerThr<-meanSP-1*sdSP
  
  toSell<-which(spreadT>meanSP+sdSP)
  toBuy<-which(spreadT>meanSP-sdSP)
  
  spreadL<-length(spreadT)
  pricesBuy<-c(rep(NA,spreadL))
  pricesSell<-c(rep(NA,spreadL))            
  sp<-as.numeric(spreadT)
  tradeQty<-100
  totalP<-0
  
  for(i in 1:spreadL){
    spTemp<-sp[i]
    if(spTemp<=lowerThr){
      if(totalP<=0){
        
        totalP<-totalP+tradeQty
        pricesBuy[i]=spTemp
      }
    }else if(spTemp>=upperThr){
    if(totalP>=0){
      totalP<-totalP-tradeQty
      pricesSell[i]<-spTemp
    }
    }
  }
  
  #Plot for the trading points/prices when the trading occurs
  
  plot(spreadT,main="Spread of selected stocks showing points when trade occurs")
  abline(h=meanSP,col="red",lwd=2)
  abline(h=upperThr,col="blue",lwd=2)
  abline(h=lowerThr,col="blue",lwd=2)
  points(xts(pricesBuy,index(spreadT)),col="green",cex=1.9,pch=19)
  points(xts(pricesSell,index(spreadT)),col="red",cex=1.9,pch=19)
      
  })


})