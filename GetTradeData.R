# GetTradeData.R
library(quantmod)
library(dygraphs)
library(shiny)

GETSYMBOl1<- function(symbol,src,fromdate,todate)
  {getSymbols(symbol,src=src,from=fromdate,to=todate,auto.assign=FALSE)}

GETSYMBOl2<- function(symbol,src,fromdate,todate)
{getSymbols(symbol,src=src,from=fromdate,to=todate,auto.assign=FALSE)}

GETSYMBOl3<- function(symbol,src,fromdate,todate)
{getSymbols(symbol,src=src,from=fromdate,to=todate,auto.assign=FALSE)}

GETSYMBOl4<- function(symbol,src,fromdate,todate)
{getSymbols(symbol,src=src,from=fromdate,to=todate,auto.assign=FALSE)}

