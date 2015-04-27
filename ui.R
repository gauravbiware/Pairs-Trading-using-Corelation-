library(shiny)
library(dygraphs)

shinyUI(fluidPage(
  titlePanel("Pairs Trading Strategy for 4 Stocks"),
  
  sidebarLayout(
    sidebarPanel(
        
      radioButtons("radio",label="Select Source of Financial data",
                   c("Yahoo Finance"="yahoo","Google Finance"="google"),selected="yahoo"),
                   
      textInput("symb1", "Select 1st Stock using its Stock Symbol", "SUN"),
      br(),
      textInput("symb2", "Select 2nd Stock using its Stock Symbol", "GOOG"),
    
      textInput("symb3", "Select 3rd  Stock using its Stock Symbol", "AAPL"),
      br(),
      textInput("symb4", "Select 4th  Stock using its Stock Symbol", "MSFT"),
      
      dateRangeInput("dates", 
        "Date range",
        start = "2015-01-01", 
        end = as.character(Sys.Date())),
   
      actionButton("get", "Get Stocks"),
      
      br(),
      br()
    ),
     #checkboxInput("log", "Plot y axis on log scale", value = FALSE)),
      
   #   checkboxInput("adjust", 
    #    "Adjust prices for inflation", value = FALSE)
    #),
    
    mainPanel(
      conditionalPanel(
        condition="input.get!='None'",
        tabsetPanel(
          
          tabPanel("Corelation Analysis",
                   helpText("Corelation Analysis for the chosen stocks"),
                   tableOutput("covarmatrix"),
                   verbatimTextOutput("covarmatrix_results"),
                   verbatimTextOutput("chosenstocks"),
                   helpText("Proceed to the next tab to see plot of daily returns for these two stocks")
                  ),
            
          
          tabPanel("Chosen Stocks",
                   #helpText("Below table provides the High, Low, open & Close Values for the selected dates"),
                  # tableOutput("OHLC1"),
                   helpText("Below plot shows daily % returns for the chosen stocks"),
                  dygraphOutput("plot1"),
                  helpText("In the next tab, we find the spread, hedge Ratio and characteristics of the spread")
                  ),                   
                 
        #  tabPanel("2nd Stock Analysis",
                 #  helpText("Below table provides the High, Low, open & Close Values for the selected dates"),
                  # tableOutput("OHLC2"),
                  # helpText("Plot of the stock Values for 2nd co is below"),
                  # plotOutput("plot2")
                 #  ),
                    
          tabPanel("Spread Analysis",
                   h4("SPREAD"),
                   helpText("Spread between the two stocks is defined as\n"),
                   code("Closing Value of 1st stock - Closing value of 2nd stock"),
                   br(),
                   br(),
                   helpText("For pairs trading strategy, we derive the regression of 
                            closing values the stocks on one another"),
                   helpText("Below is the output of this regression"),
                   verbatimTextOutput("HedgeRatio"),
                   br(),
                   helpText("From the regression output we can see that the Hedge Ratio OR 
                            the regression coefficent is "),
                   verbatimTextOutput("HRCoefficient"),
                   br(),
                   helpText("Histogram of the spread"),
                   plotOutput("HistSpread"),
                   br(),
                   helpText("We can also see the distribution of this spread"),
                   plotOutput("DistSpread"),
                   br()
                   ),
          
          tabPanel("Trading",
                   h4("Trading Principles"),
                   helpText("Based on this spread, we can derive the following trading rules\n"),
                   code("SELL Stock 1 if the spread breaches UPPER threshold, where UPPER threshold
                        = Mean of spread + 1 Standard deviation of spread"),
                   br(),
                   br(),
                   code("BUY Stock 1 if the spread breaches LOWER threshold, where LOWER threshold
                        = Mean of spread - 1 Standard deviation of spread"),
                   br(),
                   plotOutput("TradingPoints")
                   )
      ))
  ))
))