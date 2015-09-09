ExploreStockData = function() {
  # Explore the stock data (correlation coeff., histograms, scatter plots)
  #
  # Args: None.  
  #
  # Returns: None.
  #   
  # author: PENG GONG, Carnegie Mellon University, Fall 2014.
    
  # clear objects
  rm(list=ls())
  setwd("F:/Classes/2014F MLSP/Stock Project")
  
  # read the output of GetPredictors() function 
  stock.data <- read.csv(file = "F:/Classes/2014F MLSP/Stock Project/SP500_v1.csv", header = TRUE, sep = ",")
  print(names(stock.data))

  # select fields:
  #   Market Cap,
  #   Revenue Total (this year), Revenue Total (last year), Revenue Total (last last year)
  #   Net Income (this year), Net Income (last year), Net Income (last last year),
  #   Dividends per Share (this year), Dividends per Share (last year), Dividends per Share (last last year)
  #   Assets Total, Liability Total, 
  #   Operating Activities - Net Cash Flow, Working Capital to Fiscal Year, Capital Expenditures to Fiscal Year
  field =  c("mktcp", 
             "revtq", "revtq1", "revtq2",
             "niq", "niq1", "niq2",
             "dvpspq", "dvpspq1", "dvpspq2",
             "atq", "ltq",
             "oancfy", "wcapq", "capxy")  
  stock.data = stock.data[field]
  
  # correlation coefficient matrix
  stock.corr = cor(stock.data)
  library(corrplot)
  corrplot(stock.corr, method = "circle", mar = c(0, 0, 0, 0))
  corrplot(stock.corr, method = "pie", mar = c(0, 0, 0, 0))
  corrplot(stock.corr, method = "number", mar = c(0, 0, 0, 0))
  
  # Histogram
  library(reshape2)
  library(ggplot2)
  stock.data.melt <- melt(stock.data)
  ggplot(stock.data.melt,aes(x = value)) + 
    facet_wrap(~variable,scales = "free_x") + 
    geom_histogram()
  
  # scatter plots
  par(mar = c(0, 0, 0, 0))
  plot(stock.data[c("mktcp", "revtq", "revtq1", "revtq2", "niq", "niq1", "niq2")], 
       main = "Scatter Plots of Stock Data, Part 1")
  plot(stock.data[, c("mktcp", "niq", "niq1", "niq2", "dvpspq", "dvpspq1", "dvpspq2")], 
       main = "Scatter Plots of Stock Data, Part 2")
  plot(stock.data[, c("mktcp", "atq", "ltq", "oancfy", "wcapq", "capxy")], 
       main = "Scatter Plots of Stock Data, Part 3")
}
  