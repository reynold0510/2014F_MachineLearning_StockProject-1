CleanStockData <- function() {
  # Read and clean the original data and save the clean data.
  #
  # Args: None.  
  #
  # Returns: The clean stock data.
  #   
  # author: PENG GONG, Carnegie Mellon University, Fall 2014.

  # clear workspace
  work.space <- ls()
  rm(work.space)
  setwd("F:/Classes/2014F MLSP/Stock Project")
  
  # years of data in the model
  years <- 3
  
  # read CSV into R
  stock.data <- read.csv(file = "F:/Classes/2014F MLSP/Stock Project/00ded53a096186c5.csv", header=TRUE, sep=",")
  
  # check the fields of data and number of rows
  print(names(stock.data))
  print(nrow(stock.data))
  
  # Extract the useful data field:
  #   Ticker Symbol, Company Name, Data Year and Quarter Calendar,Data Year and Quarter Fiscal,
  #   Revenue Total, Net Income, Assets Total, Liability Total, 
  #   Operating Activities - Net Cash Flow, Working Capital to Fiscal Year, Capital Expenditures to Fiscal Year
  #   Dividends per Share, Price Close Quarter, Common Shares Outstanding 
  field.name <- c("tic", "conm", "datacqtr", "datafqtr",
            "revtq", "niq", "atq", "ltq",
            "oancfy", "wcapq", "capxy",
            "dvpspq", "prccq", "cshoq")  
  stock.data <- stock.data[field.name]
  
  # extract the complete cases
  stock.data <- stock.data[complete.cases(stock.data), ]
  write.csv(stock.data, file = "F:/Classes/2014F MLSP/Stock Project/SP500.csv")
  
  # return the clearn data
  return(stock.data)
}
