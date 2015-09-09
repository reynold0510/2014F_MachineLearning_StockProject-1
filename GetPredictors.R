GetPredictors <- function() {
  # Calculate the predictor variables and the reponse variable, and save them into a csv file
  #
  # Args: None.  
  #
  # Returns: None.
  #   
  # author: PENG GONG, Carnegie Mellon University, Fall 2014.
  
  # load functions
  source("F:/Classes/2014F MLSP/Stock Project/CleanStockData.R")
  source("F:/Classes/2014F MLSP/Stock Project/GetPreviousQuarter.R") 
  source("F:/Classes/2014F MLSP/Stock Project/GetPreviousQuarters.R") 
  stock.data <- CleanStockData()
  
  # Part 1: calculate the working captical and captical expenditure of a quarter   
  tickers <- unique(stock.data$tic)
  for (ii in 1:length(tickers)) {
    com.data <- stock.data[stock.data$tic == tickers[ii], ]
    com.nrow <- nrow(com.data)
    for (jj in com.nrow:1) {
      fiscal.quarter <- toString(com.data[jj, "datafqtr"])
      quarter <- as.numeric(substr(fiscal.quarter, 6, 6))
      
      if (quarter != 1) {
        previous.fiscal.quarter <- GetPreviousQuarter(fiscal.quarter)
        com.record <- com.data[com.data[, "datafqtr"] == previous.fiscal.quarter, ]
        
        if (nrow(com.record) == 0){
          com.data[jj, "oancfy"] <- NA
          com.data[jj, "capxy"] <- NA
        } else {
          com.data[jj, "oancfy"] <- com.data[jj, "oancfy"] - com.data[jj-1, "oancfy"]
          com.data[jj, "capxy"] <- com.data[jj, "capxy"] - com.data[jj-1, "capxy"]
        }
      }   
    }
    stock.data[stock.data$tic == tickers[ii], ] <- com.data
  }
  
  # Part 2: calculate the free cash flow  
  # fcfq <- stock.data[, "oancfy"] - stock.data[, "wcapq"] - stock.data[, "capxy"]
  # stock.data["fcfq"] <- fcfq

  # Part 3: calculate the three years' revenues, net incomes, dividends 
  years <- 3
  stock.nrow <- nrow(stock.data)
  
  revtq1 <- rep(NA, stock.nrow)
  revtq2 <- rep(NA, stock.nrow)
  niq1 <- rep(NA, stock.nrow)
  niq2 <- rep(NA, stock.nrow)
  dvpspq1 <- rep(NA, stock.nrow)
  dvpspq2 <- rep(NA, stock.nrow)  
  
  new.field.name <- c("revtq1", "revtq2",
               "niq1", "niq2",
               "dvpspq1", "dvpspq2")
  new.field <- list(revtq1, revtq2,
                 niq1, niq2,
                 dvpspq1, dvpspq2)
  stock.data[new.field.name] <- new.field
  
  tickers <- unique(stock.data$tic) 
  for (ii in 1:length(tickers)) {
    com.data <- stock.data[stock.data$tic == tickers[ii], ]
    com.nrow <- nrow(com.data)
    com.qtr <- com.data$datafqtr
    for (jj in com.nrow:1) {
      fiscal.quarter <- toString(com.data$datafqtr[jj])
      previous.fiscal.quarters <- GetPreviousQuarters(fiscal.quarter, years)
      ind <- match(previous.fiscal.quarters, com.qtr)
      ind.sum <- sum(is.na(ind)) 
      
      if (ind.sum == 0) {
        com.quarter.data <- com.data[ind, ]
        com.data[jj, "revtq"]  <- sum(com.quarter.data[9:12, "revtq"])
        com.data[jj, "revtq1"] <- sum(com.quarter.data[5:8, "revtq"])
        com.data[jj, "revtq2"] <- sum(com.quarter.data[1:4, "revtq"])
      
        com.data[jj, "niq"]  <- sum(com.quarter.data[9:12, "niq"])
        com.data[jj, "niq1"] <- sum(com.quarter.data[5:8, "niq"])
        com.data[jj, "niq2"] <- sum(com.quarter.data[1:4, "niq"])
       
        com.data[jj, "dvpspq"]  <- sum(com.quarter.data[9:12, "dvpspq"] * com.quarter.data[9:12, "cshoq"])
        com.data[jj, "dvpspq1"] <- sum(com.quarter.data[5:8, "dvpspq"] * com.quarter.data[5:8, "cshoq"])
        com.data[jj, "dvpspq2"] <- sum(com.quarter.data[1:4, "dvpspq"] * com.quarter.data[1:4, "cshoq"])        
      }      
    } 
    
    stock.data[stock.data$tic == tickers[ii], ] <- com.data
  }

  # Part 5: Calculatet the market cap
  mktcp <- stock.data[, "prccq"] * stock.data[, "cshoq"]
  stock.data["mktcp"] <- mktcp
  
  # reorganize the fields
  field <-  c("tic", "conm", "datacqtr", "datafqtr",
              "revtq", "revtq1", "revtq2",
              "niq", "niq1", "niq2",
              "dvpspq", "dvpspq1", "dvpspq2",
              "atq", "ltq",
              "oancfy", "wcapq", "capxy",
              "mktcp")
  stock.data <- stock.data[field]  
  print(nrow(stock.data))
  stock.data <- stock.data[complete.cases(stock.data), ]
  print(nrow(stock.data))
  stock.data <- stock.data[stock.data$datacqtr != "", ]
  print(nrow(stock.data))
  write.csv(stock.data, file = "F:/Classes/2014F MLSP/Stock Project/SP500_v1.csv")

  print("The feature extraction work is done!")
}