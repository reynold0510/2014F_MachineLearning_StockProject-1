  # Modeling market caps of companies using the financial fundamentals as predictors, this function
  # uses the output from GetPredictors() function
  #  
  # author: PENG GONG, Carnegie Mellon University, Fall 2014.
  
  # clear objects
  rm(list = ls())
  setwd("F:/Classes/2014F MLSP/Stock Project")
  source("F:/Classes/2014F MLSP/Stock Project/GetPreviousQuarter.R") 
  source("F:/Classes/2014F MLSP/Stock Project/GetPreviousQuarters.R")
 
  # read the output of GetPredictors() function 
  stock.data <- read.csv(file = "F:/Classes/2014F MLSP/Stock Project/SP500_v1.csv", header = TRUE, sep = ",")
  print(names(stock.data))


  # split data into Train and Test datasets
  predictTime <- "2014Q3"
  years <- 2
  Train <- stock.data[!(stock.data$datacqtr %in% GetPreviousQuarters(predictTime, years)), ]
  Test <- stock.data[stock.data$datacqtr %in% GetPreviousQuarters(predictTime, years), ]
  head(Train)
  # SVD
  field <-  c("revtq", "revtq1", "revtq2",
             "niq", "niq1", "niq2",
             "dvpspq", "dvpspq1", "dvpspq2",
             "atq", "ltq",
             "oancfy", "wcapq", "capxy")
  svd.Train <- Train[field]
  svd.Test <- Train[field]
  
  svd.Train <- prcomp(svd.Train, retx = TRUE)
  svd.Test <- predict(svd.Train, svd.Test)
  
  Train[field] <- data.frame(svd.Train$x)
  Test[field] <- data.frame(svd.Test)

  # linear model  
  linear.model <- lm(mktcp ~ revtq + revtq1 + revtq2 +
                   niq + niq1 + niq2 +
                   dvpspq + dvpspq1 + dvpspq2 +                 
                   datacqtr, Train)
  # list the regression coefficients  
  smm <- summary(linear.model)
  print(smm)
 
  # visualize the std. errs of coeff.
  linear.coeff <- coef(smm)
  coeff.errs <- linear.coeff[, 2]
  plot(coeff.errs)
  
  # visualize the market trend
  market.trend <- linear.coeff[7:nrow(linear.coeff), 1]
  barplot(market.trend-mean(market.trend))
   
  # performance valuation using Test data
  linear.resid <- linear.model$residuals
  ref.time <- "2012Q1"
  com.time <- "2014Q1"
  
  ref.ind <- Train$datacqtr == ref.time
  ref.Train <- Train[ref.ind, ]
  ref.resid <- linear.resid[ref.ind]
  
  com.ind <- Test$datacqtr == com.time
  com.Test <- Test[com.ind, ]
  
  top.profit <- c(0,0)
  bottom.profit <- c(0,0)
  
  for (i in 1:nrow(ref.Train)) {
    if (ref.Train$tic[i] %in% com.Test$tic) {
      if (ref.resid[i] < 0) {
        per <- (com.Test[com.Test$tic == ref.Train$tic[i], "mktcp"] - ref.Train[i, "mktcp"]) / ref.Train[i, "mktcp"]
        top.profit[1] <- top.profit[1] + per
        top.profit[2] <- top.profit[2] + 1
      } else {
        per <- (com.Test[com.Test$tic == ref.Train$tic[i], "mktcp"] - ref.Train[i, "mktcp"]) / ref.Train[i, "mktcp"]
        bottom.profit[1] <- bottom.profit[1] + per
        bottom.profit[2] <- bottom.profit[2] + 1
      }
    }
  }
  
  print("The profit if we buy the top picks:")
  if (top.profit[1] != 0) {
    print(top.profit[1]/top.profit[2])
  } else {
    print("No top picks available")
  }
  
  print("The profit if we buy the bottom picks:")
  if (bottom.profit[1] != 0) {
    print(bottom.profit[1]/bottom.profit[2])
  } else {
    print("No bottom picks available")
  }
  # regression coefficient transform
  svd.linear.slope <- svd.Train$rotation[, 1:9] %*% linear.coeff[2:10, 1]
  print(svd.linear.slope)   
 