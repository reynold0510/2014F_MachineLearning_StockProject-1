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
  predictTime = "2014Q3"
  years <- 2
  Train <- stock.data[!(stock.data$datacqtr %in% GetPreviousQuarters(predictTime, years)), ]
  Test <- stock.data[stock.data$datacqtr %in% GetPreviousQuarters(predictTime, years), ]

  # Feature list:
  #   Ticker, Company Name, Calendar year and quarter, Fiscal year and quarter
  #   Market Cap,
  #   Revenue Total (this year), Revenue Total (last year), Revenue Total (last last year)
  #   Net Income (this year), Net Income (last year), Net Income (last last year),
  #   Dividends per Share (this year), Dividends per Share (last year), Dividends per Share (last last year)
  #   Assets Total, Liability Total, 
  #   Operating Activities - Net Cash Flow, Working Capital to Fiscal Year, Capital Expenditures to Fiscal Year

  # Feature list:  
  #   field <-  c("tic", "conm", "datacqtr", "datafqtr",
  #              "mktcp",
  #              "revtq", "revtq1", "revtq2",
  #              "niq", "niq1", "niq2",
  #              "dvpspq", "dvpspq1", "dvpspq2",
  #              "atq", "ltq",
  #              "oancfy", "wcapq", "capxy")
  
  ## Part 1: linear model  
  linear.model <- lm(mktcp ~ revtq + revtq1 + revtq2 +
                   niq + niq1 + niq2 +
                   dvpspq + dvpspq1 + dvpspq2 +
                   atq + ltq + oancfy + wcapq + capxy +
                   datacqtr, Train)
  # list the regression coefficients  
  smm <- summary(linear.model)
  smm
  
  # diagnosis  
  plot(linear.model, which = c(1:5), 
       caption = list("Residuals vs Fitted", "Normal Q-Q",
                      "Scale-Location", "Cook's distance",
                      "Residuals vs Leverage"),
      id.n = 5, labels.id = names(residuals(linear.model)), cex.id = 0.75,
      label.pos = c(4,2),cex.caption = 1) 
 
  # visualize the std. errs of coeff.
  model.cff <- coef(smm)
  coeff.errs <- model.cff[, 2]
  plot(coeff.errs)
  
  # visualize the market trend
  market.trend <- model.cff[16:nrow(model.cff), 1]
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
  
  
  ## Part 2: SVD + linear model
  
  
   