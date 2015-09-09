# 2014F_MachineLearning_StockProject-1
Model stock evaluation using financial fundamentals
The raw data are the financial fundamentals for S&P 500 companies from Wharton Research Data Services (WRDS). The raw data are not included in this repo because I am not authorized to publish the data. 

Function descriptions:
(1) CleanStockData(): Clean the raw data from Wharton Research Data Services (WRDS). 
(2) GetPredictors(): Preprocess the data and extract the features.
(3) ExploreStockData(): Exploratory analysis of the stock data; Visualize some patterns in the data.
(4) ModelStock_LinearRegression: A linear model to find the stock market trend and find the undervalued stocks.
(5) ModelStock_SVDRegression: A principle component regression model to find the stock market trend and find the undervalued stocks.
(6) GetPreviousQuarter(): Get the previous quarter given the current quarter
(7) GetPreviousQuarters(): Get the previous quarters given the current quarter and how many years you want to date back.
