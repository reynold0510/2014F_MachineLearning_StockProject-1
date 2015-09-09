GetPreviousQuarters <- function(current.quarter,years) {
  # Get the previous quarters from the current quarter and the length of years
  #
  # Args: 
  #   current.quarter: the current year and quarter in the format like "2014Q3"
  #   years: how many years you want to go back to get the quarters
  #
  # Returns: 
  #   previous.quarters: a vector of the previous year and quarters 
  # in the format like c("2013Q4", "2014Q1", "2014Q2", "2014Q3") 
  #
  # author: PENG GONG, Carnegie Mellon University, Fall 2014.
  
  previous.quarters = current.quarter
  
  for (ii in 1:(4 * years - 1)) {
    previous.quarter = GetPreviousQuarter(current.quarter)
    previous.quarters = c(previous.quarter, previous.quarters)
    current.quarter = previous.quarter
  }
  
  return(previous.quarters)
}