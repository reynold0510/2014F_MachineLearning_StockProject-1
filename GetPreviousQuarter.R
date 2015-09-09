GetPreviousQuarter <- function(current.quarter) {
  # Get the previous quarter from the current quarter
  #
  # Args: 
  #   current.quarter: the current year and quarter in the format like "2014Q3"
  #
  # Returns: 
  #   previous.quarter: the previous year and quarter in the format like "2014Q3"
  #
  # author: PENG GONG, Carnegie Mellon University, Fall 2014.
  
  if (typeof(current.quarter) != "character") {
    stop("Wrong Type of Inputs!")
  }
  
  if (nchar(current.quarter) != 6) {
    stop("Wrong Length of String!")
  }
  
  year <- as.numeric(substr(current.quarter,1,4))
  quarter <- as.numeric(substr(current.quarter,6,6))

  if (quarter == 1) {
    quarter.new <- 4
    year.new <- year - 1
  } else {
    quarter.new <- quarter - 1
    year.new <- year
  }
  
  quarter.new <- toString(quarter.new)
  year.new <- toString(year.new)
  
  previous.quarter <- paste(year.new, "Q", quarter.new, sep = "")
  return(previous.quarter)
}

