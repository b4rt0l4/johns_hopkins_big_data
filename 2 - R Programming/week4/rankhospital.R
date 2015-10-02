
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
  ## Check that state and outcome are valid
  if(!any(state == data$State)) {
    stop('invalid state')
  }
  
  if(outcome == 'heart attack') {
    index <- 11
  } else if(outcome == 'heart failure') {
    index <- 17
  } else if(outcome == 'pneumonia') {
    index <- 23
  } else {
    stop('invalid outcome')
  }
  ## Return hospital name in that state with the given rank
  data[,index] <- suppressWarnings(as.numeric(data[,index]))
  data <- na.omit(data)
  
  state_data <- subset(data, State==state)
  state_data <- state_data[order(state_data[,index], state_data[,2], na.last=TRUE),2]
  state_data <- na.omit(state_data)
  
  if (num == "best") {
    num <-1
  } else if (num == "worst") {
    num <- length(state_data)
  } else if (is.numeric(num)) {
    num <- as.numeric(num)
  } else {
    stop('invalid num')
  }

  ## 30-day death rate
  state_data[num]
}