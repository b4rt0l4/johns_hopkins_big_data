
best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
  ## Check that state and outcome are valid
  if(!any(state == data$State)) {stop('invalid state')}
  
  if(outcome == 'heart attack') {
    index <- 11
  } else if(outcome == 'heart failure') {
    index <- 17
  } else if(outcome == 'pneumonia') {
    index <- 23
  } else {
    stop('invalid outcome')
  }
  ## Return hospital name in that state with lowest 30-day death
  data[,index] <- suppressWarnings(as.numeric(data[,index]))
  data <- na.omit(data)
  
  state_data <- subset(data, State==state)
  state_data <- state_data[order(state_data[,index], na.last=TRUE),2]
  state_data <- na.omit(state_data)
  
  ## rate
  state_data[1]
}
