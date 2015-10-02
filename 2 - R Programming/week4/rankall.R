rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
  
  ## Check that state and outcome are valid
  if(outcome == 'heart attack') {
    index <- 11
  } else if(outcome == 'heart failure') {
    index <- 17
  } else if(outcome == 'pneumonia') {
    index <- 23
  } else {
    stop('invalid outcome')
  }
  
  data[,index] <- suppressWarnings(as.numeric(data[,index]))
  data <- data[!is.na(data[,index]),]
  
  
  # sort data by state name, then outcome, then hospital name
  data <- data[order(data$State, data[index], data$Hospital.Name),]
  
  # aggregate by state, choosing the row that corresponds to the rank num
  ranksbystate <- aggregate(data, by=list(data$State), function(x) {
    if (!is.numeric(num)) {
      if (num == "best") {
        num <- 1
      } else if (num == "worst") {
        num <- length(x)
      } else {
        stop("invalid num")
      } 
    }
    x[num]
  })
  
  # get just the columns we need and rename them
  out <- ranksbystate[,c(3,1)]
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  names(out) <- c("hospital","state")
  return(out)

}