complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  complete_data_cases <- rep(0, length(id))
  index <- 0
  
  for (i in id) {
    file <- paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep="") 
    data <- read.csv(file=file, header=T, sep=",")
    index <- index + 1
    complete_data_cases[index] <- sum(complete.cases(data))
  }
  
  result <- data.frame(id = id, nobs = complete_data_cases)
  result
}