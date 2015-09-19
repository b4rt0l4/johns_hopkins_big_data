pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  clean_data <- c()
    
  for (i in id) {
    file <- paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep="")  
    data <- read.csv(file=file, header=T, sep=",")
    
    clean_data <- c(clean_data, data[!is.na(data[,pollutant]), pollutant])
  }
  
  result <- mean (clean_data)
  result
}