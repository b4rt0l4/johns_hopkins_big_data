corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  files <- as.character( list.files(directory) )
  correlations <- numeric(0)
  index <- 0
  for (i in files) {
    file <- paste(directory, "/", i, sep="") 
    data <- read.csv(file=file, header=T, sep=",")
    complete_data_cases <- complete.cases(data)
    
     if (sum(complete_data_cases) >= threshold) {
       index <- index + 1
       correlations[index] <- cor(data[complete_data_cases, "sulfate"], data[complete_data_cases, "nitrate"])
     }
  }
  correlations
}