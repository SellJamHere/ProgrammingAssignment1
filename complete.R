complete <- function(directory, id = 1:332) {
  nobs <- numeric(length(id))
  completeFrame <- data.frame(id, nobs)
  
  index <- 1
  for(i in id) {
    ## Read the file
    fileName <- paste(directory, "/", sprintf("%03d", i), ".csv", sep="")
    dataFrame <- read.csv(fileName)
    
    ##Count the number of complete cases
    completeCases <- dataFrame[complete.cases(dataFrame),]
    completeFrame[index,] <- c(i, nrow(completeCases))
    
    index <- index + 1
  }
  #Return complete cases
  completeFrame
}