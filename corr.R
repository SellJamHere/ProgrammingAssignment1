corr <- function(directory, threshold = 0) {
  correlations <- numeric()
  
  id <- 1:332
  for(i in id) {
    ## Read the file
    fileName <- paste(directory, "/", sprintf("%03d", i), ".csv", sep="")
    dataFrame <- read.csv(fileName)
    
    ##Count the number of complete cases
    completeCases <- dataFrame[complete.cases(dataFrame),]
    completeCaseCount <- nrow(completeCases)
    if(completeCaseCount > threshold) {
      sulfates <- completeCases[["sulfate"]]
      nitrates <- completeCases[["nitrate"]]
      correlations <- c(correlations, cor(sulfates, nitrates))
    }
  }
  
  ## Return a numeric vector of correlations
  correlations
}