pollutantmean <- function(directory, pollutant, id = 1:332) {
  pollution <- matrix(nrow=length(id), ncol=2)
  index <- 1
  for(i in id) {
    ## Read the file
    fileName <- paste(directory, "/", sprintf("%03d", i), ".csv", sep="")
    dataFrame <- read.csv(fileName)
    
    ## Calculate the sum and count of pollution for that file
    pollutionData <- dataFrame[[pollutant]]
    pollutionData <- pollutionData[!is.na(pollutionData)]
    pollutionSum <- sum(pollutionData, na.rm=TRUE)
    pollution[index,] <- c(pollutionSum, length(pollutionData))
    index <- index + 1
  }
  ##Caluculate the mean over all files
  totalPollution <- sum(pollution[,1], na.rm=TRUE)
  totalCount <- sum(pollution[,2], na.rm=TRUE)
  meanPollution <- totalPollution / totalCount
  round(meanPollution, digits=3)
}