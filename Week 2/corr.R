corr <- function(directory, threshold = 0) {
  
  res <- c()
  
  for (index in 1:332){
    
    if(index<10){
      fileName <- paste("00", index, ".csv", sep="")
    }
    else if(index<=99){
      fileName <- paste("0", index, ".csv", sep="")
    }
    else {
      fileName <- paste(index, ".csv", sep="")
    }
    
    fileName <- paste(directory,"/",fileName, sep="")
    file <- read.csv(file=fileName)
    
    dFrame <- file[complete.cases(file),]
    rows <- nrow(dFrame)
    
    
    if (rows > threshold){
      res1 <- cor(dFrame["sulfate"], dFrame["nitrate"])
      res <- c(res,res1)
    }
  }
  return(res)
}
