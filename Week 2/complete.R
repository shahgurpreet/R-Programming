complete <- function(directory, id) {
  head <- c("##"," ", "id", "nobs")
  data <- c();
  df <- data.frame(head)
  j = 1
  for (index in id){
    count = 0
    fileIndex <- 1
    f <- c(index)
    if(f<10){
      fileName <- paste("00", f, ".csv", sep="")
    }
    else if(f<=99){
      fileName <- paste("0", f, ".csv", sep="")
    }
    else {
      fileName <- paste(f, ".csv", sep="")
    }
    
    fileName <- paste(directory,"/",fileName, sep="")
    file <- read.csv(file=fileName)
    data[j] <- sum(complete.cases(file))
    j <- j+1
  }
  
  res <- data.frame(id = id, nobs = data)
  return(res)
}