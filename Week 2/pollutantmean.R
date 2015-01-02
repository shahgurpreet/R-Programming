pollutantmean <- function(directory, pollutant, id = 1:332){
  
  values <- c();
  for (index in id){
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
    polFile <- file[pollutant]
    polFile <- polFile[!is.na(polFile)]
    
    for (p in polFile){
      values <- c(values, p)
    }
    
  }
  
  mean(values)
}


