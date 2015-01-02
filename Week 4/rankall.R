rankall <- function(outcome, num = "best") {
  
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  outcomes <- c("heart failure", "heart attack", "pneumonia")
  
  if(!outcome %in% outcomes){
    stop("invalid outcome")
  }
  
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid number")
    }
  }
  
  if(outcome == "heart attack") {
    col_nos <- c(1,2,3)
  } else if(outcome == "heart failure") {
    col_nos <- c(1,2,4)
  } else if(outcome == "pneumonia") {
    col_nos <- c(1,2,5)
  } 
  
  file <- file[, col_nos]
  
  names(file)[3] = "Deaths"
  file[, 3] = suppressWarnings( as.numeric(file[, 3]) )
  
  file = file[!is.na(file$Deaths),]
  
  splited = split(file, file$State)
  ans = lapply(splited, function(x, num) {
    # Order by Deaths and then HospitalName
    x = x[order(x$Deaths, x$Hospital.Name),]
    
    # Return
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }, num)
  
  #Return data.frame with format
  return ( data.frame(hospital=unlist(ans), state=names(ans)) )
  
  
}