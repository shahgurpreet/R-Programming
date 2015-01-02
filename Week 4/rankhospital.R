rankhospital <- function(state, outcome, num = "best") {
  
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcomes <- c("heart failure", "heart attack", "pneumonia")
  
  if(!outcome %in% outcomes){
    stop("invalid outcome")
  }
  
  
  if(outcome == "heart attack") {
    col_nos <- c(7, 11, 2)
  } else if(outcome == "heart failure") {
    col_nos <- c(7, 17, 2)
  } else if(outcome == "pneumonia") {
    col_nos <- c(7, 23, 2)
  } 
  
  file <- file[, col_nos]
  res   <- file[, 1] == state
  if(sum(res) == 0) {
    stop("invalid state")
  }
  names(file) <- c("State", "Rate", "HospitalName")
  file <- file[res, ]
  file[, 2] <- as.numeric(file[, 2])
  file <- na.omit(file)
  file <- file[order(file$Rate, file$HospitalName), ]
  if(num == "best") {
    num <- 1
  } else if(num == "worst") {
    num <- nrow(file) 
  }
  file[num, 3]
}