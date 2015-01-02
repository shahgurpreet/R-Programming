
best <- function(state, outcome) {

  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  data[, 11] <- as.numeric(data[, 11]) # heart attack
  data[, 17] <- as.numeric(data[, 17]) # heart failure
  data[, 23] <- as.numeric(data[, 23]) # pneumonia
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
    stop("invalid state")
  } else if(!outcome %in% outcomes) {
    stop("invalid outcome")
  } else {
    if(outcome == "heart attack") {
      states <- data[data[, 7]==state, ]
      outcomes <- states[, 11]
      min <- min(outcomes, na.rm=T)
      min_index <- which(outcomes == min)
      hospital_name <- states[min_index, 2]
      } 
      else if(outcome == "heart failure") {
        states <- data[data[, 7]==state, ]
        outcomes <- states[, 17]
        min <- min(outcomes, na.rm=T)
        min_index <- which(outcomes == min)
        hospital_name <- states[min_index, 2]
    } else {
      states <- data[data[, 7]==state, ]
      outcomes <- states[, 23]
      min <- min(outcomes, na.rm=T)
      min_index <- which(outcomes == min)
      hospital_name <- states[min_index, 2]
    }
    result <- hospital_name
    return(result)
  }
}
