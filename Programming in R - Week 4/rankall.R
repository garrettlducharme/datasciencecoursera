#Returns a data frame containing the hospital with a given rank for each state.
#This data frame also includes DC and the territories GU, MP, PR, and VI. 

#Allowed outcomes are "heart attack", "heart filure", and "pneumonia".
#Hospitals that do not have data for a given outcome will not be included in the
#ranking.
#num may be supplied the characters "best", "worst", or an integer. Integers
#that are larger than the number of hospitals with available data for an outcome
#and state will have a value of NA in the data frame.
#Ties for hospitals with the same rank are decided by choosing the hospital
#that comes first alphabetically.
rankall <- function(outcome, num = "best"){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
    if(outcome == "heart attack") {
      ind <- 11
    }
    else if(outcome == "heart failure") {
      ind <- 17
    }
    else if(outcome == "pneumonia") {
      ind <- 23
    }
    else {stop("invalid outcome")}
    
    data <- data[!(data[,ind] == "Not Available"),]
    sorted_data <- data[order(data$State, as.numeric(data[,ind]), 
                              data$Hospital.Name),]
    
    hospital <- character(51)
    state <- c(state.abb, "DC", "GU", "MP", "PR", "VI")
    state <- state[order(state)]
    
    if(num == "best"){
        for(i in 1:length(state)){
            hospital[i] <- sorted_data[sorted_data$State == state[i],
                                       "Hospital.Name"][1]
        }
    }
    else if(num == "worst"){
        for(i in 1:length(state)){
            last = nrow(sorted_data[sorted_data$State == state[i],])
            
            hospital[i] <- sorted_data[sorted_data$State == state[i],
                                       "Hospital.Name"][last]
        }
    }
    else{
        for(i in 1:length(state)){
            hospital[i] <- sorted_data[sorted_data$State == state[i],
                                       "Hospital.Name"][num]
        }
    }
    
    return(data.frame(hospital, state))
}