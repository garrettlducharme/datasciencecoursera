#Returns the hospital with a given rank for a state and outcome. The best
#hospital is the one with the lowest mortality rate for a given outcome,
#while the worst hospital is the one with the highest mortality rate.

#Allowed outcomes are "heart attack", "heart filure", and "pneumonia".
#Allowed states are the abbreviations for the fifty US states, as well as
#"DC", "GU", "MP", "PR", and "VI".
#Hospitals that do not have data for a given outcome will not be included in the
#ranking.
#num may be supplied the characters "best", "worst", or an integer. Integers
#that are larger than the number of hospitals with available data for an outcome
#and state will cause the function to return NA.
#Ties for hospitals with the same rank are decided by choosing the hospital
#that comes first alphabetically.

rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    states <- c(state.abb, "DC", "GU", "MP", "PR", "VI")
    
    if(state %in% states == FALSE) {stop("invalid state")}
    
    red_data <- data[data$State == state,]
    
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
    
    red_data <- red_data[!(red_data[,ind] == "Not Available"),]
    red_data <- red_data[order(as.numeric(red_data[,ind]),
                               red_data$Hospital.Name),]
    
    
    if(num == "best"){
        hos_name <- red_data[1, "Hospital.Name"]
    }
    else if(num == "worst"){
        hos_name <- red_data[nrow(red_data), "Hospital.Name"]
    }
    else if(num > nrow(red_data)){
        return(NA)
    }
    else{
        hos_name <- red_data[num, "Hospital.Name"]
    }
    
    return(hos_name)
    
}