#Returns the best hospital for a given state and outcome. The best hospital is
#the one with the lowest mortality rate for the given outcome within a state.

#Allowed outcomes are "heart attack", "heart filure", and "pneumonia".
#Allowed states are the abbreviations for the fifty US states, as well as
#"DC", "GU", "MP", "PR", and "VI".
#Ties for the best hospital are chosen by selecting the hospital that comes first
#alphabetically.
best <- function(state, outcome) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    states <- c(state.abb, "DC", "GU", "MP", "PR", "VI")

    if(state %in% states == FALSE) {stop("invalid state")}
    
    red_data = data[data$State == state,]
    
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
    
    out_min <- min(as.numeric(red_data[,ind]), na.rm = TRUE)
    hos_names <- red_data[as.numeric(red_data[,ind]) == out_min, "Hospital.Name"]
    
    hos_names <- hos_names[!is.na(hos_names)]
    return(sort(hos_names)[1])
}

