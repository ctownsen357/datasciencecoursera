#The 'best' function reads the outcome-of-care-measures.csv
#file and returns a character vector with  the  name  of  
#the  hospital  that  has  the  best  (i.e.   lowest)  30-day  
#mortality  for  the  specified  outcome in that state.

#The outcomes can be one of "heart attack", "heart failure", or "pneumonia".

best <- function(state, outcome){
    care.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #check the state code
    states <- unique(care.outcome$State)
    if (state %in% states == F) stop("invalid state")
        
    if (is.na(match(outcome,  c("heart attack", "heart failure", "pneumonia")))) {
        stop("invalid outcome")
    }
    
    col.name <- "" #will create a map from outcome to the appropriate column name in the dataset
    
    if (outcome == "heart attack"){
        col.name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }   

    if (outcome == "heart failure"){
        col.name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }   

    if (outcome == "pneumonia"){
        col.name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }   
    
    #get only the hospital name, state, and target column
    data <- subset(care.outcome[c("Hospital.Name", "State", col.name)], State == state)
    
    data[,col.name] = as.numeric(data[,col.name])
    data <- data[complete.cases(data),]
    min.attack.rate <- min(data[,col.name])
    final.results <- data[data[col.name] == min.attack.rate, ]$Hospital.Name
    sort(final.results)[1]
}