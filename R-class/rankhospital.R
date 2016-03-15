## Returns hospital name in specified state with the specified rank (num)
rankhospital <- function(state, outcome, num = "best") {
    data <-read.csv("outcome-of-care-measures.csv",colClasses="character")

    if(!state %in% unique(data[,7])) stop("invalid state")

    column.number <- if (outcome == "heart attack") {11}	     
    else if (outcome == "heart failure") {17}
    else if (outcome == "pneumonia") {23}
    else
    {
        stop("invalid outcome")
    }
    
    data[,column.number]=as.numeric(data[,column.number])
    death.rate=data[data[,7]==state,c(2,column.number)]
    death.rate=na.omit(death.rate)
    number.of.rows=nrow(death.rate)
    
    if(num=="best") num=1
    else if (num=="worst") num=number.of.rows
    
    if(num>number.of.rows) return(NA)
    
    ord=order(death.rate[,2],death.rate[,1])
    death.rate[ord,][num,1]
}
