# reads the outcome-of-care-measures.csv file and returns a 2-column data frame
#ontaining the hospital in each state that has the ranking speci ed in num
rankall <- function(outcome, num = "best") {
    data <-read.csv("outcome-of-care-measures.csv",colClasses="character")

    states<-unique(data[,7])
    
    column.number <- if (outcome == "heart attack") {11}	     
    else if (outcome == "heart failure") {17}
    else if (outcome == "pneumonia") {23}
    else
    {
        stop("invalid outcome")
    }

    data[,column.number] = as.numeric(data[,column.number])
    data = data[,c(2,7,column.number)]
    data = na.omit(data) #data = data[complete.cases(data), ]
    
    
    rank.in.state<-function(state){
        death.rate = data[data[,2] == state,]
        number.of.rows = nrow(death.rate)
        
        if(num == "best") {num = 1}
        else if (num == "worst") {num = number.of.rows}

        if(num > number.of.rows){return(NA)}
        
        ord = order(death.rate[,3],death.rate[,1])
        c(death.rate[ord,][num,1], state)
    }
    
    results = do.call(rbind, lapply(states, rank.in.state))
    results = results[order(results[, 2]), ]
    rownames(results) = results[, 2]
    colnames(results) = c("hospital", "state")
    
    data.frame(results)
}
