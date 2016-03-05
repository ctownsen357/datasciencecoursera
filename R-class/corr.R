library("plyr")

#calculates the correlation between sulfate and nitrate greater than the threshold for the specified directory of observations
corr <- function(directory, threshold = 0) {
    p <- paste(directory, "/*.csv", sep = "")

    tmp <- list.files(directory)
    tmp <- paste(paste(directory, "/", sep = ""), tmp, sep = "")
    df <- do.call(rbind, lapply(tmp, read.csv))
    
    completedf <- df[complete.cases(df),]
    counts_by_id <- count(completedf, "ID")
    target_ids <- subset(counts_by_id, counts_by_id$freq > threshold)$ID

    crr = numeric()
    for (id in target_ids){
        data_to_process <- subset(completedf, completedf$ID == id)
        crr <- c(crr, cor(data_to_process$sulfate, data_to_process$nitrate))
    }
    crr
    #colSum(!is.na(df)) # gave a count of non NA for each column
    #sum(!is.na(df)) # gave count of non NA for the entire df
}

#used this when tinkering to figure things out so I didn't have to keep waiting for all the data to load
loaddf <- function(directory) {
    p <- paste(directory, "/*.csv", sep = "")
    
    tmp <- list.files(directory)
    tmp <- paste(paste(directory, "/", sep = ""), tmp, sep = "")
    #df <- lapply(tmp, read.csv)
    df <- do.call(rbind, lapply(tmp, read.csv))
}


#found these examples when I got stuck so I could figure out why my version was not working
#I was applying cor to the entire data set rather than for each ID
dcorr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating the location of
    ## the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the number of
    ## completely observed observations (on all variables) required to compute
    ## the correlation between nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    df = dcomplete(directory)
    ids = df[df["nobs"] > threshold, ]$id
    corrr = numeric()
    for (i in ids) {
        
        newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                                 ".csv", sep = ""))
        dff = newRead[complete.cases(newRead), ]
        corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
    }
    return(corrr)
}
dcomplete <- function(directory, id = 1:332) {
    f <- function(i) {
        data = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                              ".csv", sep = ""))
        sum(complete.cases(data))
    }
    nobs = sapply(id, f)
    return(data.frame(id, nobs))
}


