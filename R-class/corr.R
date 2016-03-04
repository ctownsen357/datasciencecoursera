corr <- function(directory, threshold = 0) {
    p <- paste(directory, "/*.csv", sep = "")

    tmp <- list.files(directory)
    tmp <- paste(paste(directory, "/", sep = ""), tmp, sep = "")
    #df <- lapply(tmp, read.csv)
    df <- do.call(rbind, lapply(tmp, read.csv))
    df[1:3,]
    #files
    completedf <- df[complete.cases(df),]
    counts_by_id <- count(completedf, "ID")
    target_ids <- subset(counts_by_id, counts_by_id$freq >= threshold)$ID
    data_to_process <- subset(completedf, completedf$ID %in% target_ids)
    cor(data_to_process$sulfate, data_to_process$nitrate)
    #colSum(!is.na(df)) # gave a count of non NA for each column
    #sum(!is.na(df)) # gave count of non NA for the entire df
}

loaddf <- function(directory) {
    p <- paste(directory, "/*.csv", sep = "")
    
    tmp <- list.files(directory)
    tmp <- paste(paste(directory, "/", sep = ""), tmp, sep = "")
    #df <- lapply(tmp, read.csv)
    df <- do.call(rbind, lapply(tmp, read.csv))
    
    
    #files
}

dcorr <- function(df, threshold = 0) {
    df[1:3,]
    #files
    #df[cumsum(!is.na(df[complete.cases(df),]))[1:1],]
}

dss <- function(df, threshold) {
 ss <- df[cumsum(!is.na(df))[1:5],]
 ss
}
#need to get the IDs of N cases where the threshold has been met