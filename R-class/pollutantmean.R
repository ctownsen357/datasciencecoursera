#complete returns the number of observations within the file ids 
#contained in the specified directory that are complete (not NA)
#trying to it a little more interesting by keeping everything as
#vectorized as possible
complete <- function(directory, id = 1:332){
    padded_ids <- paste(sprintf("%03d", id), ".csv", sep = "")
    padded_ids <- paste("/", padded_ids, sep = "")
    padded_ids <- paste(directory, padded_ids, sep = "")
    
    file_lengths <- do.call(rbind, lapply(lapply(lapply(padded_ids, read.csv), complete.cases),sum))
    
    x_name <- "id"
    y_name <- "nobs"
    
    df <- data.frame(id, file_lengths)
    names(df) <- c(x_name, y_name)
    df
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    # "Date","sulfate","nitrate","ID"
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    ##read.csv(file = "specdata/")

    ## 'polutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the 
    ## mean; either "sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    #creating a vector of all the paths for the target ids
    padded_ids <- paste(sprintf("%03d", id), ".csv", sep = "")
    padded_ids <- paste("/", padded_ids, sep = "")
    padded_ids <- paste(directory, padded_ids, sep = "")
    
    pdata <- do.call(rbind, lapply(padded_ids, read.csv))
    
    mean_val <- mean(pdata[,pollutant], na.rm = TRUE)
    
    #format(round(mean_val, 3), nsmall = 3)
    format.default(mean_val, digits = 4)
    
}

