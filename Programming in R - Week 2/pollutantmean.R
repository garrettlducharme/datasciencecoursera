pollutantmean <- function(directory, pollutant, id = 1:332) {
  
    #This function calculates the mean pollutant levels for a set of observation
    #sites. The directory containing the data and the name of the pollutant must be
    #supplied as arguments.
    
    files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
    temp <- lapply(files[id], read.csv, header = TRUE)
    data <- do.call(rbind, temp)
    rm(temp)
    
    mean(data[[pollutant]], na.rm = TRUE)
}