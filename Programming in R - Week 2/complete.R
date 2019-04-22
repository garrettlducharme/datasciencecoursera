complete <- function(directory, id = 1:332) {
  
    #Returns a data frame with the number of complete observations for the given
    #ids of the observation sites.
  
    files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)[id]
    counts <- rep(NA, length(id))
    
    for (i in 1:length(id)) {
        counts[i] <- nrow(na.omit(read.csv(files[i], header = TRUE)))
    }
    
    cases <- data.frame("id" = id, "nobs" = counts)
    cases
}