corr <- function(directory, threshold = 0) {
  
    #Returns a vector of correlations between for each of the observation sites
    #with complete observations above the given threshold. This function makes
    #use of the complete function to check which sites have observations above
    #the given threshold.
  
    temp <- complete(directory)
    cases <- which(temp$nobs > threshold)
    correlations <- rep(NA, length(cases))
    files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)[cases]
    dfs <- lapply(files, read.csv, header = TRUE)
    
    if(length(dfs) == 0){
        return(numeric(0))
    }
    else{
        for (i in 1:length(dfs)) {
            correlations[i] <- cor(dfs[[i]]$sulfate, dfs[[i]]$nitrate, use = 'complete.obs')
        }
    }
    return(correlations)
}