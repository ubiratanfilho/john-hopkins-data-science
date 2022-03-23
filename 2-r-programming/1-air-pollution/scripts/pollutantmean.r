pollutantmean <- function(pollutant, id=1:332, dir=path_data){
        ### Returns the mean of the specified pollutant in all files within `id`
        
        ### Arguments
        ## dir (character) -> the files directory
        ## pollutant (character) -> the pollutant we wil return its mean ("sulfate" or "nitrate")
        ## id (vector of integers) -> the id of the files we will analyze
        
        path_data <- list.files("data", full.names = TRUE) # list all files in dir
        sum_pollutants = 0 # sum of pollutants (excluding NAs)
        len_pollutants = 0 # number of pollutants (excluding NAs)
        
        for (i in id){
                f = read.csv(path_data[i]) # reading file
                sum_pollutants <- sum_pollutants + sum(f[pollutant], na.rm = TRUE)
                len_pollutants <- len_pollutants + nrow(na.omit(f[pollutant]))
        }
        
        mean_pollutants <- sum_pollutants/len_pollutants #  calculating the mean
        mean_pollutants
        
}