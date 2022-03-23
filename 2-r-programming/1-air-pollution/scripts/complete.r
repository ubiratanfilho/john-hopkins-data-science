complete <- function(dir = "data", id = 1:332){
        ### Returns a dataframe with the number of complete cases (non-NA values
        ### in all columns) in each file specified by `id`
        
        ### Arguments
        ## dir (character) -> the files directory
        ## id (vector of integers) -> the id of the files we will analyze
        
        path_data <- list.files("data", full.names = TRUE) # list all files in dir
        df <- data.frame() # creates empty df
        for (i in id) {
                f = data.frame(read.csv(path_data[i])) # reading file
                non_NA_sum <- sum(!is.na(f["sulfate"] & f["nitrate"])) # sum of 
                # non NA values 
                df2 <- data.frame(id = i, nobs = non_NA_sum) # df we will append
                # with id and nobs
                df <- rbind(df, df2) # joining our dfs
        }
        df
}