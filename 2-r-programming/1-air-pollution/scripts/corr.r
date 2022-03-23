corr <- function(threshold = 0, dir = "data"){
        ### Returns all files with the correlation between sulfate and nitrate
        ### greater than the threshold
        
        ### Arguments
        ## threshold (integer) <- the corr threshold
        ## dir (list of characters) <- directory with files to be analyzed
        
        path_data <- list.files("data", full.names = TRUE) # list all files in dir

        corr_vec <- c()
        for (i in 1:332){
                f <- data.frame(read.csv(path_data[i])) # reading file
                f_nonNA <- f[!is.na(f$sulfate & f$nitrate), ]
                if (nrow(f_nonNA) > threshold){
                        corr_i <- cor(f_nonNA$sulfate, y=f_nonNA$nitrate)
                        corr_vec <- append(corr_vec, c(corr_i))
                }
        }
        corr_vec
}