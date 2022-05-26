library(magrittr)
library(dplyr)
# The goal of this script is to:
#       1. Merges the training and the test sets to create one data set.

#       2. Extracts only the measurements on the mean and standard deviation for
#        each measurement. 

#       3. Uses descriptive activity names to name the activities in the data set

#       4. Appropriately labels the data set with descriptive variable names. 

#       5. From the data set in step 4, creates a second, independent tidy data 
#       set with the average of each variable for each activity and each subject.

### Reading our data
# train set
DATA_PATH <- "UCI HAR Dataset"
X_train <- readLines(file.path(DATA_PATH, "train/X_train.txt"))
X_test <- readLines(file.path(DATA_PATH, "test/X_test.txt"))
y_train <- readLines(file.path(DATA_PATH, "train/y_train.txt"))
y_test <- readLines(file.path(DATA_PATH, "test/y_test.txt"))
sub_train <- readLines(file.path(DATA_PATH, "train/subject_train.txt"))
sub_test <- readLines(file.path(DATA_PATH, "test/subject_test.txt"))
# X_train and X_test are strings. We want to split them to create a numeric vector. 
# Also our first two character is " ", so we must remove it.
preprocess_raw_X_data <- function(x){
        # In this funciont, we will receive our string and return as a preprocessed
        # dataframe
        x <- substr(x, 3, nchar(x))
        x <- gsub("  ", " ", x)
        x %>% 
                lapply(strsplit, " ") %>% 
                data.frame() %>%
                data.table::transpose() %>%
                apply(2, as.numeric) %>%
                as.data.frame()
        
}
X_train <- preprocess_raw_X_data(X_train)
X_test <- preprocess_raw_X_data(X_test)
y_train <- as.numeric(y_train)
y_test <- as.numeric(y_test)
sub_train <- as.numeric(sub_train)
sub_test <- as.numeric(sub_test)
# merge train and test into one dataset
X_dataset <- rbind(X_train, X_test)
y_dataset <- c(y_train, y_test)
sub_dataset <- c(sub_train, sub_test)
# add mean and deviation of all instances
X_dataset <- as.data.frame(cbind(apply(X_dataset, 1, mean), apply(X_dataset, 1, sd)))
colnames(X_dataset) <- c("mean", "sd")
# change numbers in y as strings representing each activity
y_dataset <- factor(y_dataset, labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", 
                                 "SITTING", "STANDING", "LAYING"))
# merge X with y
dataset <- cbind(sub_dataset, X_dataset, y_dataset)
colnames(dataset) <- c("subject_id", "mean", "sd", "activity")
# create df with the average variables per subject and activity
avg_activity_per_subject <- dataset %>% 
                                        group_by(subject_id, activity) %>%
                                        summarise(mean = mean(mean),
                                                  sd = mean(sd))
# exporting data
write.csv(dataset, "output/full_dataset.csv", row.names = FALSE)
write.csv(avg_activity_per_subject, "output/avg_activity_per_subject.csv", row.names = FALSE)