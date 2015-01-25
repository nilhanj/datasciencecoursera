## Courese Project for Gettign and Cleaning Data ##############################
## 
## This script is to produce the following -
## 1) Merges the training and the test sets to create one data set.
## 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3) Uses descriptive activity names to name the activities in the data set
## 4) Appropriately labels the data set with descriptive variable names. 
## 5) From the data set in step 4, creates a second, independent tidy data set with 
##    the average of each variable for each activity and each subject.
##
###############################################################################

run_analysis <- function() {

  # load all the necessary packages
  library(dplyr)
  library(tidyr)
  
  
  ## assign file paths to the 6 files we want to extract and merge
  features_file <- "./UCI HAR Dataset/features.txt"
  
  x_train <- "./UCI HAR Dataset/train/X_train.txt"
  y_train <- "./UCI HAR Dataset/train/y_train.txt"
  subject_train <- "./UCI HAR Dataset/train/subject_train.txt"
  
  x_test <- "./UCI HAR Dataset/test/X_test.txt"
  y_test <- "./UCI HAR Dataset/test/y_test.txt"
  subject_test <- "./UCI HAR Dataset/test/subject_test.txt"
  
  activity_labels <- "./UCI HAR Dataset/activity_labels.txt"                      
  
  
  
  ##  Combine training data 
  #   add subject ID to add activity numbers
  data_merged <- read.table(subject_train)
  data_new <- read.table(y_train)
  
  data_merged <- cbind(data_merged, data_new)
 
  # Reusing same variable to get training data
  data_new <- read.table(x_train)
  
  data_merged <- cbind(data_merged, data_new)
  
  # Combine test data in the same way
  data_test <- read.table(subject_test)
  data_new <- read.table(y_test)
  data_test <- cbind(data_test, data_new)
  
  data_new <- read.table(x_test)
  data_test <- cbind(data_test, data_new)
  
 
  ## Merge train and test data using rbind
  data_merged <- rbind(data_merged, data_test)
  
  ## Add the Column names Subject_ID, Activity and features for the variables
  
  features <- read.table(features_file)
  
  # Take the features names column and turn into a character vector, then use 
  # to assign the variable names
  features_names <- as.character(features[,2]) 

  #Convert to a data table so we can use dplyr verbs
  data_tbl <- tbl_df(data_merged) # converting to a data table to work with Dplyr
  rm(data_merged)  # Remove data_frame to save release memory
  
  # Set column names for subject id, activity and the features from features.txt
  colnames(data_tbl) <- c("subjectid", "activity", features_names)
  
  # Subset the data to extract the subjectid, activity and columns containing mean() and std() AT THE END OF THE NAME
  # NOTE: I have chosen not to inlcude columns with names where mean and std appear in the middle of the name
  
  data_tbl <- data_tbl[, grep("subjectid|activity|mean\\(|std\\(", colnames(data_tbl), ignore.case = TRUE)] #
  
  
  ## Substitue activity index with corresponding label from the file
  activity_name <- read.table(activity_labels)
  data_tbl <- mutate(data_tbl, activity = activity_name[activity,2])

  ## Create tidy data set with the average of each variable for each activity 
  ## and each subject.
  
  # Remove paranthises and hyphens from activity names
  names(data_tbl) <- gsub("\\(|\\)|\\-", "", names(data_tbl))
  
  # Change all column names to lower case 
  names(data_tbl) <- tolower(names(data_tbl))   
  
 
    data_tbl <- group_by(data_tbl, activity, subjectid)    # Group by 'subjects' then 'activity'
      # Get the mean of of all variable by activity and subject and assign to new columns mean
    data_tbl <- summarise_each(data_tbl,(funs(mean)))
        
    # write a text file od all variables selectes
    write.csv(data_tbl, "./tidy-data.csv")
    # Print tidy data
    print(data_tbl)
  
  
}