# Coursera - Getting and Cleaning Data - Assignment
# Author - Mitchell Harvey
# Date - 7 March 2021

# install libraries
library(here)
library(dplyr)

# get the data
# create data directory
if (dir.exists("data")) {
  message("data directory exists")
  
} else {
  message("data directory does not exist, creating directory...")
  dir.create("data")
  message("data directory created")
}

# download data
if (file.exists("./data/data.zip")) {
  message("data.zip is already downloaded")
  
  
} else {
  message("data.zip does not exist, downloading...")
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, here("data/data.zip"), method = "libcurl", mode = "wb")
  message("data downloaded.")
}

# unzip data
unzip(here("data/data.zip"), exdir=here("data"))


# Merges the training and the test sets to create one data set.

# get the files
X_test <- read.table(here("data/UCI HAR Dataset/test/X_test.txt"))
y_test <- read.table(here("data/UCI HAR Dataset/test/y_test.txt"))
subject_test <- read.table(here("data/UCI HAR Dataset/test/subject_test.txt"))
X_train <- read.table(here("data/UCI HAR Dataset/train/X_train.txt"))
y_train <- read.table(here("data/UCI HAR Dataset/train/y_train.txt"))
subject_train <- read.table(here("data/UCI HAR Dataset/train/subject_train.txt"))

# Create Labels for columns
cols <- c("activityId")
colnames(y_test) <- cols
colnames(y_train) <- cols

cols <- c("subjectId")
colnames(subject_test) <- cols
colnames(subject_train) <- cols


# get feature labels
features <- read.table(here("data/UCI HAR Dataset/features.txt"))

cols <- c(features[2])
colnames(X_train) <- cols[["V2"]]
colnames(X_test) <- cols[["V2"]]

# merge by cols
merged_train <- cbind(subject_train, y_train, X_train)
merged_test <- cbind(subject_test, y_test, X_test)

# merge by rows
merged <- rbind(merged_train, merged_test)



# Extracts only the measurements on the mean and standard deviation 
# for each measurement. 

stripped <- merged[,
       grepl("std\\(\\)",names(merged)) | 
         grepl("mean\\(\\)",names(merged)) |
         grepl("^activityId$",names(merged)) |
         grepl("^subjectId$",names(merged))]




# Uses descriptive activity names to name the activities in the data set

# get activity labels
activity_labels <- read.table(here("data/UCI HAR Dataset/activity_labels.txt"))
cols <- c("activityId", "activity")

colnames(activity_labels) <- cols

adjusted <- left_join(stripped, activity_labels, by="activityId")

# reshuffle cols
adjusted = adjusted[,c(1,69,2:68)]

# drop the activityId col
adjusted = subset(adjusted, select = -c(activityId))

# Appropriately labels the data set with descriptive variable names. 

# see above when feature labels are assigned to col names

# From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.

# average all features, grouped by subjectId, activity
final <- adjusted %>% 
  group_by(subjectId, activity) %>%
  summarise_all(.funs = c(mean="mean")) %>% 
  ungroup()

name <- "final_output.csv"

# output to csv
write.csv(final, here(name), row.names = FALSE)

message("Processing complete! find '", name, "' here: ", here(),'/', name)
