## Part I, Merges the training and the test sets to create one data set.

## Setting up the working directory and functional libraries

> setwd("C:\\Users\\norma_000\\Desktop\\nc\\R\\CourseraFiles")
> library(data.table)
> library(dplyr)

## Getting the column or feature_names

> feature_names <- read.table("UCI HAR Dataset/features.txt")

## Getting the labels from the 6 different activities
> activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

## Getting information from the training and test tables

> subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
> activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
> features_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
> subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
> activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
> features_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

## Merging the training and test tables by subject, activity, and features

> subject <- rbind(subject_test,subject_train)
> activity <- rbind(activity_test,activity_train)
> feature <- rbind(features_test, features_train)

## Naming the activity, subject, and data columns

> colnames(activity) <- "ActivityID"
> colnames(subject) <- "SubjectID"
> colnames(feature) <- t(feature_names[2])

## Combining the data by columns by subject, then feature, finally activity

> combined_data <- cbind(subject,activity,feature,activity)

## Part II, Extracts only the measurements on the mean and standard deviation for each measurement.

## Finding the mean/standard deviation columns

> mean_stdev <- grep(".*mean.*|.*std.*", names(combined_data), ignore.case=TRUE)

## Combining the subject, activity, and mean/standard deviation columns
> mean_stdev_cols <- c(1,2,mean_stdev)

## Reducing the overall data set to the mean/standard deviation data set
> mean_stdev_cols_data <- combined_data[,mean_stdev_cols]

## Part III, Uses descriptive activity names to name the activities in the data set

## Replacing the ActivityIDs 1 to 6 with the activity names from the defined activity_labels variable

> mean_stdev_cols_data$ActivityID <- as.character(mean_stdev_cols_data$ActivityID)

> for (i in 1:6){
  + mean_stdev_cols_data$ActivityID[mean_stdev_cols_data$ActivityID == i] <- as.character(activity_labels[i,2])
  + }
> mean_stdev_cols_data$ActivityID <- as.factor(mean_stdev_cols_data$ActivityID)

## Part IV, Appropriately labels the data set with descriptive variable names.

## Using gsub to replace the abbreviations with words

> names(mean_stdev_cols_data) <- gsub("Acc","Accelerometer",names(mean_stdev_cols_data))
> names(mean_stdev_cols_data) <- gsub("BodyBody","Body",names(mean_stdev_cols_data))
> names(mean_stdev_cols_data) <- gsub("Gyro","Gyroscope",names(mean_stdev_cols_data))
> names(mean_stdev_cols_data) <- gsub("Mag","Magnitude",names(mean_stdev_cols_data))
> names(mean_stdev_cols_data) <- gsub("^t","Time",names(mean_stdev_cols_data))
> names(mean_stdev_cols_data) <- gsub("^f","Frequency",names(mean_stdev_cols_data))
> names(mean_stdev_cols_data) <- gsub("tBody","TimeBody",names(mean_stdev_cols_data))

## Part V, from the data set in step 4, creates a second, independent tidy data set with the average of each 
variable for each activity and each subject.

## Taking the mean by subjectID and activityID of the mean_stdev_cols_data dataset.

> mean_stdev_cols_data$SubjectID <- as.factor(mean_stdev_cols_data$SubjectID)
> mean_stdev_cols_data <- data.table(mean_stdev_cols_data)
> tidyData <- aggregate(. ~SubjectID + ActivityID, mean_stdev_cols_data, mean)
> tidyData <- tidyData[order(tidyData$SubjectID,tidyData$ActivityID),]
> write.table(tidyData, file = "tidydata.txt", row.names = FALSE)