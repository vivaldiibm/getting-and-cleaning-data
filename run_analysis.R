setwd ("E:/coursera course/Course3- Getting and Cleaning the data/Getting and Cleaning Data/Wk4") 
library(data.table)
library(dplyr)

# Task 1 : Merges the training and the test sets to create one data set.
featuresname <- read.table("./UCI HAR Dataset/features.txt", header=FALSE)
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", header=FALSE)
      # reading training data.
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE)
activitytrain <- read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE)
featuretrain <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE)
      # reading test data
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE)
activitytest <- read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE)
featuretest <- read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE)

    # merge dataset
subject <- rbind(subjecttrain, subjecttest)
activity <-  rbind(activitytrain, activitytest)
feature <- rbind(featuretrain, featuretest)
  # naming the column
colnames (feature) <- t(featuresname[2])
colnames (activity) <- "activity"
colnames (subject) <- "subject"
  # Combining total data
datacomplete <- cbind(feature, activity, subject)

 # Task 2 : Extracts only the measurements on the mean and standard deviation for each measurement.
colmeansd <- grep(".*Mean.*|.*Std.*", names(datacomplete), ignore.case=TRUE)
col <- c(colmeansd, 562, 563)
dim(datacomplete)
dataextract <- datacomplete[,col]
dim(dataextract)

 # Task 3 : Uses descriptive activity names to name the activities in the data set.

dataextract$activity <- as.character(dataextract$activity)
dataextract$activity[dataextract$activity ==1] <- "WALKING"
dataextract$activity[dataextract$activity ==2] <- "WALKING_UPSTAIRS"
dataextract$activity[dataextract$activity ==3] <- "WALKING_DOWNSTAIRS"
dataextract$activity[dataextract$activity ==4] <- "SITTING"
dataextract$activity[dataextract$activity ==5] <- "STANDING"
dataextract$activity[dataextract$activity ==6] <- "LAYING"


 # Task 4 : Appropriately labels the data set with descriptive variable names. 
head(str(dataextract),2)
head(str(dataextract))
names(dataextract) <- gsub("std", "sd", names(dataextract))
names(dataextract) <- gsub("^t", "time", names(dataextract))
names(dataextract) <- gsub("^f", "frequency", names(dataextract))
names(dataextract) <- gsub("Acc", "accelerometer", names(dataextract))
names(dataextract) <- gsub("BodyBody", "body", names(dataextract))
names(dataextract) <- gsub("Mag", "magnitude", names(dataextract))
names(dataextract) <- gsub("Gyro", "gyroscope", names(dataextract))
head(str(dataextract),6)


 # Task 5 : From the data set in step 4, creates a second, independent tidy data set with the average
 # of each variable for each activity and each subject.

dataextract$subject <- as.factor(dataextract$subject)
dataextract <- data.table(dataextract)
datatidy <- aggregate(. ~subject + activity, dataextract, mean)
datatidy <- datatidy[order(datatidy$subject, datatidy$activity),]
write.table(datatidy, file = "datatidy.txt", row.names = FALSE)
