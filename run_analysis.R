#Download the UCI HAR Data set 
UCI_HAR_Dataset <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                                 destfile = "Dataset.zip", method='curl')
unzip("Dataset.zip")

#Reading files from UCI HAR Data set folder
library(data.table)
library(dplyr)

activity <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("UCI HAR Dataset/test/x_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt") 
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("UCI HAR Dataset/train/x_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

#Renaming columns
activity <- rename(activity, ActivityID = V1, Activity = V2)
features <- rename(features, FeatureID = V1, Feature = V2)
subject_test <- rename(subject_test, SubjectID = V1)
colnames(x_test) <- features[,2]
y_test <- rename(y_test, ActivityID = V1)
subject_train <-rename(subject_train, SubjectID = V1)
colnames(x_train) <- features[,2]
y_train <- rename(y_train, ActivityID = V1)

#Processing Steps
#1 Merge the training and the test sets to create one data set
x <- rbind(x_test,x_train)
y <- rbind(y_test, y_train)
subject <- rbind(subject_test, subject_train)

#1.2 Combine all data sets into one
testandtrain <- cbind(subject, x, y)

#2 Extract only the measurements on the mean and standard deviation for each measurement
testandtrain <- testandtrain[, grep("Activity|Subject|mean|std", names(testandtrain))]
testandtrain <- merge(testandtrain, activity, by = "ActivityID") 

#2.1 Aligning column Activity as first column
testandtrain <- testandtrain[, c(82, 1:81)]

#2.2 View combined data set with extracting only mean and standard deviation for each measurement
View(testandtrain)

#3 Uses descriptive activity names to name the activities in the data set
#4 Appropriately labels the data set with descriptive variable names
names(testandtrain) <- gsub("^t", "Time", names(testandtrain))
names(testandtrain) <- gsub("Freq", "Frequency", names(testandtrain))
names(testandtrain) <- gsub("^f", "Frequency", names(testandtrain))
names(testandtrain) <- gsub("Acc", "Accelerometer", names(testandtrain))
names(testandtrain) <- gsub("Gyro", "Gyroscope", names(testandtrain))
names(testandtrain) <- gsub("Mag", "Magnitude", names(testandtrain))
names(testandtrain) <- gsub("mean()", "Mean", names(testandtrain))
names(testandtrain) <- gsub("std", "Standard Deviation", names(testandtrain))
names(testandtrain) <- gsub("BodyBody", "Body", names(testandtrain))

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
testandtrain_average <- group_by(testandtrain, Activity, ActivityID, SubjectID)

#5.1 Getting the average 
testandtrain_average <- summarize_all(testandtrain_average, funs(mean))

#Final:Writing into a tidy data set text file
write.table(testandtrain_average,file="Tidy Data set.txt", row.names = FALSE)