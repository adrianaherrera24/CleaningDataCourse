## Cleaning Data

## Reading Test data
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", sep = "")
xTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt", sep = "")
yTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt", sep = "")

## Reading Train data
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", sep = "")
xTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt", sep = "")
yTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt", sep = "")

## Reading Features data
features <- read.table("./data/UCI HAR Dataset/features.txt", sep = "")

## Reading Activity data
Actlabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", sep = "")

## Merge all combinations of Test and Train
Subject <- rbind(subject_test,subject_train)
x <- rbind(xTrain,xTest)
y <- rbind(yTrain,yTest)

## Renaming columns
names(Subject) <- 'subject_id'
names(features) <- c("feature_id", "feature")
names(Actlabels) <- c("activity_id", "activity")
names(y) <- 'id'
names(x) <- features$feature

#########################################################################
## 1. Merges the training and the test sets to create one data set.
#########################################################################
mainData <- cbind(Subject, x, y)

##############################################################################################
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##############################################################################################
mainMeasurements <- select(mainData, subject_id,id, contains("mean"), contains("std"))
names(mainMeasurements)

###############################################################################
## 3. Uses descriptive activity names to name the activities in the data set.
###############################################################################
mainActivities <- mainMeasurements%>%mutate(activity=(Actlabels[id, 2]))%>%select(subject_id,id,activity,everything())

###############################################################################
## 4. Appropriately labels the data set with descriptive variable names.
###############################################################################
names(mainActivities)<-gsub("Acc", "Accelerometer", names(mainActivities))
names(mainActivities)<-gsub("Gyro", "Gyroscope", names(mainActivities))
names(mainActivities)<-gsub("^t", "Time", names(mainActivities))
names(mainActivities)<-gsub("BodyBody", "Body", names(mainActivities))
names(mainActivities)<-gsub("Mag", "Magnitude", names(mainActivities))
names(mainActivities)<-gsub("^f", "Frequency", names(mainActivities))
names(mainActivities)<-gsub("tBody", "TimeBody", names(mainActivities))

###############################################################################
## 5. From the data set in step 4, creates a second, independent tidy data set
##    with the average of each variable for each activity and each subject.
###############################################################################
finalData <- mainActivities%>%group_by(subject_id,activity)%>%summarise_all(mean)

###############################################################################
## 6. Write .txt with the final data
###############################################################################
write.table(finalData, "finalData.txt", row.name=FALSE)

