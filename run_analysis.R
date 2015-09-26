## Load required packages https://rstudio-pubs-static.s3.amazonaws.com/55939_3a149c3034c4469ca938b3d9ce964546.html

library(dplyr)
library(data.table)
library(tidyr)

filesPath <- C:/Project/UCI HAR Dataset/UCI HAR Dataset"
setwd(filesPath)
if(!file.exists("./data")) {dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

## Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

## Read data from the files into the variables
  
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "subject_test.txt" )))

dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "Y_test.txt" )))
     
dataTrain <- tbl_df(read.table(file.path(filesPath, "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "X_test.txt" )))

## 1_Merges the training and the test sets to create one data set

# for both Activity and Subject files this 
In both Activity and Subject files, it will be merged the training and the test sets by row binding and the variables "subject" and "activityNum" will be renamed. 

alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

#DATA training and test files combination

dataTable <- rbind(dataTrain, dataTest)

# name variables according to feature

dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

## 2_Extracts only the measurements on the mean and standard deviation for each measurement

# Reading "features.txt" 
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE)

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 


## 3_Uses descriptive activity names to name the activities in the data set

# put name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

# create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

## 4_Appropriately labels the data set with descriptive variable names

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
head(str(dataTable),6)

## 5_From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
write.table(dataTable, "TidyData.txt", row.name=FALSE) 