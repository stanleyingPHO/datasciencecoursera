#####################################################################################################################

## Coursera - Getting and Cleaning Data - Course Project
## Stanley Ing
## 2015-05-16

#runAnalysis.r File Description

# This script will perform the following steps on the UCI HAR Dataset downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

######################################################################################################################

## Libraries

library(dplyr)

## Set working directory to the location where UCI HAR Dataset is located

setwd("~/Desktop/Continuing Education/Coursera/Getting and Cleaning Data/UCI HAR Dataset")

# 1. Merge the training and the test sets to create one data set.

## Read in data into R 

# Train folder

features <- read.table('./features.txt', header=FALSE) #contains features from accelerometer and gyroscope
activitylabel <- read.table('./activity_labels.txt', header=FALSE) #contains the activity label (numeric and corresponding label)
subjecttrain <- read.table('./train/subject_train.txt', header=FALSE) #contains subjects who performed activity (1-30)
xtrain <- read.table('./train/X_train.txt', header=FALSE) #contains training sets
ytrain <- read.table('./train/y_train.txt', header=FALSE) #contains test labels

# Test folder

subjecttest <- read.table('./test/subject_test.txt', header=FALSE) #contains subjects with test data
xtest <- read.table('./test/x_test.txt', header=FALSE) #contains test sets 
ytest <- read.table('./test/y_test.txt', header=FALSE) #contains test labels

## As the dataframes do not have column names, we will need to assign them

# Assigning column names to training data

colnames(activitylabel) <- c('activityID', 'activitylabel') 
colnames(subjecttrain) <- "subjectID"
colnames(xtrain) <- features[,2]
colnames(ytrain) <- "activityID"

# Assigning column names to test data 

colnames(subjecttest) <- "subjectID"
colnames(xtest) <- features[,2]
colnames(ytest) <- "activityID"

## Create datasets

# training data - combining subjecttrain, ytrain, and xtrain

trainingdata <- cbind(subjecttrain, ytrain, xtrain)

# test data - combining subjecttest, ytest, xtest

testdata <- cbind(subjecttest, ytest, xtest)

## Merge training and test datasets

finaldata <- rbind(trainingdata, testdata)

## Extract colnames for next step

colNames <- colnames(finaldata)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

## Display ColNames to identify how column names are structured

colNames

## Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | 
                         grepl("subject..",colNames) | 
                         grepl("-mean..",colNames) & 
                         !grepl("-meanFreq..",colNames) & 
                         !grepl("mean..-",colNames) | 
                         grepl("-std..",colNames) & 
                         !grepl("-std()..-",colNames))

## Subset finaldata table based on logicalvector to keep only desired variables

finaldata = finaldata[logicalVector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

## Merge the finalData set with the acitivityType table to include descriptive activity names
finaldata <- merge(finaldata, activitylabel, by='activityID');

## Update the colNames vector to include the new column names after merge
colNames <- colnames(finaldata)

# 4. Appropriately label the data set with descriptive activity names. 

## Cleaning up the variable names
for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

## Reassigning the new descriptive column names to the finalData set

colnames(finaldata) <- colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

## Create a new table without the activitylabel column

finaldata <- finaldata[,names(finaldata) != 'activitylabel']

## Summarize dataframe to include just the mean of each variable for each activity and each subject

tidydata <- aggregate(finaldata[,names(finaldata) != c('activityID', 'subjectID')], 
                      by=list(activityID=finaldata$activityID, subjectID=finaldata$subjectID), mean)

## Merge tidydata with activitylabel to include descriptive acitvity names

tidydata <- merge(tidydata, activitylabel, by='activityID')
tidydata <- arrange(tidydata, subjectID, activityID)
tidydata <- tidydata %>% select(subjectID, activityID, activitylabel, timeBodyAccMagnitudeMean, timeBodyAccMagnitudeStdDev, 
                                timeGravityAccMagnitudeMean, timeGravityAccMagnitudeStdDev, timeBodyAccJerkMagnitudeMean, 
                                timeBodyAccJerkMagnitudeStdDev, timeBodyGyroMagnitudeMean, timeBodyGyroMagnitudeStdDev, 
                                timeBodyGyroJerkMagnitudeMean, timeBodyGyroJerkMagnitudeStdDev, freqBodyAccMagnitudeMean, freqBodyAccMagnitudeStdDev,
                                freqBodyAccJerkMagnitudeMean, freqBodyAccJerkMagnitudeStdDev, freqBodyGyroMagnitudeMean, freqBodyGyroMagnitudeStdDev,
                                freqBodyGyroJerkMagnitudeMean, freqBodyGyroJerkMagnitudeStdDev)
        
# Export the tidydata set

write.table(tidydata, './tidydata.txt',row.name=FALSE,sep='\t')
