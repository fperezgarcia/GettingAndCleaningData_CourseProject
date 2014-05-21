setwd("/Users/fernando_perez/Documents/workspace_R/coursera/GettingAndCleaningData_CourseProject/data")
library(plyr)
library(data.table)

ImportData <- function (fileName, dir="root"){
    
    rootDir <- "UCI HAR Dataset"
    
    if (dir == "test"){
        name = paste(rootDir, "/test/", fileName, sep = "");
    }
    
    if (dir == "train"){
        name = paste(rootDir, "/train/", fileName, sep = "");
    }
    
    if (dir == "root"){
        name = paste(rootDir, "/", fileName, sep = "");
    }

    data <- read.table(name, header=FALSE)    
    
    data
}

##########################
##1. Merges the training and the test sets to create one data set.
##########################
sourceNameZip <- "getdata-projectfiles-UCI HAR Dataset.zip"
url           <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#Download / Unzip source data
if (!file.exists(sourceNameZip)){
    download.file(url, sourceNameZip, method="curl")
    unzip(sourceNameZip,exdir=".") 
}

#Load Features Data
trainData <- ImportData("X_train.txt", "train")
testData  <- ImportData("X_test.txt", "test")
allFeaturesData <- rbind(trainData, testData)

#Load Activity Data
activityTrainData <- ImportData("y_train.txt", "train")
activityTestData  <- ImportData("y_test.txt",  "test")
activityData <- rbind(activityTrainData, activityTestData)

#Load Suject Data
subjectTrainData <- ImportData("Subject_train.txt", "train")
subjectTestData  <- ImportData("Subject_test.txt",  "test")
subjectData <- rbind(subjectTrainData, subjectTestData)

##########################
##2. Extracts only the measurements on the mean and standard deviation for each measurement.
##########################
featureNames <- ImportData("features.txt")
selectedFeaturesNames <- featureNames[grep("mean|std", featureNames[,2]),]
featuresData <- allFeaturesData[,selectedFeaturesNames[,1]]

data <- cbind(featuresData, activityData)
data <- cbind(data, subjectData)
names(data) <- c(as.character(selectedFeaturesNames[,2]), "ActivityId", "SubjectId")

##########################
##3. Uses descriptive activity names to name the activities in the data set
##4. Appropriately labels the data set with descriptive activity names
##########################
activityIds <- data[,"ActivityId"]
activityFactors <- as.factor(activityIds)
activityFactors = revalue(activityFactors, c("1"="WALKING", "2"="WALKING_UPSTAIRS","3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))
data[,"ActivityId"] = activityFactors
colnames(data) <- c(as.character(selectedFeaturesNames[,2]), "ActivityName", "SubjectId")

##########################
##5. Creates a second, independent tidy data set with the average of each variable 
##   for each activity and each subject. 
##########################
dataTable <- data.table(data)
meanDataTable <- dataTable[, lapply(.SD,mean), by=c("ActivityName","SubjectId")]

#Add -mean() to all data column names
meanColNames = sapply(names(meanDataTable)[-(1:2)], function(name) paste0(name, "-mean()"))
setnames(meanDataTable, names(meanDataTable), c("ActivityName", "SubjectId", meanColNames))

#Save Tidy Data to file
write.csv(meanDataTable, file="TidyDataSet_Means.csv", row.names = FALSE);