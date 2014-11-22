
suppressWarnings(library(data.table))


rm(list=ls())

features = read.table("./features.txt", header=FALSE)
activityType = read.table("./activity_labels.txt", col.names = c("activityId","activityType"), header=FALSE)
subjectTrain = read.table("./train/subject_train.txt", col.names = c("subjectId"),  header=FALSE)
yTrain  = read.table("./train/y_train.txt", col.names = c("activityId"),  header=FALSE)
xTrain  = read.table("./train/x_train.txt", header=FALSE)

subjectTest = read.table("./test/subject_test.txt", col.name = c("subjectId"), header=FALSE)
xTest = read.table("./test/x_test.txt", header=FALSE)
yTest = read.table("./test/y_test.txt", col.name = "activityId",  header=FALSE)
colnames(xTrain) = features[,2]
colnames(xTest) = features[,2]

finalTrainData = cbind ( xTrain, yTrain, subjectTrain)
finalTestData = cbind ( xTest, yTest, subjectTest)



## 1. Merges the training and the test sets to create one data set.

oneDataSet = rbind (finalTrainData, finalTestData)
colNames  = colnames(oneDataSet) 


## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

logicalVector = (grepl(  "activity..", colNames) | grepl( "subject.." , colNames) | grepl( "-mean..", colNames) & !grepl( "-meanFreq..", colNames) & !grepl( "mean..-" , colNames) | grepl( "-std.." , colNames) & !grepl( "-std()..-" , colNames))

oneDataSet = oneDataSet[logicalVector==TRUE]


## 3. Uses descriptive activity names to name the activities in the data set

oneDataSet = merge(oneDataSet, activityType, by='activityId', all.x=TRUE) 

colNames  = colnames(oneDataSet)

## 4. Appropriately labels the data set with descriptive variable names. 

colNames = gsub("\\()","",colNames)
colNames = gsub("-mean","Mean",colNames)
colNames = gsub("-std","StdDev",colNames)
colNames = gsub("BodyBody","Body",colNames)
colNames = gsub("^(t)","time",colNames)
colNames = gsub("^(f)","freq",colNames)

colnames(oneDataSet) = colNames


## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

step5Data  <- copy (oneDataSet)
step5Data$activityType <- NULL

step5FinalData    = aggregate(step5Data [,names(step5Data) != c('activityId','subjectId')] , by = list(activityId=step5Data$activityId, subjectId = step5Data$subjectId), mean) 

step5FinalData    = merge (step5Data, activityType, by='activityId', all.x=TRUE)


write.table(step5FinalData, './step5FinalData.txt', row.name=FALSE)



