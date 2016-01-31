# Reads in data set given the:
#  1. Path to data file
#  2. Path to data labels
#  3. Path to Subject IDs
#  4. Path to variable names
readData <- function(xpath, ypath, subjpath, variablesFile)
{
    #Read in feature names
    features <- read.table(variablesFile, header = FALSE, stringsAsFactors = FALSE)
    
    setData <- read.table(xpath, header = FALSE)
    setlabel <- read.table(ypath, header = FALSE, col.names = c("activity"))
    subject <- read.table(subjpath, header = FALSE, col.names = c("subject"))

    #Rename to feature variable names
    numFeatures <- ncol(setData)
    setData <- setnames(setData, old = names(setData[,1:numFeatures]), new = features$V2)
    
    #Assign unique names to feature variables
    names(setData) <- make.names(names=names(setData), unique=TRUE, allow_ = TRUE)
    
    #Create one data frame with all set information
    bindedData <- cbind(subject, setlabel, setData)
    
    return(bindedData)
}

#1. Merges the training and the test sets to create one data set.
mergeData <-function(set1, set2)
{
    #Merge both data sets
    mergeData <-merge(set1, set2, all = TRUE)
    
    #Sorty by Subject, then label
    mergeData <-arrange(mergeData, subject, activity)
    
    return(mergeData)
}

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
extractMeanAndStd <- function(myData)
{
    ##Takes in a data set
    ## Checks for column names with mean and std in the column names
    ## creates new dataset with first 2 columns, and mean() and std() columns
    myData <- select(myData, subject, activity, matches("\\.(mean|std)\\.\\.*"))
    
    return(myData)
}

#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5.From the data set in step 4, creates a second, 
#   independent tidy data set with the average of each variable 
#   for each activity and each subject.
#
createTidy <- function(myData, activityFile)
{
    #Read in Activity names
    activitylabels <- read.table(activityFile, header = FALSE, stringsAsFactors = FALSE)
    
    #Assign actual label name to label number
    myData <- mutate(myData, activity = activitylabels$V2[activity])
    
    #Rename columns to be more descriptive
    names(myData) <- sub("^t", "time", names(myData))
    names(myData) <- sub("^f", "freq", names(myData))
    names(myData) <- gsub("\\.+", "\\.", names(myData))
    
    bySubjActivity <- group_by(myData, subject, activity)
    
    ##Createa new data set with the average result for each variable
    ##by the same subject and activity
    meanBySubjectActivity <- summarize_each(bySubjActivity, funs(mean))
    
    return(meanBySubjectActivity)
}

#Run this function to get final result
#Make sure that your working directory is set to the UCI HAR Dataset folder
runAnalysis <- function()
{
    library(dplyr)
    library(data.table)
    
    featuresFile <- "features.txt"
    activityFile <- "activity_labels.txt"
    
    trainXFile <- "train/X_train.txt"
    trainYFile <- "train/y_train.txt"
    subjectTrainFile <- "train/subject_train.txt"
    
    testXFile <- "test/X_test.txt"
    testYFile <- "test/y_test.txt"
    subjectTestFile <- "test/subject_test.txt"
    
    test <- readData(testXFile, testYFile, subjectTestFile, featuresFile)
    train <- readData(trainXFile, trainYFile, subjectTrainFile, featuresFile)
    
    merged <- mergeData(test, train)
    merged <- extractMeanAndStd(merged)
    myResult <- createTidy(merged, activityFile)
    
    return (myResult)
}


