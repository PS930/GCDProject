######################################################################################
#The following is a script for creating a tidy data set containing measurements from #
#6 types of activities collected by accelerometers for subjects in training and in   #
#testing groups. Please read the codebook see the comments along the way to          #
#understand why certain manipulations were made. For project details refer to:       #                            #
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones #
######################################################################################
#Set working directory.
setwd("~/Coursera/R Projects/Getting and Cleaning Data")
#Download the data and save the zipped file with the name "Dataset.zip"
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip ","Dataset.zip", mode="wb")
#Unzip the file in your working directory
unzip("Dataset.zip")
#Manually take files out of test and training folders & placed in working directory,
#inside the "UCI HAR Dataset" folder.
setwd("~/Coursera/R Projects/Getting and Cleaning Data/UCI HAR Dataset")

#download packages needed that have already been installed.
library(plyr)
library(dplyr)

#Read all the files first.
subjectTest = read.table("subject_test.txt")
yTest = read.table("y_test.txt") 
xTest = read.table("X_test.txt") #Contains measurements in test group
featuresDF = read.table("features.txt")
subjectTrain = read.table("subject_train.txt")
yTrain = read.table("y_train.txt")
xTrain = read.table("X_train.txt") #Contains measurements in train group
activityLabel = read.table("activity_labels.txt")

#################################################################################
#The next set of codes rename and extract certain columns, based on the project #
#requirements.                                                                  #
#################################################################################

#The xTest & xTrain contain the measurements for the test and train subjects.
#featuresDF contains the column names of the measurements. Paste the variable
#names to the xTest & xTrain and select only mean and standard deviation columns 
colnames(xTest) <- featuresDF[, 2] #Subsetted 2nd col of featuresDF pasted to the first file as col names
colnames(xTrain) <- featuresDF[, 2] #Subsetted 2nd col of featuresDF pasted to the first file as col names
xTestMean <- xTest[,grepl("mean", colnames(xTest))] #get cols that only have "mean" in col name
xTrainMean <- xTrain[,grepl("mean", colnames(xTrain))] #get cols that only have "mean" in col name
xTestStd <- xTest[,grepl("std", colnames(xTest))] #get cols that only have "std" in col name
xTrainStd <- xTrain[,grepl("std", colnames(xTest))] #get cols that only have "std" in col name

#Delete objects I no longer need to make space in the global environment
xTest <- NA
xTrain <- NA
featuresDF <- NA

#subjectTest & subjectTrain contain subject ID #s. Rename cols V1 in each one to SubjectID
subjectTest <- dplyr::rename(subjectTest, SubjectID = V1)
subjectTrain <- dplyr::rename(subjectTrain, SubjectID = V1)

#yTest & yTrain contain activity type by using numbers 1-6 for each subject.
#Rename the column to "Activity"
yTest <- dplyr::rename(yTest, Activity = V1)
yTrain <- dplyr::rename(yTrain, Activity = V1)


#####################################################
#The next set of codes bind and join files/objects. #
#####################################################

#Column bind all means and standard deviations for test and train. Then, row
#bind those objects into 1 object.
xTestMeanStd <- cbind(xTestMean,xTestStd) #Binds by col
xTrainMeanStd <- cbind(xTrainMean, xTrainStd) #Binds by col
xTestTrainMeanStd <- rbind(xTestMeanStd, xTrainMeanStd) #Binds 1st and 2nd object by row
#Delete objects I no longer need to make space in the global environment
xTestMean <- NA
xTestStd <- NA
xTrainMean <- NA
xTrainStd <- NA
xTestMeanStd <- NA
xTrainMeanStd <- NA

#Rename column with activity # to be "Activity". Then join activityLabel with 2 objects
#that have activity numbers for test and train
activityLabel <- dplyr::rename(activityLabel, Activity = V1) #Rename the 1st col. This col will be used for the join below. 
yTestLabel = join(yTest, activityLabel, type = "full") #Join these 2 objects so that activity names can be matched up to activity numbers on yTestRename 
yTrainLabel = join(yTrain, activityLabel, type = "full") #Join these 2 objects so that activity names can be matched up to activity numbers on yTestRename
#Delete objects I no longer need to make space in the global environment
activityLabel <- NA
yTest <- NA
yTrain <- NA

#Bind activity label objects, remove the col with numbers and rename the colume with labels I want to keep
yTestTrainLabel <- rbind(yTestLabel, yTrainLabel) #bind test & train rows with activity labels and numbers
yTestTrainLabel <- select(yTestTrainLabel, -Activity) #Take out the column with numbers that I used for the join
yTestTrainActivity <- dplyr::rename(yTestTrainLabel, Activity = V2) #Now have the right label for this col
#Delete objects I no longer need to make space in the global environment
yTestLabel <- NA
yTrainLabel <- NA
yTestTrainLabel <- NA

#bind all Subject IDs
AllSubjectIDs <- rbind(subjectTest, subjectTrain)
#Delete objects I no longer need to make space in the global environment
subjectTest <- NA
subjectTrain <- NA

#Column bind IDs and activity labels
IDandLabel <- cbind(AllSubjectIDs, yTestTrainActivity) #bind IDs to labels
#Delete objects I no longer need to make space in the global environment
AllSubjectIDs <- NA
yTestTrainActivity <- NA

#Last bind: column bind object with IDs and labels with object with Mean & Std measurements
#for all participants
final <- cbind(IDandLabel, xTestTrainMeanStd) #This has 10299 obs & 81 variables
#Delete objects I no longer need to make space in the global environment
IDandLabel <- NA
xTestTrainMeanStd <- NA

#############################################################################################
#Now, write a table with the above data frame with means for each activity for each subject.#
#Each row will represent a different activity for each participant, so each participant will#
#have 6 rows. I will use a wide-tidy, as opposed to a long-tidy, format for the data frame. #
#############################################################################################

finalAverages <- ddply(final,  c('SubjectID','Activity'), numcolwise(mean)) #requires plyr. 180 rows and 81 columns
write.table(finalAverages, file = "finalAverages.txt", row.names=FALSE) #create TXT file with data