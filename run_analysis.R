# Imported data from features.txt into ColNames.csv
# Reading ColNames.csv csv file into data frame, dfColNames
fileColNames <- paste(getwd(), "/UCI HAR Dataset/ColNames.csv", sep="")
dfColNames <- read.csv(fileColNames, header=F)
ColumnNames <- as.character(dfColNames[,1])
uniq <- make.unique(ColumnNames)

# Reading data set, X_train.txt into data frame
fileTrain <- paste(getwd(), "/UCI HAR Dataset/train/X_train.txt", sep="")
dfTrain <- read.table(fileTrain, header=F, col.names=uniq)

# Reading data set, X_test.txt into data frame
fileTest <- paste(getwd(), "/UCI HAR Dataset/test/X_test.txt", sep="")
dfTest <- read.table(fileTest, header=F, col.names=uniq)

# Reading data set, subject_test.txt into data frame
fileSubjectTest <- paste(getwd(), "/UCI HAR Dataset/test/subject_test.txt", sep="")
dfSubjectTest <- read.table(fileSubjectTest, header=F)

# Reading data set, y_train.txt into data frame
fileSubjectTrain <- paste(getwd(), "/UCI HAR Dataset/train/subject_train.txt", sep="")
dfSubjectTrain <- read.table(fileSubjectTrain, header=F)

# Reading data set, activity_labels.txt into data frame
fileActivityName <- paste(getwd(), "/UCI HAR Dataset/activity_labels.txt", sep="")
dfActivityNames <- read.table(fileActivityName, header=F)

# Reading data set, y_test.txt into data frame
fileActivityTest <- paste(getwd(), "/UCI HAR Dataset/test/y_test.txt", sep="")
dfActivityTest <- read.table(fileActivityTest, header=F)

# Reading data set, y_train.txt into data frame
fileActivityTrain <- paste(getwd(), "/UCI HAR Dataset/train/y_train.txt", sep="")
dfActivityTrain <- read.table(fileActivityTrain, header=F)

# Replace the test activity indices with Activity Labels(activity_labels.txt)
activityTest1 <- gsub("1", dfActivityNames[1,2], dfActivityTest$V1, fixed=TRUE )
activityTest2 <- gsub("2", dfActivityNames[2,2], activityTest1, fixed=TRUE )
activityTest3 <- gsub("3", dfActivityNames[3,2], activityTest2, fixed=TRUE )
activityTest4 <- gsub("4", dfActivityNames[4,2], activityTest3, fixed=TRUE )
activityTest5 <- gsub("5", dfActivityNames[5,2], activityTest4, fixed=TRUE )
activityTestFinal <- gsub("6", dfActivityNames[6,2], activityTest5, fixed=TRUE )

# Replace the train activity indices with Activity Labels(activity_labels.txt)
activityTrain1 <- gsub("1", dfActivityNames[1,2], dfActivityTrain$V1, fixed=TRUE )
activityTrain2 <- gsub("2", dfActivityNames[2,2], activityTrain1, fixed=TRUE )
activityTrain3 <- gsub("3", dfActivityNames[3,2], activityTrain2, fixed=TRUE )
activityTrain4 <- gsub("4", dfActivityNames[4,2], activityTrain3, fixed=TRUE )
activityTrain5 <- gsub("5", dfActivityNames[5,2], activityTrain4, fixed=TRUE )
activityTrainFinal <- gsub("6", dfActivityNames[6,2], activityTrain5, fixed=TRUE )

dfTest$subject <- dfSubjectTest[,1]
dfTrain$subject <- dfSubjectTrain[,1]

dfTest$activity <- activityTestFinal
dfTrain$activity <- activityTrainFinal

# Merging the test and training data frames
dfTotal <- rbind(dfTrain, dfTest)

# Checking for missing values in the data frame
dfFinalComp <- dfTotal[complete.cases(dfTotal),]

# Step 2 in Course Project
##############################   start   ###############################
# Retrieving the column names with pattern, mean()
patmean <- "mean()"
colmean <- grep(patmean, uniq, fixed=TRUE, invert=FALSE)

# Retrieving the column names with pattern, std()
patstd <-  "std()"
colstd <- grep(patstd, uniq, fixed=TRUE, invert=FALSE)

# Creating final numeric vector with indices of columns with mean() or std()
colTotal <- colmean
colTotal <- append(colTotal, colstd)
colFinal <- append(colTotal, c(562, 563))
# Subsetting data frame with above columns
dfFinal <- dfFinalComp[,colfinal]
##############################   end   ###############################

# Source: https://leemendelowitz.github.io/blog/tapply-and-split.html
# Split the data frame by activity(68) and subject(67)
spltFinal <- split(dfFinal, dfFinal[,68:67])

# Define function to get column means of all columns except activity and subject
takeMeans <- function(x) colMeans(dfFinal[, 1:66], na.rm = TRUE)

# Apply column mean function, takeMeans to data frame split by factors
finalOutPutMatrix <- sapply(spltFinal, takeMeans)

#Convert matrix to data frame
dfFinalOutPut <- data.frame(finalOutPutMatrix)

# Get the row names of the dat frame which are the column names(variables) 
Variables <- rownames(dfFinalOutPut)

# Insert first column as column names(variables) followed by remaining variables 
finalOutPut <- cbind(Variables, dfFinalOutPut)

# Write output 
write.table(finalOutPut, file="CourseProject_GettingAndSavingData.txt", row.names=FALSE, quote=FALSE)
