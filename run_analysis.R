# Merges the training and the test sets to create one data set.

# This function takes a folder name and returns it's contents in 
# a merged human readable data.frame with the following data:
#     mean                      -> (mean_x,mean_y,mean_z)
#     standard deviation        -> (std_x, std_y, std_z)
#     activity                  -> The activity (standing, laying...)
#     group                     -> Data group (test or train)
#     subject                   -> The subject
getMergedFolder <- function(folder)
{
  #Gets a file names list to load
  fileList <- list.files(folder, pattern=paste("*_", folder, sep=""), 
                         full.names=TRUE)
  
  #Gets the file indexes on the file list
  fileIndex.subject <- fileList[grep("subject", 
                                     fileList, ignore.case=T)]
  fileIndex.x <- fileList[grep("x_", fileList, ignore.case=T)]
  fileIndex.y <- fileList[grep("y_", fileList, ignore.case=T)]
  
  #Loads the file throught their indexes
  test.subject <- read.table(fileIndex.subject)
  test.x <- read.table(fileIndex.x)
  test.y <- read.table(fileIndex.y)
  
  # Uses descriptive activity names to name the activities in 
  # the data set
  act <- read.table("activity_labels.txt", 
                    col.names=c("index", "act"), 
                    stringsAsFactors=FALSE)
  
  #Replace numbers with descriptions
  test.y[,1] <- mapply(function(x){ act[x,2] }, test.y[,1])
  test.y <- as.factor(test.y[,1])
  
  # Extracts only the measurements on the mean and standard 
  #   deviation for each measurement.
  test <- test.x[,1:6]
  test <- cbind(test, test.y)
  
  #Creates a factor column of test and train groups
  f <- gl(1,length(test[,1]),labels=folder)
  test <- cbind(test,f)
  
  #Adds the subject to the result
  test <- cbind(test, as.factor(test.subject[,1]))
  
  # Appropriately labels the data set with descriptive names
  columNames <- c("mean_x", "mean_y", "mean_z", "std_x", 
                  "std_y", "std_z", "activity", "group", "subject")
  colnames(test) <- columNames
  
  return(test)
}

############ Merged files ############
test <- getMergedFolder("test")
train <- getMergedFolder("train")

final <- test
final <- rbind(final, train)

# Creates a second, independent tidy data set with the average of each 
#   variable for each activity and each subject. 
summary <- aggregate(final[,1:6],
                  by=list(final$subject, final$activity),
                  FUN=mean)

colnames(summary) <- c("subject","activity", 
                       colnames(summary)[3:length(summary[1,])])

#Delete the unused variables
rm(test)
rm(train)