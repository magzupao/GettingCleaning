#run console, example:
#>executeAnalysis()
executeAnalysis <- function() {
  
  #1. Merges the training and the test sets to create one data set.
  # 
  # Folder structure and files.
  # >dir()
  # >Project_UCI HAR_Dataset/test/subject_test.txt
  # >Project_UCI HAR_Dataset/test/X_test.txt
  # >Project_UCI HAR_Dataset/test/y_test.txt
  # >Project_UCI HAR_Dataset/train/subject_train.txt
  # >Project_UCI HAR_Dataset/train/X_train.txt
  # >Project_UCI HAR_Dataset/train/y_train.txt
  #
  # load data 
  print("******************* load data ************************")
  dataSubjectTrain <- read.table("Project_UCI HAR_Dataset/train/subject_train.txt",header = FALSE)
  dataSubjectTest  <- read.table("Project_UCI HAR_Dataset/test/subject_test.txt",header = FALSE)
  
  dataActivityTest  <- read.table("Project_UCI HAR_Dataset/test/y_test.txt",header = FALSE)
  dataActivityTrain <- read.table("Project_UCI HAR_Dataset/train/y_train.txt",header = FALSE)
  
  dataFeaturesTest  <- read.table("Project_UCI HAR_Dataset/test/X_test.txt",header = FALSE)
  dataFeaturesTrain <- read.table("Project_UCI HAR_Dataset/train/X_train.txt",header = FALSE)
  
  #Merge subject, train and test
  dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
  dataActivity<- rbind(dataActivityTrain, dataActivityTest)  
  dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)
  print("******************** END ***********************")

  #add title head
  colnames(dataSubject) <- "subject"					
  colnames(dataActivity) <- "activity"				
  dataFeaturesNames <- read.table("Project_UCI HAR_Dataset/features.txt",head=FALSE)				  
  colnames(dataFeatures) <- dataFeaturesNames$V2			

  mergeColData <- cbind(dataFeatures,dataActivity,dataSubject)
				
  #We load only the columns that have mean y std
  colWithMeanSTD <- grep(".*mean.*|.*std.*", names(mergeColData), ignore.case=TRUE)				
  requiredColumns <- c(colWithMeanSTD, 562, 563)
				
  extractedData <- mergeColData[,requiredColumns]
  #dim(extractedData)
				
  #We change the numerical value of "activity" column by its description, example 5 = STANDING
  activityLabels <- read.table("Project_UCI HAR_Dataset/activity_labels.txt",header = FALSE)
  #head(activityLabels$V2[extractedData$activity],30)
  extractedData$activity <- head(activityLabels$V2[extractedData$activity],length(extractedData$activity))			
				
  #We change the letter that is in each column for each description, example t = time
  names(extractedData)<-gsub("^t", "time", names(extractedData))
  names(extractedData)<-gsub("^f", "frequency", names(extractedData))
  names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
  names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
  names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
  names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
				
  names(extractedData)
				
  #We create a data set ordered subject
  Data<-aggregate(. ~subject + activity, extractedData, mean)
  Data<-Data[order(Data$subject,Data$activity),]
  write.table(Data, file = "tidydata.txt",row.name=FALSE)


}
