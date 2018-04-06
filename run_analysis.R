run_analysis<-function(wd){
  setwd(wd)
  
  #-----------------------------------------------------------------
  # 0. Downloding and extracting raw data
  #-----------------------------------------------------------------
  
  url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  zip_name<-"UCI HAR Dataset.zip"
  path<-file.path(getwd(),zip_name)
  if (!file.exists(path)){
    download.file(url,path)
    unzip(zip_name)
  }
  
  #-----------------------------------------------------------------
  # 1. Merges the training and the test sets to create one data set.
  #-----------------------------------------------------------------
  
  ###Getting Train Set tables
  x_trainSet<-read.table("UCI HAR Dataset/train/X_train.txt")
  y_trainSet<-read.table("UCI HAR Dataset/train/y_train.txt")
  sub_trainSet<-read.table("UCI HAR Dataset/train/subject_train.txt")
  
  ###Getting Test Set tables
  x_testSet<-read.table("UCI HAR Dataset/test/X_test.txt")
  y_testSet<-read.table("UCI HAR Dataset/test/y_test.txt")
  sub_testSet<-read.table("UCI HAR Dataset/test/subject_test.txt")
  
  ###Merging datasets into one and adding aswell
  ###"activity" and "subject" data per observation.
  x_final_set<-rbind(x_testSet,x_trainSet)
  y_final_set<-rbind(y_testSet,y_trainSet)
  sub_final_set<-rbind(sub_testSet,sub_trainSet)
  merge_set<-cbind(x_final_set,y_final_set,sub_final_set)
  ##Getting Feautures Names
  featuresNames<-read.table(("UCI HAR Dataset/features.txt"))
  ##Getting a correspondency table betwen ID and Name activities.
  activityName<- read.table("UCI HAR Dataset/activity_labels.txt")
  
  #-----------------------------------------------------------------
  #2. Extracts only the measurements on the mean and standard 
  #   deviation for each measurement.
  #-----------------------------------------------------------------
  
  #Getting a logical vector that identifies the elements with "mean" and "SD"
  validFeatures<-grepl("mean\\(\\)|std\\(\\)", featuresNames[,2])
  #Extractiong the columns on data set that have "mean" and "SD" info 
  merge_fltr_set<-merge_set[,validFeatures]
  #Making correspondency between the activity's ID and its name in order to 
  #change the id value on "activity" column for its name.
  merge_fltr_set[,67]<-activityName[,2][match(merge_fltr_set[,67],activityName[,1])]
  
  #-----------------------------------------------------------------
  #3. Uses descriptive activity names to name the activities in the 
  #	  data set.
  #-----------------------------------------------------------------
  
  filter_feat<-as.character(featuresNames[validFeatures,2])
  
  filter_feat <- gsub('Mag',"Magnitude",filter_feat)
  filter_feat <- gsub('Acc',"Acceleration",filter_feat)
  filter_feat <- gsub('Gyro',"AngularSpeed",filter_feat)
  filter_feat <- gsub('^t',"TimeDom.",filter_feat)
  filter_feat <- gsub('\\.mean',".Mean",filter_feat)
  filter_feat <- gsub('Freq\\.',"Frequency.",filter_feat)
  filter_feat <- gsub('Freq$',"Frequency",filter_feat)
  filter_feat <- gsub('GyroJerk',"AngularAcceleration",filter_feat)
  filter_feat <- gsub('\\.std',".StandardDeviation",filter_feat)
  filter_feat <- gsub('^f',"FrequencyDom.",filter_feat)
  
  #-----------------------------------------------------------------
  #4. Appropriately labels the data set with descriptive activity 
  #   names.
  #-----------------------------------------------------------------
  
  names(merge_fltr_set)<-c(filter_feat,"Activity","Subject")
  
  #-----------------------------------------------------------------
  #5. Creates a second, independent tidy data set with the average 
  #   of each variable for each activity and each subject
  #-----------------------------------------------------------------
  tidyData <- aggregate(. ~ Subject + Activity, merge_fltr_set, mean)
  
  write.table(tidyData, file = "tidydata.txt",row.name=FALSE)
  
  
}