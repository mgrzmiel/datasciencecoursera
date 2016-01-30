# GETTING AND CLEANING DATA 
# PROJECT
# Author: Magdalena Grzmiel
# 01/265/2016


# load required library
library(plyr)


# read the test data
train_data<-read.table("/Users/magda/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset/train/X_train.txt", 
                         header=FALSE)

# read the train data
test_data<-read.table("/Users/magda/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset/test/X_test.txt", 
                      header=FALSE)

# bind the test and train data together
all_data <-rbind(train_data, test_data)

# read the names
names<-read.table("/Users/magda/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset/features.txt", 
                                 header=FALSE)

#get only the names - the second column
names<-names[, 2]
# convert to a vector
names <- as.vector(names)

# rename each column in data frame to the names from the file
for(i in 1:ncol(all_data)) {
  colnames(all_data)[i] <- names[i]
}

# read the subject data 
# train data
train_subject<-read.table("/Users/magda/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset/train/subject_train.txt", 
                          header=FALSE)

# test data
test_subject<-read.table("/Users/magda/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset/test/subject_test.txt", 
                         header=FALSE)

# bind the test and training subject datasets
all_subject <-rbind(train_subject, test_subject)

# read the activity datasets
# train data
train_activity<-read.table("/Users/magda/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset/train/y_train.txt", 
                          header=FALSE)

# test data
test_activity<-read.table("/Users/magda/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset/test/y_test.txt", 
                           header=FALSE)

# bind the test and training data together
all_activity <-rbind(train_activity, test_activity)

# add the subject and activity type to the dataset
all_data<-cbind(all_activity, all_data)
all_data<-cbind(all_subject, all_data)

# rename the first two coulmns to subject_no and activity_type
colnames(all_data)[1] <- 'subjectNo'
colnames(all_data)[2] <- 'activityType'

# extracts only the measurements of the mean and standard deviation for each measurement
# get the names to see which coulms should be extracted
names(all_data)
# extract the following columns
# 1 - subject_no
# 2 - activity_type
# 3 - tBodyAcc-mean()-X
# 4 - tBodyAcc-mean()-Y
# 5 - tBodyAcc-mean()-Z
# 6 - tBodyAcc-std()-X
# 7 - tBodyAcc-std()-Y
# 8 - tBodyAcc-std()-Z

first_data_set <- all_data[, c(1:8)]

# Replace the number with activity type to descriptive activity names
# get the names from the file
activities_name<-read.table("/Users/magda/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset/activity_labels.txt", 
                            header=FALSE)

# conver the names to the vector
activities_names<-as.vector(activities_name[,2])

# replace the activity type number to descriptive name
for (i in 1:length(activities_names)){
  first_data_set$activityType[first_data_set$activityType==i] <- activities_names[i]
}

# rename the columns 
colnames(first_data_set)[3] <- "tBodyAccelerationMeanX"
colnames(first_data_set)[4] <- "tBodyAccelerationMeanY"
colnames(first_data_set)[5] <- "tBodyAccelerationMeanZ"
colnames(first_data_set)[6] <- "tBodyAccelerationStdX"
colnames(first_data_set)[7] <- "tBodyAccelerationStdY"
colnames(first_data_set)[8] <- "tBodyAccelerationStdZ"
head(first_data_set)

# conver the activity type to factor
first_data_set$activity_type<-as.factor(first_data_set$activityType)

# save the dataframe to txt file
write.table(first_data_set,
            file="/Users/magda/Desktop/Coursera/GettingAndCleaningData/datasciencecoursera/Course#3Assignment/avg_and_std_body_acc.txt",
            row.names = FALSE)

# create teh list to keep the avg for each variable for each activity for each subject
avg_for_each_variable<-vector(mode='list', length=6)

# calc average of each variable for each activity and each subject.
avg_for_each_variable[[1]]<-ddply(first_data_set, .(activityType,subjectNo),summarise,avgtBodyAccMeanX=mean(tBodyAccelerationMeanX))
avg_for_each_variable[[2]]<-ddply(first_data_set, .(activityType,subjectNo),summarise,avgtBodyAccMeanY=mean(tBodyAccelerationMeanY))
avg_for_each_variable[[3]]<-ddply(first_data_set, .(activityType,subjectNo),summarise,avgtBodyAccMeanZ=mean(tBodyAccelerationMeanZ))
avg_for_each_variable[[4]]<-ddply(first_data_set, .(activityType,subjectNo),summarise,avgtBodyAccStdX=mean(tBodyAccelerationStdX))
avg_for_each_variable[[5]]<-ddply(first_data_set, .(activityType,subjectNo),summarise,avgtBodyAccStdY=mean(tBodyAccelerationStdY))
avg_for_each_variable[[6]]<-ddply(first_data_set, .(activityType,subjectNo),summarise,avgtBodyAccStdZ=mean(tBodyAccelerationStdZ))

# put the data into one tide data sets
second_data_set<-join_all(avg_for_each_variable, by = c("activityType", "subjectNo"), type = "left", match = "all")
#str(second_data_set)

# save the data frame to csv file
write.table(second_data_set,
            file="/Users/magda/Desktop/Coursera/GettingAndCleaningData/datasciencecoursera/Course#3Assignment/avg_body_acc_per_activity_type_per_subject.txt",
            row.names = FALSE)

head(second_data_set)