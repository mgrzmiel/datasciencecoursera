# GETTING AND CLEANING DATA 
# PROJECT
# Author: Magdalena Grzmiel
# 01/265/2016


# load required library
library(plyr)


# read the test data
train_data<-read.table("/Users/magda/Desktop/GettingAndCleaningData/UCI HAR Dataset/train/X_train.txt", 
                         header=FALSE)

# read the train data
test_data<-read.table("/Users/magda/Desktop/GettingAndCleaningData/UCI HAR Dataset/test/X_test.txt", 
                      header=FALSE)

# bind the test and train data together
all_data <-rbind(train_data, test_data)

# read the names
names<-read.table("/Users/magda/Desktop/GettingAndCleaningData/UCI HAR Dataset/features.txt", 
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
train_subject<-read.table("/Users/magda/Desktop/GettingAndCleaningData/UCI HAR Dataset/train/subject_train.txt", 
                          header=FALSE)

# test data
test_subject<-read.table("/Users/magda/Desktop/GettingAndCleaningData/UCI HAR Dataset/test/subject_test.txt", 
                         header=FALSE)

# bind the test and training subject datasets
all_subject <-rbind(train_subject, test_subject)

# read the activity datasets
# train data
train_activity<-read.table("/Users/magda/Desktop/GettingAndCleaningData/UCI HAR Dataset/train/y_train.txt", 
                          header=FALSE)

# test data
test_activity<-read.table("/Users/magda/Desktop/GettingAndCleaningData/UCI HAR Dataset/test/y_test.txt", 
                           header=FALSE)

# bind the test and training data together
all_activity <-rbind(train_activity, test_activity)

# add the subject and activity type to the dataset
all_data<-cbind(all_activity, all_data)
all_data<-cbind(all_subject, all_data)

# rename the first two coulmns to subject_no and activity_type
colnames(all_data)[1] <- 'subject_no'
colnames(all_data)[2] <- 'activity_type'

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
activities_name<-read.table("/Users/magda/Desktop/GettingAndCleaningData/UCI HAR Dataset/activity_labels.txt", 
                            header=FALSE)

# conver the names to the vector
activities_names<-as.vector(activities_name[,2])

# replace the activity type number to descriptive name
for (i in 1:length(activities_names)){
  first_data_set$activity_type[first_data_set$activity_type==i] <- activities_names[i]
}

# rename the columns 
colnames(first_data_set)[3] <- "tBodyAcc_mean_X"
colnames(first_data_set)[4] <- "tBodyAcc_mean_Y"
colnames(first_data_set)[5] <- "tBodyAcc_mean_Z"
colnames(first_data_set)[6] <- "tBodyAcc_std_X"
colnames(first_data_set)[7] <- "tBodyAcc_std_Y"
colnames(first_data_set)[8] <- "tBodyAcc_std_Z"
nrow(first_data_set)

# conver the activity type to factor
first_data_set$activity_type<-as.factor(first_data_set$activity_type)

# save the dataframe to txt file
write.table(first_data_set,file="/Users/magda/Desktop/GettingAndCleaningData/avg_and_std_body_acc.txt",
            row.names = FALSE)

# create teh list to keep the avg for each variable for each activity for each subject
avg_for_each_variable<-vector(mode='list', length=6)

# calc average of each variable for each activity and each subject.
avg_for_each_variable[[1]]<-ddply(first_data_set, .(activity_type,subject_no),summarise,avg_tBodyAcc_mean_X=mean(tBodyAcc_mean_X))
avg_for_each_variable[[2]]<-ddply(first_data_set, .(activity_type,subject_no),summarise,avg_tBodyAcc_mean_Y=mean(tBodyAcc_mean_Y))
avg_for_each_variable[[3]]<-ddply(first_data_set, .(activity_type,subject_no),summarise,avg_tBodyAcc_mean_Z=mean(tBodyAcc_mean_Z))
avg_for_each_variable[[4]]<-ddply(first_data_set, .(activity_type,subject_no),summarise,avg_tBodyAcc_std_X=mean(tBodyAcc_std_X))
avg_for_each_variable[[5]]<-ddply(first_data_set, .(activity_type,subject_no),summarise,avg_tBodyAcc_std_Y=mean(tBodyAcc_std_Y))
avg_for_each_variable[[6]]<-ddply(first_data_set, .(activity_type,subject_no),summarise,avg_tBodyAcc_std_Z=mean(tBodyAcc_std_Z))

# put the data into one tide data sets
second_data_set<-join_all(avg_for_each_variable, by = c("activity_type", "subject_no"), type = "left", match = "all")
#str(second_data_set)

# save the data frame to csv file
write.table(second_data_set,file="/Users/magda/Desktop/GettingAndCleaningData/avg_body_acc_per_activity_type_per_subject.txt",
            row.names = FALSE)
