# GACDWeek4

Steps used to create the dataset
#_________________________________________________________________________
# Task 1: Merges the training and the test sets to create one data set.
#_________________________________________________________________________
# Step 1.a Read the X_train and X_test datasets into R objects.
train_data<- as.data.frame(read.delim(file = "X_train.txt",sep = "",header = FALSE)) #converting the "training" dataset into a data frame and saving into the object "train_data".
test_data <- as.data.frame(read.delim(file = "X_test.txt", sep = "",header = FALSE)) #converting the "test" dataset into a data frame and saving into the object "test_data".

# STEP 1b. Assign column Names to the "training" and "test" data frames. 
# The column names of each measurement is present in the "features.txt" file that came with the data set.
data_column_names <- as.data.frame(read.delim("features.txt",sep = " ",header = FALSE,col.names = c("No.","labels")))
data_column_names$labels <- gsub(data_column_names$labels,pattern = ",",replacement = "")
colnames(train_data) <- data_column_names$labels
colnames(test_data) <- data_column_names$labels

# step 1c. adding an indicator column at the end to keep track of whether a given row is 
# part of the "training" or "test" dataset after they are mereded together.
dataset_type <- data.frame()
dataset_type  <- rep("training",times = nrow(train_data))
train_data_final <- cbind(train_data,dataset_type)

dataset_type <- data.frame()
dataset_type <- rep("test",times = nrow(test_data))
test_data_final <- cbind(test_data,dataset_type)

# step 1e: Final Step : merge the training and test datasets into one.
combined_data_set <- rbind(train_data_final,test_data_final)
write.csv(combined_data_set,"combined_data_set.csv")

#_________________________________________________________________________________
#TASk 2: Extracts only the measurements on the mean and standard deviation for each measurement.
#_________________________________________________________________________________
# step 1a: Extract only measurements related tro mean and standard deviation.
# note: this should *exclude* measurements named "meanFreq()" - because that is not stricly a "simple" mean - it is a weighted average.
combined_data_set <- combined_data_set[, grepl("mean()",names(combined_data_set)) | 
                                         grepl("std()" ,names(combined_data_set)) | 
                                         grepl("dataset_type",names(combined_data_set))] #include mean() and std()
combined_data_set <- combined_data_set[, !grepl("meanFreq",names(combined_data_set))] #remove meanFreq() because the question asks only for mean().
write.csv(combined_data_set,"combined_data_set.csv")

#_________________________________________________________________________________
#TASk 3: Uses descriptive activity names to name the activities in the data set
#_________________________________________________________________________________

#Step 3a. Extract Activity Indicators from the files "y_train.txt" and "y_test.txt"

train_labels <- as.data.frame(read.delim(file = "y_train.txt",sep = "",header = FALSE))
test_labels <- as.data.frame(read.delim(file = "y_test.txt",sep = "",header = FALSE))

colnames(train_labels) <- "activitylabel"
colnames(test_labels) <- "activitylabel"

combined_labels <- rbind(train_labels, test_labels)

#The question/tasks asks that the activity labels which are numbered with 1,2,3,4,5 and 6 be repaced with descriptive labels like "WALKING", WALKING_UPSTAIRS" etc.
# These descriptive labels are available in the file "activity_labels.txt" which came with the data set.

combined_labels$activitylabel <- gsub(combined_labels$activitylabel,pattern = "1",replacement = "WALKING")
combined_labels$activitylabel <- gsub(combined_labels$activitylabel,pattern = "2",replacement = "WALKING_UPSTAIRS")        
combined_labels$activitylabel <- gsub(combined_labels$activitylabel,pattern = "3",replacement = "WALKING_DOWNSTAIRS")
combined_labels$activitylabel <- gsub(combined_labels$activitylabel,pattern = "4",replacement = "SITTING")
combined_labels$activitylabel <- gsub(combined_labels$activitylabel,pattern = "5",replacement = "STANDING")
combined_labels$activitylabel <- gsub(combined_labels$activitylabel,pattern = "6",replacement = "LAYING")

combined_data_set2 <- cbind(combined_labels, combined_data_set)
colnames(combined_data_set2)

#_________________________________________________________________________________
#TASk 4:  Appropriately labels the data set with descriptive variable names.
#_________________________________________________________________________________
#The file descriptive.csv was created manually by this author in order to map each non-descriptive nem with a descriptive one.
#The colnames() function was used to replace the existing column names with descriptive ones.
descriptive_names <- c("activitylabel","Mean of Body Acceleration X Axis","Mean of Body Acceleration Y Axis","Mean of Body Acceleration Z Axis","Standard Deviation of Body Acceleration X Axis","Standard Deviation of Body Acceleration Y Axis","Standard Deviation of Body Acceleration Z Axis","Mean of Gravity Acceleration X Axis","Mean of Gravity Acceleration Y Axis","Mean of Gravity Acceleration Z Axis","Standard Deviation of of Gravity Acceleration X Axis","Standard Deviation of of Gravity Acceleration Y Axis","Standard Deviation of of Gravity Acceleration Z Axis","Mean of Body Acceleration Jerk X Axis","Mean of Body Acceleration Jerk Y Axis","Mean of Body Acceleration Jerk Z Axis","Standard Deviation of Body Acceleration Jerk X Axis","Standard Deviation of Body Acceleration Jerk Y Axis","Standard Deviation of Body Acceleration Jerk Z Axis","Mean of Gravity Acceleration Jerk X Axis","Mean of Gravity Acceleration Jerk Y Axis","Mean of Gravity Acceleration Jerk Z Axis","Standard Deviation of of Body Gyro  X Axis","Standard Deviation of of Body Gyro  Y Axis","Standard Deviation of of Body Gyro  Z Axis","Mean of Body Gyro Jerk Jerk X Axis","Mean of Body Gyro Jerk Jerk Y Axis","Mean of Body Gyro Jerk Jerk Z Axis","Standard Deviation of of Body Gyro Jerk Jerk X Axis","Standard Deviation of of Body Gyro Jerk Jerk Y Axis","Standard Deviation of of Body Gyro Jerk Jerk Z Axis","Mean of Body Acceleration Magnitude","Standard Deviation of Body Acceleration Magnitude","Mean of Gravity Acceleration Magnitude","Standard Deviation of Gravity Acceleration Magnitude","Mean of Body Acceleration Jerk Magnitude","Standard Deviation of Body Acceleration Jerk Magnitude","Mean of Body Gyro Magnitude Magnitude","Standard Deviation of Body Gyro Magnitude Magnitude","Mean of Body Gyro Jerk Magnitude Magnitude","Standard Deviation of Body Gyro Jerk Magnitude Magnitude","Fast Fourier transform - Mean of Body Acceleration X Axis","Fast Fourier transform - Mean of Body Acceleration Y Axis","Fast Fourier transform - Mean of Body Acceleration Z Axis","Fast Fourier transform - Standard Deviation of Body Acceleration X Axis","Fast Fourier transform - Standard Deviation of Body Acceleration Y Axis","Fast Fourier transform - Standard Deviation of Body Acceleration Z Axis","Fast Fourier transform - Mean of Body Acceleration Jerk X Axis","Fast Fourier transform - Mean of Body Acceleration Jerk Y Axis","Fast Fourier transform - Mean of Body Acceleration Jerk Z Axis","Fast Fourier transform - Standard Deviation of Body Acceleration Jerk X Axis","Fast Fourier transform - Standard Deviation of Body Acceleration Jerk Y Axis","Fast Fourier transform - Standard Deviation of Body Acceleration Jerk Z Axis","Fast Fourier transform - Mean of Body Gyro X Axis","Fast Fourier transform - Mean of Body Gyro Y Axis","Fast Fourier transform - Mean of Body Gyro Z Axis","Fast Fourier transform - Standard Deviation of Body Gyro X Axis","Fast Fourier transform - Standard Deviation of Body Gyro Y Axis","Fast Fourier transform - Standard Deviation of Body Gyro Z Axis","Fast Fourier transform - Mean of Body Acceleration Magnitude","Fast Fourier transform - Standard Deviation  of Body Acceleration Magnitude","Fast Fourier transform - Mean of Body Acceleration Jerk Magnitude","Fast Fourier transform - Standard Deviation of Body Acceleration Jerk Magnitude","Fast Fourier transform - Mean of Body Gyro Magnitude Magnitude","Fast Fourier transform - Standard Deviation of Body Gyro Magnitude Magnitude","Fast Fourier transform - Mean of Body Gyro Jerk Magnitude Magnitude","Fast Fourier transform - Standard Deviation of Body Gyro Jerk Magnitude Magnitude","dataset_type")
length(descriptive_names)
colnames(combined_data_set2)<- descriptive_names
colnames(combined_data_set2)

str(colnames(combined_data_set2))

#_________________________________________________________________________________
#TASk 5:  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#Good luck!
#_________________________________________________________________________________
#Step 5a. Before performing thisd step, we need to integrate the "subject" information into the final dataset.
#This information is contained in the files subject_test.txt and subject_train.txt.

subject_train <- data.frame(read.csv("subject_train.txt",header = FALSE))
subject_test <- data.frame(read.csv("subject_test.txt",header = FALSE))

colnames(subject_train) <- "subject"
colnames(subject_test) <- "subject"

subject_all <- rbind(subject_train,subject_test)
combined_data_set3 <- cbind(subject_all,combined_data_set2)

#dropping the last column 
combined_data_set3$dataset_type <- NULL

#generate the means for each value and create a new tidy dataset.
final_tidy_dataset <- aggregate(combined_data_set3, by=list(combined_data_set3$subject,combined_data_set3$activitylabel),FUN = mean, na.rm=TRUE)
final_tidy_dataset$Group.1 <- NULL
final_tidy_dataset$activitylabel <- NULL
colnames(final_tidy_dataset)[1] <- "activitylable"

write.table(final_tidy_dataset,"final tidy dataset.txt",row.name=FALSE)
