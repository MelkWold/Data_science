#Question 1: Merging the "training" and "test" data sets
# First let's load the respective libraries to be used in the analysis
library(dplyr)
library(data.table)
# Load the files from the "test" folder into R, assign names to them and check their respective dimensions.
subject_test<-read.csv("subject_test.txt", header= F)
names(subject_test) <-"subject"
y_test<-read.csv("y_test.txt", header= F)
names(y_test) <-"activity_label"
X_test<-read.csv("X_test.txt", header= F, sep = "")
features <-read.csv("features.txt", sep= "", header = F)
length(features$V2)
names(X_test)<-features$V2
dim(subject_test)
dim(X_test)
dim(y_test)
#Let's merge the three files using cbind and check if the dimensions add up
merged_test_files <- cbind(subject_test, y_test, X_test)
dim(merged_test_files)

# Now let's load the respective files from the "train" folder into R, assign names to them and check their dimensions
subject_train<-read.csv("subject_train.txt", header= F)
names(subject_train) <-"subject"
y_train<-read.csv("y_train.txt", header= F)
names(y_train) <-"activity_label"
X_train<-read.csv("X_train.txt", header= F, sep = "")
names(X_train)<-features$V2
dim(subject_train)
dim(X_train)
dim(y_train)
#We need tp merge the three files using cbind and check if the dimensions add up
merged_train_files <- cbind(subject_train, y_train, X_train)
dim(merged_train_files)

# The next step is to combine the two files we created from the "test" and "train" folders using rbind and generate unique column names
merged_both <- rbind(merged_test_files, merged_train_files)
dim(merged_both)
col_names <- make.names(names = names(merged_both), unique=TRUE, allow_ = TRUE)
names(merged_both) <- col_names
#Question 2: Extracting the measurements on the mean and standard deviation
# let's extract the columns that contain the mean and standard deviation of each measurement
extracted_mean_std <- select(merged_both, subject, activity_label, contains("mean") , contains("std"))
#Question 3: Using descriptive names to label activities
#We will replace the activity labels with descriptive names. First, look at the structure of the data to see if "activity_label" is coded as factor
str(merged_both)
# Since "activity_label" is not yet coded as factor, we need to change its type to "factor"
merged_both$activity_label<- as.factor(merged_both$activity_label)
merged_both$subject<- as.factor(merged_both$subject)
#let's check if the change to factor works and check the levels
str(merged_both$activity_label)
levels(merged_both$activity_label)
#Now, we assign the levels of "activity_label" to the descriptive names
levels(merged_both$activity_label) <-c("walking", "walking_ustairs", "walking_downstairs", "sitting", "standing", "laying")
#we can check if our adjustement worked by viewimg the levels of the "merged_both$activity_label" column
levels(merged_both$activity_label)
# we can do the same to change the label of the "extracted_mean_std" file
extracted_mean_std$activity_label<-as.factor(extracted_mean_std$activity_label)
str(extracted_mean_std$activity_label)
levels(extracted_mean_std$activity_label) <-c("walking", "walking_ustairs", "walking_downstairs", "sitting", "standing", "laying")
levels(extracted_mean_std$activity_label)
#Question 4: Using approprirate lables for the variable names
# The variable names I used are already descriptive. They are the exact names I found in the features file and I replaced the V1, V2, etc with these names.
head(names(merged_both), n=10)
#Question 5: Creating a second tidy data set with averages of each variable for each activity and subject
#let's arrange the merged_both file according to subjects
arranged<-arrange(merged_both, subject)
# Now, let calclulate the averages of each variable, each activity for each subject. We first need to group the data frame by subject and activity and summarize the grouped data using the mean function
averages<-merged_both %>% group_by(subject, activity_label) %>% summarize_each (funs(mean))
head(averages)
write.table(x = averages, file ="tidy_data.txt", sep = " ", row.names=FALSE)