# 1. Merges the training and the test sets to create one data set.

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
x_combi <- rbind(x_train, x_test)
y_combi <- rbind(y_train, y_test)
subject_combi <- rbind(subject_train, subject_test)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

x_names <- read.table("UCI HAR Dataset/features.txt")
req_columns <- grep("-mean[(][)]|-std[(][)]", x_names[, 2])
x_combi <- x_combi[, req_columns]
names(x_combi) <- x_names[req_columns, 2]
names(x_combi) <- tolower(names(x_combi))
names(x_combi) <- gsub("[(]|[)]", "", names(x_combi))


# 3. Uses descriptive activity names to name the activities in the data set.

activity <- read.table("UCI HAR Dataset/activity_labels.txt")
activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
y_combi[,1] = activity[y_combi[,1], 2]
names(y_combi) <- "activity"


# 4. Appropriately labels the data set with descriptive variable names.

names(subject_combi) <- "subject"
combined_data <- cbind(subject_combi, y_combi, x_combi)


# 5. From the data set in Step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

unique_subject = unique(subject_combi)[,1]
subject_count = length(unique_subject)
activity_count = length(activity[,1])
numCols = dim(combined_data)[2]
data2 = combined_data[1:(subject_count*activity_count), ]

row = 1
for (s in 1:subject_count) {
        for (a in 1:activity_count) {
                data2[row, 1] = unique_subject[s]
                data2[row, 2] = activity[a, 2]
                tmp <- combined_data[combined_data$subject==s & combined_data$activity==activity[a, 2], ]
                data2[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}

write.table(data2, "data_with_average.txt", row.names = FALSE)