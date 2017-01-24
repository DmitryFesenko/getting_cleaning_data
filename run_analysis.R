run_analysis <- function() {
        library(reshape2)
        #load train data
        subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
        activity_train <- read.table("UCI HAR Dataset/train/y_train.txt")
        measurements_train <- read.table("UCI HAR Dataset/train/X_train.txt")
        train_set <- cbind(subject_train, activity_train, measurements_train)
        #load test data
        subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
        activity_test <- read.table("UCI HAR Dataset/test/y_test.txt")
        measurements_test <- read.table("UCI HAR Dataset/test/X_test.txt")
        test_set <- cbind(subject_test, activity_test, measurements_test)
        #merge two data sets into one data frame
        all_data <- rbind(train_set, test_set)
        #load features data 
        features <- read.table("UCI HAR Dataset/features.txt")
        #extract names from data frame as vector
        features_name <- as.vector(features[,2])
        #create header for all_data 
        names(all_data) <- c("subject", "activity", features_name)
        #extract measurements only on the mean and standart deviation
        main_data <- all_data[, grep(".*subject.*|.*activity.*|.*mean.*|.*std.*", names(all_data))]
        #load activity_labels
        activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
        #replace activity numbers with discriptive names using them as factors
        main_data$activity <- factor(main_data$activity, levels = activity_labels[,1], labels = activity_labels[,2])
        #convert subjects to factors
        main_data$subject <- as.factor(main_data$subject)
        #change variables names to more descriptive and tidy
        names(main_data) <- gsub("mean", "Mean", names(main_data))
        names(main_data) <- gsub("std", "Std", names(main_data))
        names(main_data) <- gsub("[-()]", "", names(main_data))
        #create independent tidy data set with the average of each variable for each activity and each subject
        main_data_melted <- melt(main_data, id = c("subject", "activity"))
        main_data_mean <- dcast(main_data_melted, subject + activity ~ variable, mean)
        #save tidy data 
        write.table(main_data_mean, "tidy.txt", row.names = FALSE, quote = FALSE)
        
}