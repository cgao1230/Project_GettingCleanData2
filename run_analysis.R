# This code is used to preprocess humam activity recognition data set.
# This code follows the steps in description in Peer assignment of Getting and 
# cleaning data.

# read training and testing data
path <- getwd()
# read feature names
features <- read.table(paste0(path,'/UCI HAR Dataset/features.txt'),colClasses='character')
# training
train_path <- paste0(path,'/UCI HAR Dataset/train')
training_data <- read.table(paste0(train_path,'/X_train.txt'))
training_label <- read.table(paste0(train_path,'/y_train.txt'),col.names = 'Label')
training_subject <- read.table(paste0(train_path,'/subject_train.txt'),col.names='Subject')
# test
test_path <- paste0(path,'/UCI HAR Dataset/test')
testing_data <- read.table(paste0(test_path,'/X_test.txt'))
testing_label <- read.table(paste0(test_path,'/y_test.txt'),col.names = 'Label')
testing_subject <- read.table(paste0(test_path,'/subject_test.txt'),col.names='Subject')

# Step1:merge training and testing data 
training_alldata <- cbind(training_label,training_subject,training_data)
testing_alldata <- cbind(testing_label,testing_subject,testing_data)
alldata <- rbind(training_alldata,testing_alldata)
# remove previous data set
rm(training_alldata,training_data,training_label,training_subject)
rm(testing_alldata,testing_data,testing_label,testing_subject)

# Step2: extract only measurements on the mean and standard deviation for each measurement
loc <- grep("mean|std",features[,2])
alldata <- alldata[,c(1,2,as.numeric(features[loc+2,1]))]
colnames(alldata)[3:ncol(alldata)]<-features[loc,2]

# Step3 & Step4: descriptive activity names
activity_names <- as.factor(alldata$Label)
levels(activity_names) <- c("walk","walk up","walk down","sit","stand","lay")
alldata[,1] <- activity_names

# Step5:Creates a second, independent tidy data set with the average
summary_data <- data.frame() # new data
unique_subject <- unique(alldata[,2])
for (i in 1:length(unique_subject)) {
        subject_index <- alldata$Subject == unique_subject[i]
        summary_data[(1+6*(i-1)):(i*6),1] <- levels(activity_names)
        summary_data[(1+6*(i-1)):(i*6),2] <- unique_subject[i]
        for (j in 1:length(loc)) {
                mean_data <- tapply(alldata[subject_index,j+2],alldata[subject_index,1],mean)
                summary_data[(1+6*(i-1)):(i*6),j+2] <- mean_data
        }
}
colnames(summary_data) <- colnames(alldata)

write.table(summary_data[,1:ncol(summary_data)],"summary_data.csv",sep=",",row.names=FALSE)
print("Data is successfully saved in summary_data.csv")










