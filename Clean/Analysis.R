#set working directory and get directory 
#setting my working directory, this is where the downloaded zip file exists  
  
  setwd("/Users/vikranthreddykasireddy/Desktop/Coursera/UCI_HAR_Dataset")

#creating dataframes for test dataset
  test_df_x<-read.table("/Users/vikranthreddykasireddy/Desktop/Coursera/UCI_HAR_Dataset/test/X_test.txt",sep = "")
  test_df_y<-read.table("/Users/vikranthreddykasireddy/Desktop/Coursera/UCI_HAR_Dataset/test/y_test.txt",sep = "")
  test_df_sub<-read.table("/Users/vikranthreddykasireddy/Desktop/Coursera/UCI_HAR_Dataset/test/subject_test.txt",sep = "")
  test_df<-cbind(test_df_sub,test_df_y,test_df_x)
  colnames(test_df)<-c("Subject","Activity",1:561)
  
  
#creating dataframes for training dataset
  train_df_x<-read.table("/Users/vikranthreddykasireddy/Desktop/Coursera/UCI_HAR_Dataset/train/X_train.txt",sep = "")
  train_df_y<-read.table("/Users/vikranthreddykasireddy/Desktop/Coursera/UCI_HAR_Dataset/train/y_train.txt",sep = "")
  train_df_sub<-read.table("/Users/vikranthreddykasireddy/Desktop/Coursera/UCI_HAR_Dataset/train/subject_train.txt",sep = "")
  train_df<-cbind(train_df_y,train_df_sub,train_df_x)
  colnames(train_df)<-c("Subject","Activity",1:561)
  
#combining dataframes to make a single dataframe
  df<-rbind(test_df,train_df)

#reading features dataframe to compare the combined data frame to activities
  
  features<-read.table("/Users/vikranthreddykasireddy/Desktop/Coursera/UCI_HAR_Dataset/features.txt",sep = "")

#grep to find for columns related to mean and standard deveiation
  
  features[,2]<-as.character(features[,2])
  features_ms<-grep(".*mean*.|.*std*.", features[,2])
  features_names<-features[features_ms,2]

#selecting only the columns related to mean and std in the combined dataframe
  
  features_tidy<-c("Activity","Subject")
  features_tidy<-append(features_tidy,features_ms,after=length(features_tidy))
  df_selected<-df[,features_tidy]
  
#tyding features names
  
  features_names<-gsub('[-()]', '',features_names)
  features_col<-c("Activity","Subject")
  features_col<-append(features_col,features_names,after =length(features_col)) 
  colnames(df_selected)<-features_col
  
#loading activity table
  
  activityLabels <- read.table("/Users/vikranthreddykasireddy/Desktop/Coursera/UCI_HAR_Dataset/activity_labels.txt")
  activityLabels[,2] <- as.character(activityLabels[,2])
  
#turning activities and subjects to factors
  
  df_selected$Activity<-factor(df_selected$Activity, levels = activityLabels[,1],labels = activityLabels[,2])
  df_selected$Subject<-as.factor(df_selected$Subject)
  
#Creating a second, independent tidy data set with the average of each variable for each activity and each subject:
  
  df_mean <- aggregate(. ~Subject + Activity, df_selected, mean)
  df_mean <- df_mean[order(df_mean$Subject, df_mean$Activity),]

#this tidy data might not look tidy in the txt format but when read to R should be tidy
  write.table(df_mean, "tidy.txt", row.names = FALSE, quote = FALSE)
  
  
  