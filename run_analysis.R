#list of files in the folder
file_list <- list.files(pattern="*.txt")
file_list<-grep("[read]",file_list,value = TRUE)
#read features file
features<-read.table("features.txt",header = FALSE)
#read activities names file
activitys_names<-read.table("activiy_labels.txt", header = FALSE)
features_train<-read.table("./train/X_train.txt",header = FALSE)
activitys_train<-read.table("./train/Y_train.txt",header=FALSE)
subjects_train<-read.table("./train/subject_train.txt",header=FALSE)
bodyAccX_train<-read.table("./train/Inertial Signals/body_acc_x_train.txt",header=FALSE)
bodyAccY_train<-read.table("./train/Inertial Signals/body_acc_y_train.txt",header=FALSE)
bodyAccZ_train<-read.table("./train/Inertial Signals/body_acc_z_train.txt",header=FALSE)
bodyGyroX_train<-read.table("./train/Inertial Signals/body_gyro_x_train.txt",header=FALSE)
bodyGyroY_train<-read.table("./train/Inertial Signals/body_gyro_y_train.txt",header=FALSE)
bodyGyroZ_train<-read.table("./train/Inertial Signals/body_gyro_z_train.txt",header=FALSE)
totalAccX_train<-read.table("./train/Inertial Signals/total_acc_x_train.txt",header=FALSE)
totalAccY_train<-read.table("./train/Inertial Signals/total_acc_y_train.txt",header=FALSE)
totalAccZ_train<-read.table("./train/Inertial Signals/total_acc_z_train.txt",header=FALSE)
features_test<-read.table("./test/X_test.txt",header = FALSE)
activitys_test<-read.table("./test/Y_test.txt",header=FALSE)
subjects_test<-read.table("./test/subject_test.txt",header=FALSE)
bodyAccX_test<-read.table("./test/Inertial Signals/body_acc_x_test.txt",header=FALSE)
bodyAccY_test<-read.table("./test/Inertial Signals/body_acc_y_test.txt",header=FALSE)
bodyAccZ_test<-read.table("./test/Inertial Signals/body_acc_z_test.txt",header=FALSE)
bodyGyroX_test<-read.table("./test/Inertial Signals/body_gyro_x_test.txt",header=FALSE)
bodyGyroY_test<-read.table("./test/Inertial Signals/body_gyro_y_test.txt",header=FALSE)
bodyGyroZ_test<-read.table("./test/Inertial Signals/body_gyro_z_test.txt",header=FALSE)
totalAccX_test<-read.table("./test/Inertial Signals/total_acc_x_test.txt",header=FALSE)
totalAccY_test<-read.table("./test/Inertial Signals/total_acc_y_test.txt",header=FALSE)
totalAccZ_test<-read.table("./test/Inertial Signals/total_acc_z_test.txt",header=FALSE)
names(features_train)<-features$V2
names(features_test)<-features$V2
names(activitys_train)<-"activitys"
names(activitys_test)<-"activitys"
names(subjects_train)<-"subject"
names(subject_test)<-"subject"
sub_act_feat_train=cbind(subjects_train,activitys_train,features_train)
sub_act_feat_test=cbind(subjects_test,activitys_test,features_test)
#question 1
subActFeatures_both<-rbind(sub_act_feat_train,sub_act_feat_test)
#question 2
subActMeanStd_both<-subActFeatures_both%>%select(matches('mean|std'))
#question 3
subActFeatures_both_descAct<-subActFeatures_both%>% 
  arrange(activitys) %>% 
  mutate(activitys = as.character(factor(activitys, levels=1:6, 
                                  labels= activitys_names$V2)))
#question 4
names(subActFeatures_both)<-gsub("tBodyAcc-","Body acceleration signal in time domain (from the accelerometer)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("tBodyAccMag-","Body acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("tBodyAccJerk-","Body acceleration jerk signal in time domain (from the accelerometer)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("tBodyAccJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform (from the accelerometer)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("tGravityAcc-","Gravity acceleration signal in time domain (from the accelerometer)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("tGravityAccMag-","Gravity acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("tBodyGyro-","Body acceleration signal in time domain (from the gyroscope)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("tBodyGyroMag-","Body acceleration signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("tBodyGyroJerk-","Body acceleration jerk signal in time domain (from the gyroscope)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("tBodyGyroJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("fBodyAcc-","Body acceleration signal in frequence domain (from the accelerometer)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("fBodyAccMag-","Body acceleration signal in frequence domain applied to Fast Fourier Transform(from the accelerometer)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("fBodyAccJerk-","Body acceleration jerk signal in frequence domain (from the accelerometer)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("fBodyGyro-","Body acceleration signal in frequence domain (from the gyroscope)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("fBodyAccJerkMag-","Body acceleration jerk signal in frequence domain applied to Fast Fourrier Transform (from the accelerometer)",names(subActFeatures_both))
names(subActFeatures_both)<-gsub("fBodyGyroMag-","Body acceleration signal in frequence domain applied to Fast Fourier Transform (from the gyroscope)",names(subActFeatures_both))

#question 5
new_tidydata<-whole_both_descAct%>%group_by(activitys,subject)%>%summarise_all(mean)