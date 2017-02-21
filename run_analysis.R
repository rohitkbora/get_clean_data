#iterating over the training and test data and merging them
##Root directory where all the data is stored
root_dir<-"UCI HAR Dataset"
##Picking the all directories inside the train directory
comb_dirs<-list.dirs(paste(root_dir,'train',sep="\\"),recursive=T)
##iterating over the directories
for(comb_dir in comb_dirs)
{
##Transforming the path as per required for Windows System 
comb_dir<-sub("/","\\\\",comb_dir) 
##replacing the "train" in path with "comb" to store the merged data
comb_dir<-sub("train","comb",comb_dir) 
#print(paste("  ???   comb_dir ?????   ",comb_dir))
##creating the path for the "comb" dir hierarchy for storing the merged data
if(!file.exists(comb_dir))
{
dir.create(file.path(comb_dir),recursive=T)
}
}
train_dir<-list.files(paste(root_dir,'train',sep="\\"),recursive=T)
#print(paste('>>>>>>>>>  train_dir >>>>>>>>  ',train_dir,sep=" "))

##Iterating over the filres in "train" directory
for (file in train_dir)
{
#print(paste('>>>>>>>>>  1 >>>>>>>>  ',file,sep=" "))
##Transforming the path as per required for Windows System 
file<-sub("/","\\\\",file)
testfile<-sub("train.txt","test.txt",file)
#print(paste('>>>>>>>>>  2 >>>>>>>>  ',paste(root_dir,'train',file,sep="\\"),sep=" "))

##Reading the data form the "train" directory
 train_df<-read.table(paste(root_dir,'train',file,sep="\\"))
#print(paste('>>>>>>>>>  3 >>>>>>>>  ',paste(root_dir,'test',testfile,sep="\\"),sep=" "))

##Reading the data form the "test" directory 
test_df<-read.table(paste(root_dir,'test',testfile,sep="\\"))

##Merging the data read from files in "train" and "test" directories
merge_df<-rbind(train_df,test_df,na.rm=T)
combfile<-sub("test.txt","comb.txt",testfile)
combfile_name<-paste(root_dir,"comb",combfile,sep="\\")

##Creating the files in the "comb" directory
if(!file.exists(combfile_name))
file.create(combfile_name)

##Write the merged data into the files in "comb" directory
write.table(merge_df,file=combfile_name)
}

############################################################################

# To extract the mean and sd for each measurement for every measurement
root_dir<-"UCI HAR Dataset\\comb\\Inertial Signals"
extract_dir<-"UCI HAR Dataset\\extract\\Inertial Signals"

##Creating the "extract" dir to store the results of the MEAN and SD
if(!file.exists(extract_dir))
{
dir.create(extract_dir,recursive=T)
}

##Iterating over the measurement files 
m_files<-list.files(root_dir)
for(m_f in m_files)
{
#print(m_f)
df<-read.table(paste(root_dir,m_f,sep="\\"))
#print(2)

##Extracting th mean over rows
df.mean<-apply(df,1,mean)
#print(df.mean)

##Extracting the SD over rows

df.sd<-apply(df,1,sd)
#print(df.sd)
df_measurement_files<-paste(extract_dir,m_f,sep="\\")

##combining the columns for MEAN and SD
df_measurement<-cbind(mean=df.mean,sd=df.sd)

##Writing the data into the file
write.table(df_measurement,df_measurement_files)
}

################################################################################

#Function to give the name of activity against the code
func_desc_name<-function(x){
 ifelse(x==1,'WALKING',ifelse(x==2,'WALKING_UPSTAIRS',ifelse(x==3,'WALKING_DOWNSTAIRS',
ifelse(x==4,'SITTING',ifelse(x==5,'STANDING','LAYING')))))
}

##############  placing the name of activity inside the dataset ##########

comb_act<-read.table("UCI HAR Dataset\\comb\\Y_comb.txt")
data_df<-read.table("UCI HAR Dataset\\comb\\X_comb.txt")
#print(head(comb_act))
comb_act$Activity<-func_desc_name(comb_act$V1)
data_df=cbind(Activity=comb_act$Activity,data_df)
#colnames(comb_act)<-c('Activity','Activity description')
#print(head(data_df)[,(1:3)])
write.table(data_df,"UCI HAR Dataset\\comb\\X_With_Act_Name.txt")



####################  Labeling the dataset with Variable name ###########
var_lab<-read.table("UCI HAR Dataset\\features.txt")
#class(var_lab)
#var_lab<-rbind(as.dataframe("Activity"),var_lab)
var_lab<-append(var_lab,"Activity",after=0)
#//data_df<-rbind(paste("Activity",var_lab,sep=","),data_df)
#colnames(data_df[,V1:])<-var_lab
colnames(data_df)<-var_lab

write.table(data_df,"UCI HAR Dataset\\comb\\X_With_Act_feature_Name.txt")
#print(head(data_df[,1:5]))
subject<-read.table("UCI HAR Dataset\\comb\\subject_comb.txt")
subject<-subject$V1
data_df<-cbind(subject,data_df)
#print(tail(data_df[,1:5]))
write.table(data_df,"UCI HAR Dataset\\comb\\X_With_Act_feature_Name.txt")


########################## applying mean over variable for for each subject and Activity ###########

data_df1<-aggregate(.~subject+Activity,data_df,FUN=mean)
#////print(head(data_df1[,1:5]))
write.table(data_df1,"UCI HAR Dataset\\comb\\X_With_Act_feature_Name_apply_mean.txt",row.name=FALSE)


