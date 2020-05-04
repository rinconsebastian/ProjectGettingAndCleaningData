#AUTOR: Félix Sebatián Rincón TObo
#CONTENT: FInal project for the Getting and Cleaning Data Course in coursera
#DATE:  2020-05-03
#vERSION: 1.0


# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#LIBRARIES
library(dplyr) 
library(reshape2)

#OBTAINING FEATURES NAMES
features <- read.table("data/features.txt")     #read features.txt file, this contain the names of columns of x_test.txt and x_train.txt files

features <- features%>%mutate(V2 = gsub("-",".",V2)) %>% #replace - for . in col V2 containing the col names
            mutate(V2 = gsub("\\(\\)","",V2)) #remove () for in col V2 containing the col names
           
col_names <- as.array(features$V2)  # take te second column containing the names of the columns and store this names in a character array




#OBTAINING TEST DATA
x_test <- read.table("data/test/X_test.txt",col.names = col_names)    #read values from x_test file and set the names of the colNames array
y_test <- read.table("data/test/y_test.txt", col.names = c("activity"))    #read activities from y_test file and set colname
subject_test <- read.table("data/test/subject_test.txt", col.names = c("subject"))    #read subjects from y_test file and set colname

test <- cbind(subject_test, y_test, x_test)   #combine the columns of the three previous set in one set called test


#OBTAINING TRAINING DATA
x_train <- read.table("data/train/X_train.txt",col.names = col_names)    #read values from x_train file and set the names of the colNames array
y_train <- read.table("data/train/y_train.txt", col.names = c("activity"))    #read activities from y_train file  and set colname
subject_train <- read.table("data/train/subject_train.txt", col.names = c("subject"))    #read subjects from y_train file and set colname

train <- cbind(subject_train, y_train, x_train)   #combine the columns of the three previous set in one set called train


#1)  MERGING TEST AND TRAINS IN ONE DATASET
records <- rbind(test,train)  #merge test dataframe and train dataframe in one dataframe called records

#2) SELECTING ONLY THE MEAN AND STANDARD DEVIATION COLUMNS
col_names_mean_std <- grep("(subject|activity|mean\\.|std\\.)", c("subject","activity",col_names)) # identify te colum number to keep  
records <-records %>% select(all_of(col_names_mean_std))   #keep the columns identified in col_names_mean_std


#4) TIDYING COLUMNS
#the names of the columns contain variables signals, axis and variables(mean std)

records <- melt(records,id=c("subject","activity"), value.name = "value")  #reorder te dataset to put variables in rows

# the new colum "variable " contain three values for each row -> signal, axis and variable, the next step is split this column

records <- records %>% mutate(axis = tolower(gsub(".+?\\.","",variable))) %>% #create a new column axis with the last part of the string in the variable col and
                             mutate(signal = gsub("\\..*$","",variable)) %>% #create a new column axis with the first part of the string in the variable col
                             mutate(variable = sub("^[a-zA-Z]*\\.","",variable)) %>%  mutate(variable = sub("\\..*$","",variable)) #remove all except the middle value in the variable columm


records <- records %>% select("subject","activity","signal","axis","variable","value")  #reorder columns


#3) USING DESCRIPTIVE ACTIVITY NAMES AND  SETTING ACTIVITY SIGNAL AXIS AND VARIABLE COLUMS AS FACTORS
activity_labels <-read.table("data/activity_labels.txt")  # read the file with the activity labels
activity_numbers <- as.integer(activity_labels$V2)        # create an array with the activity numbers
activity_labels <-  tolower(as.array(activity_labels$V2))  # create an array with the activity names

records  <- records %>% mutate(activity= factor(activity, activity_numbers, activity_labels)) %>%  # setting activity as factor column
                        mutate(signal = factor(signal)) %>%   # setting signal as factor column
                        mutate(axis = factor(axis)) %>%       # setting axis as factor column
                        mutate(variable = factor(variable))   # setting variable as factor column


#5) CREATING SUMMARY FOR STEP 5

records_summarise <- records %>%    
                  group_by(signal,variable, activity, subject) %>%  #Group the dataset  by signal,variable, activity, subject
                  summarise(average = mean(value))                  #summarise calculating the mean of the value colum for each signal,variable, activity, subject


write.table(records_summarise,file = "recordsStep5.txt", row.names = FALSE)     # save the sumamrise in a txt files

