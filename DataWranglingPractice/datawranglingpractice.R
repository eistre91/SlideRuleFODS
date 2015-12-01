setwd("C:/DataWranglingPractice")

#rm(list=ls()) to remove all variables from environment

library(dplyr)

subject_test <- read.table("test/subject_test.txt")
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/Y_test.txt")

temp = list.files(pattern="*.txt")
myfiles = lapply(temp, read.table)

temp = list.files(pattern="*txt")
for (i in 1:length(temp)) assign(temp[i], read.table(temp[i]))

head(body_acc_x_test.txt)
head(activity_labels.txt)

subject_train.txt <- rename(subject_train.txt, subject_id = V1)
subject_test.txt <- rename(subject_test.txt, subject_id = V1)

subject_combined <- bind_rows(subject_train.txt, subject_test.txt)

X_combined <- bind_rows(X_train.txt, X_test.txt)
Y_combined <- bind_rows(y_train.txt, y_test.txt)

body_acc_x_combined <- bind_rows(body_acc_x_train.txt, body_acc_x_test.txt)
body_acc_y_combined <- bind_rows(body_acc_y_train.txt, body_acc_y_test.txt)
body_acc_z_combined <- bind_rows(body_acc_z_train.txt, body_acc_z_test.txt)

body_gyro_x_combined <- bind_rows(body_gyro_x_train.txt, body_gyro_x_test.txt)
body_gyro_y_combined <- bind_rows(body_gyro_y_train.txt, body_gyro_y_test.txt)
body_gyro_z_combined <- bind_rows(body_gyro_z_train.txt, body_gyro_z_test.txt)

total_acc_x_combined <- bind_rows(total_acc_x_train.txt, total_acc_x_test.txt)
total_acc_y_combined <- bind_rows(total_acc_y_train.txt, total_acc_y_test.txt)
total_acc_z_combined <- bind_rows(total_acc_z_train.txt, total_acc_z_test.txt)

head(X_combined)

as.numeric(rownames(subject_combined))
subject_combined <- add_rownames(subject_combined, var = "rowname")

X_combined <- add_rownames(X_combined, var = "rowname")
Y_combined <- add_rownames(Y_combined, var = "rowname")

body_acc_x_combined <- add_rownames(body_acc_x_combined, var = "rowname")
body_acc_y_combined <- add_rownames(body_acc_y_combined, var = "rowname")
body_acc_z_combined <- add_rownames(body_acc_z_combined, var = "rowname")

body_gyro_x_combined <- add_rownames(body_gyro_x_combined, var = "rowname")
body_gyro_y_combined <- add_rownames(body_gyro_y_combined, var = "rowname")
body_gyro_z_combined <- add_rownames(body_gyro_z_combined, var = "rowname")

total_acc_x_combined <- add_rownames(total_acc_x_combined, var = "rowname")
total_acc_y_combined <- add_rownames(total_acc_y_combined, var = "rowname")
total_acc_z_combined <- add_rownames(total_acc_z_combined, var = "rowname")

inner_join(Y_combined, activity_labels.txt, by = "V1")
Y_combined <- rename(Y_combined, activitylabel = V1)
Y_combined <- rename(Y_combined, activityname = V2)

names(X_combined)[2:562] <- as.character(features.txt[[2]])

full_table <- subject_combined
full_table <- inner_join(full_table, Y_combined, by = "rowname")
full_table <- inner_join(full_table, X_combined, by = "rowname")

names(full_table) <- make.names(names(full_table), unique=TRUE)
mean_std_table <- select(full_table, contains("mean"), contains("std"))

#Merges the training and the test sets to create one data set. --- check

#Extracts columns containing mean and standard deviation for each measurement ---check (Hint: Since some feature/column names are repeated, you may need to use the 'make.names()' function in R)

#Creates variables called 'ActivityLabel' and 'ActivityName' 
#that label all observations with the corresponding activity labels and 
#names respectively --- check

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
full_table %>%
  group_by(subject_id, activitylabel) %>%
  summarise_each(funs(mean), vars = tBodyAcc.mean...X:angle.Z.gravityMean.)

# full_table %>%
#   group_by(activitylabel) %>%
#   group_by(subject_id) %>%
#   select(tBodyAcc.mean...X:angle.Z.gravityMean.)
