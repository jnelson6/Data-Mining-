# Course     : cs513-B
# First Name : Julia
# Last Name  : Nelson
# Id         : 10432982
# Description: Homework 2 EDA


# start by clearing lists 
rm(list=ls())


# 1- Load the “breast-cancer-wisconsin.data.csv” from canvas into R 
#     and perform the EDA analysis by:

# set working directory path for csv file 
setwd("/Users/Julia/Documents/cs513/hw2") 

# read csv file, data contains headers so true, 
# na.strings =c("?") changes ? to NA 
bcwData <- read.csv("breast-cancer-wisconsin.data.csv",header = TRUE,na.strings = "?") 

###### I.Summarizing each column (e.g. min, max, mean )  
summary(bcwData)     # summary() includes mean, median, min, max, 1st & 3rd Quartiles
  
###### II.Identifying missing values
is.na(bcwData)    # checks if data is NA (t=NA/f=not NA)

###### III.Replacing the missing values with the “mean” of the column.
# help found at https://stackoverflow.com/questions/25835643/replace-missing-values-with-column-mean
# For-loop replacing NA with mean of column
for(i in 1:ncol(bcwData)){                   
  # replace = get variable to replace
  replace <- is.na(bcwData[ ,i])
  # replace NA with rounded mean of column i
  bcwData[replace, i] <- round(mean(bcwData[ ,i], na.rm = TRUE),4) #can round by adding round() around mean
}
bcwData   #prints data so you can see change of NA to mean

    # 1st attempt at replacing NA using individual column names 
      #data$F1[is.na(data$F1)] <- mean(data$F1, na.rm = TRUE) # any NA in column F1 replaced by mean of column
      #data$F2[is.na(data$F2)] <- mean(data$F2, na.rm = TRUE) # any NA in column F2 replaced by mean of column
      #data$F3[is.na(data$F3)] <- mean(data$F3, na.rm = TRUE) # any NA in column F3 replaced by mean of column
      #data$F4[is.na(data$F4)] <- mean(data$F4, na.rm = TRUE) # any NA in column F4 replaced by mean of column
      #data$F5[is.na(data$F5)] <- mean(data$F5, na.rm = TRUE) # any NA in column F5 replaced by mean of column
      #data$F6[is.na(data$F6)] <- mean(data$F6, na.rm = TRUE) # any NA in column F6 replaced by mean of column
      #data$F7[is.na(data$F7)] <- mean(data$F7, na.rm = TRUE) # any NA in column F7 replaced by mean of column
      #data$F8[is.na(data$F8)] <- mean(data$F8, na.rm = TRUE) # any NA in column F8 replaced by mean of column
      #data$F9[is.na(data$F9)] <- mean(data$F9, na.rm = TRUE) # any NA in column F9 replaced by mean of column


###### IV.Displaying the frequency table of “Class” vs. F6
ftable(Class=bcwData$Class,F6=bcwData$F6)       # adding Class= and F6= adds header names to the table

###### V.Displaying the scatter plot of F1 to F6, one pair at a time
# pairs(formula, data)
pairs(bcwData[2:7], main = "Wisconsin Breast Cancer Scatter Plot", col="purple" )
    # Attempt One at a time individually
    # plot(bcwData$F1,bcwData$F2)
    # plot(bcwData$F1,bcwData$F3)
    # plot(bcwData$F1,bcwData$F4)
    # plot(bcwData$F2,bcwData$F3)
    # plot(bcwData$F2,bcwData$F4)
    # plot(bcwData$F3,bcwData$F4)
    
###### VI.Show histogram box plot for columns F7 to F9
boxplot(bcwData[8:10], main = "Box Plot Histogram (F7 to F9)")

###### 2- Delete all the objects from your R- environment. 
rm(list=ls())
# Reload the “breast-cancer-wisconsin.data.csv” 
#    from canvas into R. Remove any row with a missing value in any of the columns.

setwd("/Users/Julia/Documents/cs513/hw2") 
data <- read.csv("breast-cancer-wisconsin.data.csv",header = TRUE,na.strings = "?") 
data <- na.omit(data)
data
