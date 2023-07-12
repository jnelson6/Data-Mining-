# Course     : cs513-B
# First Name : Julia
# Last Name  : Nelson
# Id         : 10432982
# Description: Homework 3 K-Nearest-Neighbor  - "I pledge my honor that I have abided by the Stevens Honor System."

# The “breast cancer dataset” in CANVAS was obtained from the University of Wisconsin Hospitals, 
# Madison from Dr. William H. Wolberg. The features in the dataset, described below, have been 
# categorized from 1 to 10.
#
# Use the knn methodology (k=3,5 and 10) to develop a classification models for the Diagnosis. 
# Important: make sure your categories are represented by the “factor” data type in R 
# and delete the rows with missing value. Use 30% test 70% training data. 
#    Features                      Domain  
# -- -----------------------------------------  
# Sample code number               id number   
# F1. Clump Thickness               1 - 10   
# F2. Uniformity of Cell Size       1 - 10   
# F3. Uniformity of Cell Shape      1 - 10   
# F4. Marginal Adhesion             1 - 10   
# F5. Single Epithelial Cell Size   1 - 10  
# F6. Bare Nuclei                   1 - 10   
# F7. Bland Chromatin               1 - 10   
# F8. Normal Nucleoli               1 - 10   
# F9. Mitoses                       1 - 10   
# Diagnosis Class:                 (2 for benign, 4 for malignant)




# start by clearing lists 
rm(list=ls())


#?install.packages
# check to see if already have kknn package
#installed.packages()
#install.packages("kknn")
#Use the R library("kknn") 

library(kknn)


# set working directory path for csv file 
setwd("/Users/Julia/Documents/cs513/hw2") 

# read csv file
bcData <- read.csv("breast-cancer-wisconsin.data.csv",header = TRUE,na.strings = "?") 
bcData <- na.omit(bcData)

#factor class
bcData$Class <- factor(bcData$Class, levels = c(2, 4), labels = c("Benign", "Malignant"))
idx <- sort(sample(nrow(bcData),as.integer(.70*nrow(bcData))))


training <- bcData[idx,]
test <- bcData[-idx,]


# k=3 test
#print('k=3')
predict_k3 <- kknn(formula=Class~., training[,-1], test, k=3,kernel ="rectangular")
fit <- fitted(predict_k3)
k3 <- table(Actual=test$Class,kknn=fit) #or Fitted=fit
### Help for accuracy function found at https://datascienceplus.com/k-nearest-neighbor-for-prediction-of-diabetes-in-nhanes/
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(k3)

# k=5 test
#print('k=5')
predict_k5 <- kknn(formula=Class~., training[,-1], test, k=5,kernel ="rectangular")
fit <- fitted(predict_k5)
k5 <- table(Actual=test$Class,kknn=fit) # or Fitted=fit
### Help for accuracy function found at https://datascienceplus.com/k-nearest-neighbor-for-prediction-of-diabetes-in-nhanes/
#accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(k5)


# k=10 test
#print('k=10')
predict_k10 <- kknn(formula=Class~., training[,-1], test, k=10,kernel ="rectangular")
fit <- fitted(predict_k10)
k10 <- table(Actual=test$Class,kknn=fit) # or Fitted=fit
### Help for accuracy function found at https://datascienceplus.com/k-nearest-neighbor-for-prediction-of-diabetes-in-nhanes/
#accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(k10)

