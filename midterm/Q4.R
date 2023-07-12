# Course     : cs513-B
# First Name : Julia
# Last Name  : Nelson
# Id         : 10432982
# Description: midterm Q4 - "I pledge my honor that I have abided by the Stevens Honor System."



############################################################################################
##############################################
##############################################
#############                    #############
#############     QUESTION 4     #############
#############                    #############
##############################################
##############################################
##############################################


# Load the CANVAS fictional “COVID19_v2.CSV” dataset into R/Python. 
# Remove the missing values. 
# Use unweighted knn(k=5) to predict infection rate (infected) for a random sample (30%) of the data (test dataset).
# COVID19: Healthcare Workers data dictionary.
#     Age: Age of healthcare worker
#     Exposure: Level of exposure to COVID 19 patients
#     MaritalStatus: Marital Status
#     Cases: Number of the cases in the county
#     MonthAtHospital: # of months that the healthcare worker has been working at the current facility
#     Infected: Is healthcare worker infected by the COVID19 virus (yes or no?)
# 

# start by clearing lists to delete all the objects from your R- environment. 
rm(list=ls())

# Reload the file from canvas into R. for anyone
# file<-file.choose()
# data<- read.csv(file, na.strings = "?",, colClasses=c("Infected" = "factor" ))

# set working directory path for csv file 
setwd("/Users/Julia/Documents/cs513/midterm") 
# load the data -- changing ? to NA
data <- read.csv("COVID19_v2.csv",header=TRUE,na.strings = "?", colClasses=c("Infected" = "factor" )) 

# Checks if Infected is factor
is.factor(data$Infected)

#	Remove the rows with missing values
data_clean <- na.omit(data)      

# displays data with missing values deleted
#data_clean   

# create training and test data sets - 30%
index <- sort(sample(nrow( data_clean),round(.30*nrow(data_clean ))))
training <- data_clean[-index,]
test <- data_clean[index,]

#	Use knn with k=5 and classify the test dataset
library(kknn) 


predict_k5 <- kknn(formula= Infected~., training[,c(-1)] , test[,c(-1)], k=5,kernel ="rectangular"  )
# unweighted = rectangualar

# fitting to k=5
fit <- fitted(predict_k5)
table(test$Infected,fit)
# View(predict_k5)         # to view  predict_k5


# performance of knn
error <- ( test$Infected!=fit)
print(error)

rate <- sum(error)/length(error)
rate

##### print out resulting table and rate
print(table(test$Infected,fit) )
print(rate)


