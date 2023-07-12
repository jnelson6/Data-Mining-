# Course     : cs513-B
# First Name : Julia
# Last Name  : Nelson
# Id         : 10432982
# Description: midterm Q5 - "I pledge my honor that I have abided by the Stevens Honor System."




############################################################################################
##############################################
##############################################
#############                    #############
#############     QUESTION 5     #############
#############                    #############
##############################################
##############################################
##############################################
#
# Load the CANVAS “COVID19_v2.CSV” dataset into R/Python. 
# Remove the missing values. 
# Discretize the “MonthAtHospital” into “less than 6 months” and “6 or more months”. 
# Also discretize the age into “less than 35”, “35 to 50” and “51 or over”. 
# Construct a Naïve Bayes model to classify infection (“infected’) based on the other variables. 
# Measure the accuracy of the model.
# Do not use the original MonthAtHospital and age variables as predictors
#
#
# start by clearing lists to delete all the objects from your R- environment. 
rm(list=ls())

# Reload the file from canvas into R. for anyone
# file<-file.choose()
# data<- read.csv(file, na.strings = "?")

# set working directory path for csv file 
setwd("/Users/Julia/Documents/cs513/midterm") 
# load the data -- changing ? to NA
data <- read.csv("COVID19_v2.csv",header=TRUE,na.strings = "?", colClasses=c("Infected" = "factor" )) 
View(data)
# Important: make sure your categories are represented by the “factor” data type in R 
# Checks if Infected is factor
is.factor(data$Infected)

# delete the rows with missing values. 
#	Remove the rows with missing values
data_clean <- na.omit(data)      
View(data_clean)










# Discretize the “MonthAtHospital” into “less than 6 months” and “6 or more months”.
table(discretize(data_clean$MonthAtHospital, method = "fixed", breaks = c(-Inf, 6, Inf), 
                 labels = c("less than 6 months", "6 or more months")))

### UNFININSHED ##

# Also discretize the age into “less than 35”, “35 to 50” and “51 or over”. 

# Construct a Naïve Bayes model to classify infection (“infected’) based on the other variables. 

# Construct a Naïve Bayes model to classify infection (“infected’) based on the other variables. 