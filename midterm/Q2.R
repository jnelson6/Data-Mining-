# Course     : cs513-B
# First Name : Julia
# Last Name  : Nelson
# Id         : 10432982
# Description: HW_Midterm Q2 - "I pledge my honor that I have abided by the Stevens Honor System."



############################################################################################
##############################################
##############################################
#############                    #############
#############     QUESTION 2     #############
#############                    #############
##############################################
##############################################
##############################################
# Load the “COVID19_v2.CSV” dataset, from the raw_data module in CANVAS, into R. 
# This is a fictional COVID19 healthcare workers data set. Perform the EDA analysis by:
# 
# STEPS:
#     Summarizing each column (e.g. min, max, mean )
#     Identifying missing values
#     Displaying the frequency table of “Infected” vs. “MaritalStatus” 
#     Displaying the scatter plot of “Age”, “MaritalStatus” and “MonthAtHospital”, one pair at a time
#     Show box plots for columns:  “Age”, and “MonthAtHospital”
#     Replacing the missing values of “Cases” with the “mean” of “Cases”.
#
# COVID19: Healthcare Workers data dictionary.
#     Age: Age of healthcare worker
#     Exposure: Level of exposure to COVID 19 patients
#     MaritalStatus: Marital Status
#     Cases: Number of the cases in the county
#     MonthAtHospital: # of months that the healthcare worker has been working at the current facility
#     Infected: Is healthcare worker infected by the COVID19 virus (yes or no?)
# 
####################################################################################


# start by clearing lists 
rm(list=ls())

# Reload the file from canvas into R. for anyone
# file<-file.choose()
# data<- read.csv(file, na.strings = "?",, colClasses=c("Infected" = "factor" ))

# set working directory path for csv file 
setwd("/Users/Julia/Documents/cs513/midterm") 
# Load the “COVID19_v2.CSV” dataset from canvas into R and perform the EDA analysis
# reads csv file ... ( data contains headers so true, and na.strings =c("?") changes ? to NA's )
covidData <- read.csv("COVID19_v2.csv",header = TRUE,na.strings = "?") 



###### I.Summarizing each column (e.g. min, max, mean )  
summary(covidData)     # summary() includes mean, median, min, max, 1st & 3rd Quartiles


###### II.Identifying missing values
is.na(covidData)    # checks if data is NA (t=NA/f=not NA)


###### III. Displaying the frequency table of “Infected” vs. "MaritalStatus"
ftable(Infected=covidData$Infected,MaritalStatus=covidData$MaritalStatus)      
# adding "Infected =" and "MaritalStatus=" adds header names to the frequency table


###### IV.Displaying the scatter plot of “Age”, “MaritalStatus” and “MonthAtHospital”, one pair at a time
pairs(~Age+MaritalStatus+MonthAtHospital,data=covidData,label.pos=0.5,font.labels=1,row1attop=TRUE,lower.panel=NULL,gap=0.5,col="purple")  
# formula method with correct vars, label.pos/font.labels adjust text, NULL lower.panel removes swapped axis plots

###### V.Show Box plots of "Age", and "MonthAtHospital" 
# Box plots of MonthAtHospital 
boxplot(covidData[2], main = "Box Plot - Age")
# Box plot of MonthAtHospital 
boxplot(covidData[6], main = "Box Plot - MonthAtHospital")
          # Combined box plot?? => boxplot(covidData$MonthAtHospital ~ covidData$Age, main = "BoxPlot Covid19 Worker Age and MonthAtHospital")

###### III.Replacing the missing values of "Cases" with the “mean” of the Cases
# print Cases column to see with NA's
covidData$Cases
# check if Cases val is NA -> if so remove and replace with the mean of cases
covidData$Cases[is.na(covidData$Cases)] <- mean(covidData$Cases, na.rm = TRUE) # any NA in column Cases replaced by mean of column
# reprint column to see replaced missing values 
print(covidData$Cases)









