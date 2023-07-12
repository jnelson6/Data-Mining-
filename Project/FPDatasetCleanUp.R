# CS 513 
# Final Project 
# Dataset Clean-Up
# UNOFFICIAL #################################



# Julia Nelson, Taylor Niedzielski, Sonia Patel, and Noah Suttora

# Clear Environment
rm(list=ls())

# Import Dataset
RawData <- read.csv("/Users/Julia/Desktop/CS 513/Project/CAX_Startup_Data.csv", na.string = c("?"))
library(e1071)
library(caret)
View(RawData)

# Remove Columns of DisInterest
Data <- RawData[-c(1,5,6,7,9,10,11,13,14,15,16,17,23,24,
                   25,26,28,34,43,46,47,48,49,50,51,
                   52,53,54,56,57,59,61,62,63,64,65,
                   66,67,71,73,74,75,76,79,80,82,83,
                   84,85,86,87,89,90,91,92,93,
                   98,99,100,101,102,103,104,105,
                   106,107,108,109,110,111,112,
                   113,114,115,116)]

# Rename Columns of Interest
colnames(Data) <- c("Status","FoundingYear","Age","FocusFunctions",
                    "EmployeeCount","TeamSizeGrowth","NumSeedInvestors",
                    "NumAngelorVCInvestors","NumCofounders","NumAdvisors",
                    "SeniorLeadershipTeamSize","EmployeeTeamSize",
                    "TopCompanyExp","StartupExp",
                    "SucessfulStartupExp","Big5Partner","ConsultingExp",
                    "ProductorService","DataFocus","ConsumerDataFocus",
                    "DataStructureFocus","SubscriptionBased", 
                    "CloudPlatformBased","LocalGlobal","BusinessModel",
                    "CapitalIntensive","CrowdsourcingBased","CrowdfundingBased",
                    "B2CorB2B","GlobalExporsure","HighestEducation",
                    "Fortune100Exp","Fortune500Exp","Fortune1000Exp",
                    "NumFounderRecognition","PricingStrategy",
                    "HyperLocalisation","LongtermFounderRelationship",
                    "GooglePageRank","NumDirectCompetitors","EmployeesPerYear",
                    "LastFundingRoundAmount","RecessionSurvival")
View(Data)


## for blank cells Data[Data == ''] <- NA
Data[Data == ''] <- NA

##make character type into integer type
Data$NumAngelorVCInvestors <- as.integer(Data$NumAngelorVCInvestors, na.rm = TRUE)
Data$NumSeedInvestors <- as.integer(Data$NumSeedInvestors, na.rm = TRUE)
Data$NumFounderRecognition<-as.integer(Data$NumFounderRecognition, na.rm = TRUE)
Data$GooglePageRank <- as.integer(Data$GooglePageRank, na.rm = TRUE)
Data$NumDirectCompetitors <- as.integer(Data$NumDirectCompetitors, na.rm = TRUE)
Data$EmployeesPerYear <- as.integer(Data$EmployeesPerYear, na.rm = TRUE)
Data$LastFundingRoundAmount <- as.integer(Data$LastFundingRoundAmount, na.rm = TRUE)


##populate NA with mean of column
Data[is.na(Data$NumAngelorVCInvestors),"NumAngelorVCInvestors"]<-mean(Data$NumAngelorVCInvestors,na.rm=TRUE) 
Data[is.na(Data$NumSeedInvestors),"NumSeedInvestors"]<-mean(Data$NumSeedInvestors,na.rm=TRUE) 
Data[is.na(Data$NumFounderRecognition),"LastFundingRoundAmount"]<-mean(Data$NumFounderRecognition,na.rm=TRUE) 
Data[is.na(Data$GooglePageRank),"GooglePageRank"]<-mean(Data$GooglePageRank,na.rm=TRUE) 
Data[is.na(Data$NumDirectCompetitors),"NumDirectCompetitors"]<-mean(Data$NumDirectCompetitors,na.rm=TRUE) 
Data[is.na(Data$EmployeesPerYear),"EmployeesPerYear"]<-mean(Data$EmployeesPerYear,na.rm=TRUE) 
Data[is.na(Data$LastFundingRoundAmount),"LastFundingRoundAmount"]<-mean(Data$LastFundingRoundAmount,na.rm=TRUE)





# Convert Character --> Factor & Add Levels
Data$Status = as.factor(Data$Status)
levels(Data$Status)<-c('Failed','Success')
Data$FocusFunctions = as.factor(Data$FocusFunctions)
Data$TeamSizeGrowth <- tolower(Data$TeamSizeGrowth)
Data$TeamSizeGrowth = as.factor(Data$TeamSizeGrowth)
levels(Data$TeamSizeGrowth)<-c('no','yes')
Data$TopCompanyExp = as.factor(Data$TopCompanyExp)
levels(Data$TopCompanyExp)<-c('No','Yes')
Data$StartupExp = as.factor(Data$StartupExp)
levels(Data$StartupExp)<-c('No','Yes')
Data$SucessfulStartupExp = as.factor(Data$SucessfulStartupExp)
levels(Data$SucessfulStartupExp)<-c('No','Yes')
Data$Big5Partner = as.factor(Data$Big5Partner)
levels(Data$Big5Partner)<-c('No','Yes')
Data$ConsultingExp = as.factor(Data$ConsultingExp)
levels(Data$ConsultingExp)<-c('No','Yes')
Data$ProductorService = as.factor(Data$ProductorService)
levels(Data$ProductorService)<-c('Both','Product', 'Service')
Data$DataFocus = as.factor(Data$DataFocus)
levels(Data$DataFocus)<-c('Both','no','Private','Public')


Data$ConsumerDataFocus = as.factor(Data$ConsumerDataFocus)
levels(Data$ConsumerDataFocus)<-c('No','Yes')
Data$DataStructureFocus = as.factor(Data$DataStructureFocus)
levels(Data$DataStructureFocus)<-c('Both','no', 'Structured','Unstructured')
Data$SubscriptionBased = as.factor(Data$SubscriptionBased)
levels(Data$SubscriptionBased)<-c('No','Yes')
Data$CloudPlatformBased <- tolower(Data$CloudPlatformBased)
Data$CloudPlatformBased = as.factor(Data$CloudPlatformBased)
levels(Data$CloudPlatformBased)<-c('both','cloud', 'none', 'platform')
Data$LocalGlobal <- tolower(Data$LocalGlobal)
Data$LocalGlobal = as.factor(Data$LocalGlobal)
levels(Data$LocalGlobal)<-c('global','local')
Data$BusinessModel = as.factor(Data$BusinessModel)
levels(Data$BusinessModel)<-c('Linear','Non-Linear')
Data$CapitalIntensive = as.factor(Data$CapitalIntensive)
levels(Data$CapitalIntensive)<-c('No','Yes')


Data$CrowdsourcingBased = as.factor(Data$CrowdsourcingBased)
levels(Data$CrowdsourcingBased)<-c('No','Yes')
Data$CrowdfundingBased = as.factor(Data$CrowdfundingBased)
levels(Data$CrowdfundingBased)<-c('No','Yes')
Data$B2CorB2B = as.factor(Data$B2CorB2B)
levels(Data$B2CorB2B)<-c('B2B','B2C')
Data$GlobalExporsure = as.factor(Data$GlobalExporsure)
levels(Data$GlobalExporsure)<-c('No','Yes')
Data$HighestEducation = as.factor(Data$HighestEducation)
levels(Data$HighestEducation)<-c('Bachelors','Masters', 'PhD')
Data$PricingStrategy = as.factor(Data$PricingStrategy)
levels(Data$PricingStrategy)<-c('No','Yes')
Data$HyperLocalisation = as.factor(Data$HyperLocalisation)
levels(Data$HyperLocalisation)<-c('No','Yes')


Data$LongtermFounderRelationship = as.factor(Data$LongtermFounderRelationship)
levels(Data$LongtermFounderRelationship)<-c('No','Yes')
Data$ RecessionSurvival = as.factor(Data$RecessionSurvival )
levels(Data$ RecessionSurvival)<-c('No','Yes')
summary(Data)

#####################################
# yes/no NA's
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Data[is.na(Data$TeamSizeGrowth),"TeamSizeGrowth"]<-Mode(Data$TeamSizeGrowth)
Data[is.na(Data$LongtermFounderRelationship), "LongtermFounderRelationship"]<-Mode(Data$LongtermFounderRelationship)
Data[is.na(Data$RecessionSurvival), "RecessionSurvival"]<-Mode(Data$RecessionSurvival)

#############################################################

## makes characters lowercase
#Data$Status <- tolower(Data$Status)
#Data$FocusFunctions <- tolower(Data$FocusFunctions)
#Data$TopCompanyExp <- tolower(Data$TopCompanyExp)
#Data$LongtermFounderRelationship <- tolower(Data$LongtermFounderRelationship)
#Data$HyperLocalisation <- tolower(Data$HyperLocalisation)
#Data$PricingStrategy <- tolower(Data$PricingStrategy)
#Data$RecessionSurvival <- tolower(Data$RecessionSurvival)
#Data$HighestEducation <- tolower(Data$HighestEducation)

#Data$GlobalExporsure <- tolower(Data$GlobalExporsure)
#Data$CrowdfundingBased <- tolower(Data$CrowdfundingBased)
#Data$ProductorService <- tolower(Data$ProductorService)
#Data$DataFocus <- tolower(Data$DataFocus)
#Data$DataStructureFocus <- tolower(Data$DataStructureFocus)
#Data$CloudPlatformBased <- tolower(Data$CloudPlatformBased)

View(Data)



