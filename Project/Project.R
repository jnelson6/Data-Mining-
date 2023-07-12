# CS 513 
# Final Project 
# Julia Nelson, Taylor Niedzielski, Sonia Patel, and Noah Suttora

# Dataset Clean-Up

# Clear Environment
rm(list=ls())

# Import Dataset
RawData <- read.csv("/Users/Julia/Documents/CS 513/Project/CAX_Startup_Data.csv", na.string = c("?"))
library(e1071)
library(caret)
View(RawData)

# Remove Columns of DisInterest
Data <- RawData[-c(1,5,6,7,9,10,11,13,14,15,16,17,23,24,25,26,28,34,43,46,47,48,
                   49,50,51,52,53,54,56,57,59,61,62,63,64,65,66,67,71,73,74,75,
                   76,79,80,82,83,84,85,86,87,89,90,91,92,93,98,99,100,101,102,
                   103,104,105,106,107,108,109,110,111,112,113,114,115,116)]

# Rename Columns of Interest
colnames(Data) <- c("Status","FoundingYear","Age","FocusFunctions",
                    "TeamSizeGrowth","NumSeedInvestors","NumAngelorVCInvestors",
                    "NumFounders","NumAdvisors","SeniorLeadershipTeamSize", 
                    "TopCompanyExp","StartupExp","SuccessfulStartupExp",
                    "Big5Partner","ConsultingExp","ProductorService",
                    "DataFocus","ConsumerDataFocus","DataStructureFocus",
                    "SubscriptionBased","CloudPlatformBased","LocalGlobal",
                    "BusinessModel","CapitalIntensive","CrowdsourcingBased",
                    "CrowdfundingBased","B2BorB2C","GlobalExposure",
                    "HighestEducation","Fortune100Exp","Fortune500Exp",
                    "Fortune1000Exp","NumFounderRecognition","PricingStrategy",
                    "HyperLocalisation","LongtermFounderRelationship",
                    "GooglePageRank","NumDirectCompetitors","EmployeesPerYear",
                    "LastFundingRoundAmount","RecessionSurvival")

# Standardized NA
Data[Data == "No Info"] <- NA
Data[Data == "Not Applicable"] <- NA
Data[Data == "not applicable"] <- NA
Data[Data == ""] <- NA
Data[Data == "n"] <- NA
Data[Data == "N"] <- NA
Data[Data == "many"] <- NA
Data[Data == "\\"] <- NA

# Convert Character --> Integer
Data$NumSeedInvestors <- as.integer(Data$NumSeedInvestors, na.rm = TRUE)
Data$NumAngelorVCInvestors <- as.integer(Data$NumAngelorVCInvestors, na.rm = TRUE)
Data$NumFounderRecognition<-as.integer(Data$NumFounderRecognition, na.rm = TRUE)
Data$GooglePageRank <- as.integer(Data$GooglePageRank, na.rm = TRUE)
Data$NumDirectCompetitors <- as.integer(Data$NumDirectCompetitors, na.rm = TRUE)
Data$EmployeesPerYear <- as.integer(Data$EmployeesPerYear, na.rm = TRUE)
Data$LastFundingRoundAmount <- as.integer(Data$LastFundingRoundAmount, na.rm = TRUE)

#Populate NA with Mean of Column
Data[is.na(Data$NumAngelorVCInvestors),"NumAngelorVCInvestors"]<-mean(Data$NumAngelorVCInvestors,na.rm=TRUE) 
Data[is.na(Data$NumSeedInvestors),"NumSeedInvestors"]<-mean(Data$NumSeedInvestors,na.rm=TRUE) 
Data[is.na(Data$NumFounderRecognition),"LastFundingRoundAmount"]<-mean(Data$NumFounderRecognition,na.rm=TRUE) 
Data[is.na(Data$GooglePageRank),"GooglePageRank"]<-mean(Data$GooglePageRank,na.rm=TRUE) 
Data[is.na(Data$NumDirectCompetitors),"NumDirectCompetitors"]<-mean(Data$NumDirectCompetitors,na.rm=TRUE) 
Data[is.na(Data$EmployeesPerYear),"EmployeesPerYear"]<-mean(Data$EmployeesPerYear,na.rm=TRUE) 
Data[is.na(Data$LastFundingRoundAmount),"LastFundingRoundAmount"]<-mean(Data$LastFundingRoundAmount,na.rm=TRUE)

# Target Variable Class & Levels
Data$Status = as.factor(Data$Status)
levels(Data$Status)<-c('Failed','Success')

# FocusFunctions Column Class & Levels
Data$FocusFunctions <- tolower(Data$FocusFunctions)
Data$FocusFunctions = as.factor(Data$FocusFunctions)

# Convert Character --> Factor & Add Levels
Data$TeamSizeGrowth <- tolower(Data$TeamSizeGrowth)
Data$TeamSizeGrowth = as.factor(Data$TeamSizeGrowth)
levels(Data$TeamSizeGrowth)<-c('no','yes')
Data$TopCompanyExp = as.factor(Data$TopCompanyExp)
levels(Data$TopCompanyExp)<-c('No','Yes')
Data$StartupExp = as.factor(Data$StartupExp)
levels(Data$StartupExp)<-c('No','Yes')
Data$SuccessfulStartupExp = as.factor(Data$SuccessfulStartupExp)
levels(Data$SuccessfulStartupExp)<-c('No','Yes')
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
Data$B2BorB2C = as.factor(Data$B2BorB2C)
levels(Data$B2BorB2C)<-c('B2B','B2C')
Data$GlobalExposure = as.factor(Data$GlobalExposure)
levels(Data$GlobalExposure)<-c('No','Yes')
Data$HighestEducation = as.factor(Data$HighestEducation)
levels(Data$HighestEducation)<-c('Bachelors','Masters', 'PhD')
Data$PricingStrategy = as.factor(Data$PricingStrategy)
levels(Data$PricingStrategy)<-c('No','Yes')
Data$HyperLocalisation = as.factor(Data$HyperLocalisation)
levels(Data$HyperLocalisation)<-c('No','Yes')
Data$LongtermFounderRelationship = as.factor(Data$LongtermFounderRelationship)
levels(Data$LongtermFounderRelationship)<-c('No','Yes')
Data[is.na(Data$RecessionSurvival),"RecessionSurvival"]<-'Not Applicable'
Data$RecessionSurvival = as.factor(Data$RecessionSurvival)
levels(Data$RecessionSurvival)<-c('No','Not Applicable','Yes')






# Replace NA's of Yes/No Factor Columns with Mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Data[is.na(Data$TeamSizeGrowth),"TeamSizeGrowth"]<-Mode(Data$TeamSizeGrowth)
Data[is.na(Data$LongtermFounderRelationship), "LongtermFounderRelationship"]<-Mode(Data$LongtermFounderRelationship)
Data[is.na(Data$RecessionSurvival), "RecessionSurvival"]<-Mode(Data$RecessionSurvival)
Data[is.na(Data$DataStructureFocus), "DataStructureFocus"]<-Mode(Data$DataStructureFocus)
Data[is.na(Data$LocalGlobal), "LocalGlobal"]<-Mode(Data$LocalGlobal)
Data[is.na(Data$BusinessModel), "BusinessModel"]<-Mode(Data$BusinessModel)
Data[is.na(Data$CapitalIntensive), "CapitalIntensive"]<-Mode(Data$CapitalIntensive)
Data[is.na(Data$GlobalExposure), "GlobalExposure"]<-Mode(Data$GlobalExposure)
Data[is.na(Data$HighestEducation), "HighestEducation"]<-Mode(Data$HighestEducation)
Data[is.na(Data$PricingStrategy), "PricingStrategy"]<-Mode(Data$PricingStrategy)
Data[is.na(Data$HyperLocalisation), "HyperLocalisation"]<-Mode(Data$HyperLocalisation)
Data[is.na(Data$Fortune100Exp), "Fortune100Exp"]<-Mode(Data$Fortune100Exp)
Data[is.na(Data$Fortune500Exp), "Fortune500Exp"]<-Mode(Data$Fortune500Exp)
Data[is.na(Data$Fortune1000Exp), "Fortune1000Exp"]<-Mode(Data$Fortune1000Exp)

#Get Combined Score for Fortune Company Experience 
Data$Fortune100Exp <- as.integer(Data$Fortune100Exp)
Data$Fortune500Exp <- as.integer(Data$Fortune500Exp)
Data$Fortune1000Exp <- as.integer(Data$Fortune1000Exp)
Data$FortuneExp <- Data$Fortune100Exp + Data$Fortune500Exp + Data$Fortune1000Exp

View(Data)
summary(Data)
summary(Data$FocusFunctions)



# EDA Code for Other Variables
table(Status = Data$Status, TeamSizeGrowth = Data$TeamSizeGrowth)
# result: teams that didn't grow had a higher chance of failing
table(Status = Data$Status, FoundingYear = Data$FoundingYear)
#result: failures mainly from years 2004-2012 
table(Status = Data$Status, NumSeedInvestors = Data$NumSeedInvestors)
# result: doesn't seem to affect if failures occur
table(Status = Data$Status, NumAngelorVCInvestors = Data$NumAngelorVCInvestors)
# result: also an even ratio of failed and success
table(Status = Data$Status, NumFounders = Data$NumFounders)
# result: 2 founders seem to be successful
table(Status = Data$Status, NumAdvisors = Data$NumAdvisors)
# result: teams that didn't grow had a higher chance of failing
table(Status = Data$Status, SeniorLeadershipTeamSize = Data$SeniorLeadershipTeamSize)
# result: evenly distributed
table(Status = Data$Status, TopCompanyExp = Data$TopCompanyExp)
# result: no effect
table(Status = Data$Status, StartupExp = Data$StartupExp)
# result: startup experience helped
table(Status = Data$Status, SuccessfulStartupExp = Data$SuccessfulStartupExp)
table(Status = Data$Status, Big5Partner = Data$Big5Partner)
table(Status = Data$Status, ConsultingExp = Data$ConsultingExp)
table(Status = Data$Status, ProductorService = Data$ProductorService)
table(Status = Data$Status, DataFocus = Data$DataFocus)
table(Status = Data$Status, ConsumerDataFocus = Data$ConsumerDataFocus)
table(Status = Data$Status, DataStructureFocus = Data$DataStructureFocus)
table(Status = Data$Status, SubscriptionBased = Data$SubscriptionBased)
table(Status = Data$Status, CloudPlatformBased = Data$CloudPlatformBased)
table(Status = Data$Status, LocalGlobal = Data$LocalGlobal)
table(Status = Data$Status, BusinessModel = Data$BusinessModel)
table(Status = Data$Status, CapitalIntensive = Data$CapitalIntensive)
table(Status = Data$Status, CrowdsourcingBased = Data$CrowdsourcingBased)
table(Status = Data$Status, CrowdfundingBased = Data$CrowdfundingBased)
table(Status = Data$Status, B2BorB2C = Data$B2BorB2C)
table(Status = Data$Status, GlobalExposure = Data$GlobalExposure)
table(Status = Data$Status, HighestEducation = Data$HighestEducation)
table(Status = Data$Status, Fortune100Exp = Data$Fortune100Exp)
table(Status = Data$Status, Fortune500Exp = Data$Fortune500Exp)
table(Status = Data$Status, Fortune1000Exp = Data$Fortune1000Exp)
table(Status = Data$Status, NumFounderRecognition = Data$NumFounderRecognition)
table(Status = Data$Status, PricingStrategy = Data$PricingStrategy)
table(Status = Data$Status, HyperLocalisation = Data$HyperLocalisation)
table(Status = Data$Status, LongtermFounderRelationship = Data$LongtermFounderRelationship)
table(Status = Data$Status, GooglePageRank = Data$GooglePageRank)
table(Status = Data$Status, NumDirectCompetitors = Data$NumDirectCompetitors)
table(Status = Data$Status, EmployeesPerYear = Data$EmployeesPerYear)
table(Status = Data$Status, LastFundingRoundAmount = Data$LastFundingRoundAmount)
table(Status = Data$Status, RecessionSurvival = Data$RecessionSurvival)
table(Status = Data$Status, FortuneExp = Data$FortuneExp)
table(Status = Data$Status, Age = Data$Age)



# Correlation Coefficients betwteen Status & other Viariables
Data$Status = as.integer(Data$Status) #1 = Failed, 2 = Success

#TopCompanyExp CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, TopCompanyExp = Data$TopCompanyExp)
cor(as.integer(Data$TopCompanyExp), Data$Status)

#StartupExp CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, StartupExp = Data$StartupExp)
cor(as.integer(Data$StartupExp), Data$Status)

#SuccessfulStartupExp CATEGORICAL
table(Status = Data$Status, SuccessfulStartupExp = Data$SuccessfulStartupExp)
cor(as.integer(Data$SuccessfulStartupExp), Data$Status)

#Big5Partner CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, Big5Partner = Data$Big5Partner)
cor(as.integer(Data$Big5Partner), Data$Status)

#ConsultingExp CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, ConsultingExp = Data$ConsultingExp)
cor(as.integer(Data$ConsultingExp), Data$Status)

#ProductorService CATEGORICAL
# 1=Both, 2=Product, 3=Service
table(Status = Data$Status, ProductorService = Data$ProductorService)
cor(as.integer(Data$ProductorService), Data$Status)

#DataFocus CATEGORICAL
# 1=Both, 2=No, 3=Private, 4=Public
table(Status = Data$Status, DataFocus = Data$DataFocus)
cor(as.integer(Data$DataFocus), Data$Status)

#ConsumerDataFocus CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, ConsumerDataFocus = Data$ConsumerDataFocus)
cor(as.integer(Data$ConsumerDataFocus), Data$Status)

#DataStructureFocus CATEGORICAL
# 1=Both, 2=No, 3=Structured, 4=Unstructured
table(Status = Data$Status, DataStructureFocus = Data$DataStructureFocus)
cor(as.integer(Data$DataStructureFocus), Data$Status)

#SubscriptionBased CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, SubscriptionBased = Data$SubscriptionBased)
cor(as.integer(Data$SubscriptionBased), Data$Status)

# save data as new file to use on methods
#write.csv(Data, "C:\\Users\\Julia\\Desktop\\NewData.csv")

################
  



################    Naive Bayes --- STATUS
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(Status ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$Status, y_pred)
cm

################    Naive Bayes --- FoundingYear
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(Status ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$FoundingYear, y_pred)
cm

################    Naive Bayes --- Age
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(Status ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$Age, y_pred)
cm

################    Don't include FocusFunctions

################    Naive Bayes --- TeamSizeGrowth
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(TeamSizeGrowth ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$TeamSizeGrowth, y_pred)
cm

################    Naive Bayes --- NumSeedInvestors
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumSeedInvestors ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumSeedInvestors, y_pred)
cm

################    Naive Bayes --- NumAngelorVCInvestors
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumAngelorVCInvestors ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumAngelorVCInvestors, y_pred)
cm

################    Naive Bayes --- NumFounders
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumFounders ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumFounders, y_pred)
cm

################    Naive Bayes --- NumAdvisors
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumAdvisors ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumAdvisors, y_pred)
cm

################    Naive Bayes --- SeniorLeadershipTeamSize
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(SeniorLeadershipTeamSize ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$SeniorLeadershipTeamSize, y_pred)
cm

################    Naive Bayes --- TopCompanyExp
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(TopCompanyExp ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$TopCompanyExp, y_pred)
cm

################    Naive Bayes --- StartupExp
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(StartupExp ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$StartupExp, y_pred)
cm

################    Naive Bayes --- SuccessfulStartupExp
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(SuccessfulStartupExp ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$SuccessfulStartupExp, y_pred)
cm

################    Naive Bayes --- Big5Partner
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(Big5Partner ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$Big5Partner, y_pred)
cm

################    Naive Bayes --- ConsultingExp
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(ConsultingExp ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$ConsultingExp, y_pred)
cm

################    Naive Bayes --- ProductorService
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(ProductorService ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$ProductorService, y_pred)
cm

################    Naive Bayes --- DataFocus
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(DataFocus ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$DataFocus, y_pred)
cm

################    Naive Bayes --- ConsumerDataFocus
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(ConsumerDataFocus ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$ConsumerDataFocus, y_pred)
cm

################    Naive Bayes --- DataStructureFocus
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(DataStructureFocus ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$DataStructureFocus, y_pred)
cm

################    Naive Bayes --- SubscriptionBased
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(SubscriptionBased ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$SubscriptionBased, y_pred)
cm

################    Naive Bayes --- CloudPlatformBased
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(CloudPlatformBased ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$CloudPlatformBased, y_pred)
cm

################    Naive Bayes --- LocalGlobal
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(LocalGlobal ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$LocalGlobal, y_pred)
cm

################    Naive Bayes --- BusinessModel
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(BusinessModel ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$BusinessModel, y_pred)
cm

################    Naive Bayes --- CapitalIntensive
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(CapitalIntensive ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$CapitalIntensive, y_pred)
cm

################    Naive Bayes --- CrowdsourcingBased
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(CrowdsourcingBased ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$CrowdsourcingBased, y_pred)
cm

################    Naive Bayes --- CrowdfundingBased
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(CrowdfundingBased ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$CrowdfundingBased, y_pred)
cm

################    Naive Bayes --- B2BorB2C
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(B2BorB2C ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$B2BorB2C, y_pred)
cm

################    Naive Bayes --- GlobalExposure
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(GlobalExposure ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$GlobalExposure, y_pred)
cm

################    Naive Bayes --- HighestEducation
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(HighestEducation ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$HighestEducation, y_pred)
cm

################    POSSIBLE OMIT -------------- NumFounderRecognition
################    Naive Bayes --- NumFounderRecognition
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumFounderRecognition ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumFounderRecognition, y_pred)
cm

################    Naive Bayes --- PricingStrategy
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(PricingStrategy ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$PricingStrategy, y_pred)
cm

################    Naive Bayes --- HyperLocalisation
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(HyperLocalisation ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$HyperLocalisation, y_pred)
cm

################    Naive Bayes --- LongtermFounderRelationship
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(LongtermFounderRelationship ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$LongtermFounderRelationship, y_pred)
cm

################    POSSIBLE OMIT 
################    Naive Bayes --- GooglePageRank
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(GooglePageRank ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$GooglePageRank, y_pred)
cm

################    Naive Bayes --- NumDirectCompetitors
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumDirectCompetitors ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumDirectCompetitors, y_pred)
cm

################    Naive Bayes --- EmployeesPerYear
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(EmployeesPerYear ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$EmployeesPerYear, y_pred)
cm

################    Naive Bayes --- LastFundingRoundAmount
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(LastFundingRoundAmount ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$LastFundingRoundAmount, y_pred)
cm

################    Naive Bayes --- RecessionSurvival
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(RecessionSurvival ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$RecessionSurvival, y_pred)
cm

################    Naive Bayes --- FortuneExp
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(FortuneExp ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$FortuneExp, y_pred)
cm












# DO NOT RUN 
# Master List of Columns in Data
Data$Status
Data$FoundingYear
Data$Age
Data$FocusFunctions
Data$TeamSizeGrowth
Data$NumSeedInvestors
Data$NumAngelorVCInvestors
Data$NumFounders
Data$NumAdvisors
Data$SeniorLeadershipTeamSize
Data$TopCompanyExp
Data$StartupExp
Data$SuccessfulStartupExp
Data$Big5Partner
Data$ConsultingExp
Data$ProductorService
Data$DataFocus
Data$ConsumerDataFocus
Data$DataStructureFocus
Data$SubscriptionBased
Data$CloudPlatformBased
Data$LocalGlobal
Data$BusinessModel
Data$CapitalIntensive
Data$CrowdsourcingBased
Data$CrowdfundingBased
Data$B2BorB2C
Data$GlobalExposure
Data$HighestEducation
Data$Fortune100Exp
Data$Fortune500Exp
Data$Fortune1000Exp
Data$NumFounderRecognition
Data$PricingStrategy
Data$HyperLocalisation
Data$LongtermFounderRelationship
Data$GooglePageRank
Data$NumDirectCompetitors
Data$EmployeesPerYear
Data$LastFundingRoundAmount
Data$RecessionSurvival
Data$FortuneExp