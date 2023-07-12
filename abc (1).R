## k-Nearest Neighbor 
## Naive Bayes

### Variables Used........
# age
# NumSeedInvestors
# NumAngelorVCInvestors
# NumFounders
# NumAdvisors
# SeniorLeadershipTeamSize
# NumFounderRecognition
# GooglePageRank
# NumDirectCompetitors
# EmployeesPerYear
# LastFundingRoundAmount
# FortuneExp




# start by clearing lists 
rm(list=ls())


library(e1071)
library(caTools)
library(class)

data <- read.csv("/Users/Julia/Documents/CS 513/Project/NewData.csv")
View(data)

data <- na.omit(data)
data <- subset(data,select=-X)
data <- subset(data,select=-FocusFunctions)
data <- subset(data,select=-FoundingYear)

str(data)
library(dplyr)
data <- data %>% mutate_if(is.character,as.factor)
str(data)
data <- data %>% mutate_if(is.factor, as.numeric)
str(data)

#library(kknn)
# Splitting data into train
# and test data
split <- sample.split(data, SplitRatio = 0.7)
train_cl <- subset(data, split == "TRUE")
test_cl <- subset(data, split == "FALSE")
# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
train_scale
test_scale <- scale(test_cl[, 1:4])



### AGE -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$Age, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$Age, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.678571428571429"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$Age, k = 3)
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))
"Accuracy = 0.660714285714286"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$Age, k = 5)
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.589285714285714"

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(Age ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$Age, y_pred)
cm
#########################



### NumSeedInvestors -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumSeedInvestors, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumSeedInvestors, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumSeedInvestors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.732142857142857"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumSeedInvestors, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumSeedInvestors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.696428571428571"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumSeedInvestors, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumSeedInvestors)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.642857142857143"

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumSeedInvestors ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumSeedInvestors, y_pred)
cm
#########################



### NumAngelorVCInvestors -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAngelorVCInvestors, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumAngelorVCInvestors, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumAngelorVCInvestors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.696428571428571" 

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAngelorVCInvestors, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumAngelorVCInvestors)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.732142857142857" 

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAngelorVCInvestors, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumAngelorVCInvestors)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.767857142857143"

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumAngelorVCInvestors ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumAngelorVCInvestors, y_pred)
cm
#########################



### NumFounders -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounders, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumFounders, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumFounders)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.303571428571429"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounders, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumFounders)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.410714285714286"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounders, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumFounders)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.392857142857143"

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumFounders ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumFounders, y_pred)
cm
#########################



### NumAdvisors -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAdvisors, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumAdvisors, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumAdvisors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.25"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAdvisors, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumAdvisors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.303571428571429"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAdvisors, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumAdvisors)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.339285714285714"

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumAdvisors ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumAdvisors, y_pred)
cm
#########################



### SeniorLeadershipTeamSize -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$SeniorLeadershipTeamSize, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$SeniorLeadershipTeamSize, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$SeniorLeadershipTeamSize)
print(paste('Accuracy =', 1-misClassError))
# Accuracy = 0.125

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$SeniorLeadershipTeamSize, k = 3)
misClassError <- mean(classifier_knn != test_cl$SeniorLeadershipTeamSize)
print(paste('Accuracy =', 1-misClassError))
# Accuracy = 0.0714285714285714

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$SeniorLeadershipTeamSize, k = 5)
misClassError <- mean(classifier_knn != test_cl$SeniorLeadershipTeamSize)
print(paste('Accuracy =', 1-misClassError))
# Accuracy= 0.107142857142857

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(SeniorLeadershipTeamSize ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$SeniorLeadershipTeamSize, y_pred)
cm
#########################



### NumFounderRecognition -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounderRecognition, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumFounderRecognition, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumFounderRecognition)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.160714285714286

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounderRecognition, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumFounderRecognition)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.160714285714286

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounderRecognition, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumFounderRecognition)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.232142857142857

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumFounderRecognition ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumFounderRecognition, y_pred)
cm
#########################




### GooglePageRank -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$GooglePageRank, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$GooglePageRank, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$GooglePageRank)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.0357142857142857

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$GooglePageRank, k = 3)
misClassError <- mean(classifier_knn != test_cl$GooglePageRank)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.0535714285714286

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$GooglePageRank, k = 5)
misClassError <- mean(classifier_knn != test_cl$GooglePageRank)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.0892857142857143

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(GooglePageRank ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$GooglePageRank, y_pred)
cm
#########################




### NumDirectCompetitors -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumDirectCompetitors, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumDirectCompetitors, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumDirectCompetitors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.375"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumDirectCompetitors, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumDirectCompetitors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.375"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumDirectCompetitors, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumDirectCompetitors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.410714285714286"

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(NumDirectCompetitors ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$NumDirectCompetitors, y_pred)
cm
#########################




### EmployeesPerYear -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$EmployeesPerYear, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$EmployeesPerYear, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$EmployeesPerYear)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.107142857142857"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$EmployeesPerYear, k = 3)
misClassError <- mean(classifier_knn != test_cl$EmployeesPerYear)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.107142857142857"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$EmployeesPerYear, k = 5)
misClassError <- mean(classifier_knn != test_cl$EmployeesPerYear)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.107142857142857"

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(EmployeesPerYear ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$EmployeesPerYear, y_pred)
cm
#########################




### LastFundingRoundAmount -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$LastFundingRoundAmount, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$LastFundingRoundAmount, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$LastFundingRoundAmount)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.196428571428571"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$LastFundingRoundAmount, k = 3)
misClassError <- mean(classifier_knn != test_cl$LastFundingRoundAmount)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.142857142857143"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$LastFundingRoundAmount, k = 5)
misClassError <- mean(classifier_knn != test_cl$LastFundingRoundAmount)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.178571428571429"

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(LastFundingRoundAmount ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$LastFundingRoundAmount, y_pred)
cm
#########################




### FortuneExp -- kNN & NB ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$FortuneExp, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$FortuneExp, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$FortuneExp)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.357142857142857"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$FortuneExp, k = 3)
misClassError <- mean(classifier_knn != test_cl$FortuneExp)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.392857142857143"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$FortuneExp, k = 5)
misClassError <- mean(classifier_knn != test_cl$FortuneExp)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.410714285714286"

#Naive Bayes
# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(FortuneExp ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$FortuneExp, y_pred)
cm
#########################






