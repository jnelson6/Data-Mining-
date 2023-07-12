library(e1071)
library(caTools)
library(class)
library(kknn) 

#remove(list=ls())

data <- read.csv("/Users/Julia/Documents/CS 513/Project/NewData.csv")
View(data)



data <- na.omit(data)
data <- subset(data,select=-X)
data <- subset(data,select=-FocusFunctions)
data <- subset(data,select=-FoundingYear)

data <- lapply(data,as.numeric)
#warnings()

data <- data.frame(data)
View(data)
# Splitting data into train
# and test data
split <- sample.split(data, SplitRatio = 0.7)
train_cl <- subset(data, split == "TRUE")
test_cl <- subset(data, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Age,
                      k = 1)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$Age, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Age,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Age,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))



#Naive Bayes 
# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl_Naive<-naiveBayes(Age ~ ., data = train_cl)
classifier_cl_Naive
# Predicting on test data'
y_pred <- predict(classifier_cl_Naive, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$Age, y_pred)
cm



