# CS 513 
# Homework 7
# Julia Nelson
# I pledge my honor that I have abided by the Stevens Honor System.

# ANN 
# The “breast cancer dataset” (wisc_bc_ContinuousVar.csv) in CANVAS was obtained from the University of Wisconsin Hospitals, Madison from Dr. William H. Wolberg. 
# The features in the dataset have been categorized.
# 7.1  
# Use the ANN methodology with five (5) nodes in the hidden layer, to develop a classification model for the Diagnosis.

# clear Environment
rm(list=ls())

# neural network package
library("neuralnet")

# import cleaned dataset and make all columns factor
Data <- read.csv("/Users/Julia/Downloads/wisc_bc_ContinuousVar.csv", colClasses = c(rep("factor", 31)))
View(Data)


# cbind needed to help with compute below
Data <- data.frame( cbind(radius_mean=Data$radius_mean, diagnosis=Data$diagnosis, texture_mean=Data$texture_mean, perimeter_mean=Data$perimeter_mean,
                           area_mean=Data$area_mean, smoothness_mean=Data$smoothness_mean, compactness_mean=Data$compactness_mean, concavity_mean=Data$concavity_mean,
                           concave.points_mean=Data$concave.points_mean, symmetry_mean=Data$symmetry_mean, fractal_dimension_mean=Data$fractal_dimension_mean,
                           radius_se=Data$radius_se, texture_se=Data$texture_se, perimeter_se=Data$perimeter_se,area_se=Data$area_se, smoothness_se=Data$smoothness_se,
                           compactness_se=Data$compactness_se, concavity_se=Data$concavity_se, concave.points_se=Data$concave.points_se, symmetry_se=Data$symmetry_se,
                           fractal_dimension_se=Data$fractal_dimension_se, radius_worst=Data$radius_worst, texture_worst=Data$texture_worst, perimeter_worst=Data$perimeter_worst,
                           area_worst=Data$area_worst, smoothness_worst=Data$smoothness_worst, compactness_worst=Data$compactness_worst, concavity_worst=Data$concavity_worst,
                           concave.points_worst=Data$concave.points_worst, symmetry_worst=Data$symmetry_worst, fractal_dimension_worst=Data$fractal_dimension_worst))
View(Data)

# convert all columns to numeric
Data <- data.frame(lapply(Data, as.numeric))

View(Data)
# train/test split (70% train, 30% test)
index <- sort(sample(nrow(Data), as.integer(.70*nrow(Data))))
train <- Data[index,]
test <- Data[-index,]

# neural network with 5 hidden layers
net_Data <- neuralnet(diagnosis~., train, hidden=5, threshold=0.01)

# plot neural network
plot(net_Data)

# predict without Diagnosis column
ANNpred <- compute(net_Data, test[,-1]) 
ANN_cat <- as.numeric(ANNpred$net.result)
#net_Data$result.matrix

ANN_cat <- ifelse(ANN_cat < 1.2, 1, 2)
# confusion matrix
conf_mat = table(Actual=test$diagnosis, prediction=ANN_cat)
conf_mat



# accuracy
accuracy <- sum(diag(conf_mat))/nrow(test) * 100
accuracy


