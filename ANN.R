# CS 513 
# Final Project 
# Julia Nelson, Taylor Niedzielski, Sonia Patel, and Noah Suttora

# ANN on Cleaned Dataset (Noah Suttora)

# clear Environment
rm(list=ls())

# import cleaned dataset and make all columns factor
Data = read.csv("/CS513/Final/Cleaned_Data.csv",
                colClasses = c(rep("factor", 42)))

# don't use FocusFunctions, no correlation, and very weak correlation
Data = Data[, ! names(Data) %in% c("FocusFunctions", "DataStructureFocus",
                                   "NumFounderRecognition", "NumSeedInvestors",
                                   "NumAngelorVCInvestors", "NumFounders",
                                   "TopCompanyExp", "ProductorService",
                                   "DataFocus", "StartupExp",
                                   "SuccessfulStartupExp", "Big5Partner",
                                   "BusinessModel", "CapitalIntensive",
                                   "GlobalExposure", "PricingStrategy",
                                   "HyperLocalisation")]

# convert all columns to numeric
Data <- data.frame(lapply(Data, as.numeric))

# train/test split (70% train, 30% test)
idx <- sort(sample(nrow(Data), as.integer(.70*nrow(Data))))
train <- Data[idx,]
test <- Data[-idx,]

# neural network package
library("neuralnet")


# (1) neural network with 5 hidden layers
net_Data <- neuralnet(Status~., train, hidden=5, threshold=0.01)

# plot neural network
plot(net_Data)

# predict without Status column
ann <- compute(net_Data, test[,-1])
ann_cat <- ifelse(ann$net.result<1.2, 1, 2)

# confusion matrix
conf_mat = table(Actual=test$Status, prediction=ann_cat)
conf_mat

accuracy <- sum(diag(conf_mat))/nrow(test) * 100
accuracy


# (2) neural network with 10 hidden layers
net_Data <- neuralnet(Status~., train, hidden=10, threshold=0.01)

# plot neural network
plot(net_Data)

# predict without Status column
ann <- compute(net_Data, test[,-1])
ann_cat <- ifelse(ann$net.result<1.2, 1, 2)

# confusion matrix
conf_mat = table(Actual=test$Status, prediction=ann_cat)
conf_mat

accuracy <- sum(diag(conf_mat))/nrow(test) * 100
accuracy