# CS 513 
# Homework 8
# Julia Nelson
# I pledge my honor that I have abided by the Stevens Honor System.

# hcluster & kmeans
# The “breast cancer dataset” (wisc_bc_ContinuousVar.csv) in CANVAS was obtained from the University of Wisconsin Hospitals, Madison from Dr. William H. Wolberg. 
# The features in the dataset have been categorized.



# clear Environment
rm(list=ls())



# import cleaned dataset and make all columns factor
data <- read.csv("/Users/Julia/Downloads/wisc_bc_ContinuousVar.csv", colClasses = c(rep("factor", 31)))
Data <- data.frame(data)

# 8.1
# Using hclust, categorize the “wisc_bc_ContinuousVar.csv” data into two (2) clusters based on. 
# All the features except the diagnosis column. 
# Tabulate the clustered rows against the “diagnosis” column. 
# (Remove the rows with missing values first)
#
Data <- na.omit(Data) 

# Factor the Data
Data <- Data[-1]
Data_dist <- dist(Data[,-1])
#cluster the distributed
results <- hclust(Data_dist)
plot(results)
# cut into 2 clusters 
cluster_Cut_2 <- cutree(results,2)
table(cluster_Cut_2, Data$diagnosis)




# 8.2
# Using k-means, categorize the “wisc_bc_ContinuousVar.csv” data into two (2) clusters based on. 
# All the features except the diagnosis column. 
# Tabulate the clustered rows against the “diagnosis” column. 
# (Remove the rows with missing values first)
# clear Environment
rm(list=ls())

# import cleaned dataset and make all columns factor
data <- read.csv("/Users/Julia/Downloads/wisc_bc_ContinuousVar.csv", colClasses = c(rep("factor", 31)))
Data <- data.frame(data)

Data <- na.omit(Data) 

# Factor the Data to remove id
Data <- Data[-1]
# 2 clusters   without diagnosis ( Data[,-1]) 
results <- kmeans(Data[,-1],2)
table(results$cluster, Data[,1])

#results$cluster


plot( Data[,1], col = results$cluster)
#points(results$centers, col = 1:5, pch = 8)





