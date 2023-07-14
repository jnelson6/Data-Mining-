### Problem 6.58 ###
z <- 1.77
# a) 
P_value <- pnorm(z, lower.tail = FALSE)
P_value
## The P-value is 0.0384.
# b) 
P_value <- pnorm(z, lower.tail = TRUE)
P_value
## The P-value is 0.9616.
# c) 
P_value <- 2*pnorm(-abs(z))
P_value
## The P-value is 0.0767.

### Problem 6.59 ###
z <- -1.69
# a) 
P_value <- pnorm(z, lower.tail = FALSE)
P_value
## The P-value is 0.9545.
# b) 
P_value <- pnorm(z, lower.tail = TRUE)
P_value
## The P-value is 0.04551.
# c) 
P_value <- 2*pnorm(-abs(z))
P_value
## The P-value is 0.0910.

### Problem 6.71 ###
# a) 
mean <- 115
sd <- 30
sample_mean <- 127.8
n <- 25
t <- (sample_mean-mean)/(sd/sqrt(n))
t
P_value <- pt(-abs(t),df=n-1)
P_value
## The P-value is 0.0217.
## Since the P-value is small, less than 0.05, we reject the null hypothesis.
## Thus, there is evidence that the population mean is larger than 115.
# b) 
# The sample needs to be independently and randomly chosen from the popualtion.
# Additionally, the data needs to be normally distributed. 
# The most important to the validity of our conclusion is the nearly normal condition. 
# If the data is not nearly normal, the probabilities will be incorrect, which is what we base our conclusion on. 

### Problem 6.73 ###
n <- 20
sigma <- 3
diff <- c(5.0, 6.5, -0.6, 1.7, 3.7, 4.5, 8, 2.2, 4.9, 3, 4.4, 0.1, 3.0, 1.1, 1.1, 5, 2.1, 3.7, -0.6, -4.2)
m <- mean(diff)
m
## H0: The mean difference is 0. 
## Ha: The mean difference is not 0.

# b) 
t <- (m-0)/(sigma/sqrt(n))
t
P_value <- 2*pt(-abs(t),df=n-1)
P_value
## The P-value is 0.00065.
## Since the P-value is so small, we reject the null hypothesis. 
## There is evidence that the mean difference is not 0.
## There is evidence to suggest a difference in the computer and driver's calculations, on average. 
## The computer may be miscalibrated. 

### Problem 6.99 ###
# a) 
mean <- 2403.7
sample_mean <- 2453.7
sd <- 880
n1 <- 100
t <- (sample_mean-mean)/(sd/sqrt(n1))
t
P_value <- pt(-abs(t),df=n1-1)
P_value
## The P-value is 0.2856.
# b) 
n2 <- 500 
t <- (sample_mean-mean)/(sd/sqrt(n2))
t
P_value <- pt(-abs(t),df=n2-1)
P_value
## The P-value is 0.1023.
# c) 
n3 <- 2500 
t <- (sample_mean-mean)/(sd/sqrt(n3))
t
P_value <- pt(-abs(t),df=n3-1)
P_value
## The P-value is 0.0023.

### Problem 6.120 ### 
x <- c(0, 1, 2, 3, 4, 5, 6)
p_0 <- c(0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.3)
p_1 <- c(0.2, 0.2, 0.2, 0.1, 0.1, 0.1, 0.1)
data <- data.frame(cbind(x, p_0, p_1))

# a) 
# X <= 2 means X = 0 or 1 or 2
# P(Type 1 Error) = P(X = 0 or X = 1 or X = 2) = P(X = 0) + P(X = 1) + P(X = 2)
# Use the probabilities from p_0.
P_type1 <- 0.1 + 0.1 + 0.2
P_type1
## There is a probability of type 1 error of 0.40, or 40%.
# b) 
# P(Type 2 Error) = P(X = 3 or X = 4 or X = 5 or X = 6) = P(X = 3) + P(X = 4) + P(X = 5) + P(X = 6)
# Use the probabilities from p_1.
P_type2 <- 0.1 + 0.1 + 0.1 + 0.1
P_type2
## There is a probability of type 2 error of 0.40, or 40%.

### Problem 7.22 ###
# a) 
n <- 16
t <- 2.15
mean <- 8
df <- n - 1
df
## There are 15 degrees of freedom.

# b) 
## 2.131 < t < 2.602
## The critical t-values are 2.131 and 2.602.
# c) 
## 0.01 < P < 0.025
## The P-value is between 0.01 and 0.025.
# d) 
## At the 5% level, since P-value < 5%, the result IS statistically significant.
## At the 1% level, since P-value > 1%, the result IS NOT statistically significant.
# e) 
P_value <- pt(-abs(t),df=n-1)
P_value
## The P-value is 0.0241.

### Problem 7.23 ###
# a) 
n <- 27
t <- 2.01
mean <- 40
df <- n - 1
df
## There are 26 degrees of freedom.
# b) 
## 1.706 < t < 2.056
## The critical t-values are 1.706 and 2.056.
# c) 
## 0.05 < P < 0.10
## The P-value is between 0.05 and 0.10.
# d) 
## At the 5% level, since P-value > 5%, the result IS NOT statistically significant.
## At the 1% level, since P-value > 1%, the result IS NOT statistically significant.
# e) 
P_value <- 2*pt(-abs(t),df=n-1)
P_value
## The P-value is 0.0549.


