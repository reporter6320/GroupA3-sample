data <- read.csv("Data_T1.csv")
data_NFL_0 <- data$NFL[1:20]
data_PTAU181_0 <- data$PTAU181[1:20]

data_NFL_1 <- data$NFL[21:40]
data_PTAU181_1 <- data$PTAU181[21:40]

print(data_NFL_1)

#Visual representation to test normailty
#Group 0 
#par(mfrow = c(1, 3))
boxplot(data_NFL_0)
hist(data_NFL_0, main = "Histogram of NFL-control", xlab = "Value (pg/mL)",ylab = "Frequency")
qqnorm(data_NFL_0)
qqline(data_NFL_0, col="red")

#boxplot(data_PTAU181_0)
#hist(data_PTAU181_0)
qqnorm(data_PTAU181_0)
qqline(data_PTAU181_0, col="red")

#Group 1 
#boxplot(data_NFL_1)
#hist(data_NFL_1)
qqnorm(data_NFL_1)
qqline(data_NFL_1, col="red")

#boxplot(data_PTAU181_1)
#hist(data_PTAU181_1)
qqnorm(data_PTAU181_1)
qqline(data_PTAU181_1, col="red")

#Shapiro test
#Group 0 
shapiro.test(data_NFL_0)
shapiro.test(data_PTAU181_0)
#Group 1
shapiro.test(data_NFL_1)
shapiro.test(data_PTAU181_1) #We reject the H_0 

#Wilcoxon rank sum test 
wilcox.test(data_NFL_0, data_NFL_1, paired = TRUE, exact=TRUE)
wilcox.test(data_PTAU181_0, data_PTAU181_1, paired = TRUE, exact=TRUE)



t.test(data_NFL_0, data_NFL_1)

#Test wheter data is lognormal or not 
log_data_NFL_0 <- log(data_NFL_0)
log_data_NFL_1 <- log(data_NFL_1)
qqnorm(log_data_NFL_0)
qqnorm(log_data_NFL_1)
shapiro.test(log_data_NFL_0)
shapiro.test(log_data_NFL_1)

log_data_PTAU181_0 <- log(data_PTAU181_0)
log_data_PTAU181_1 <- log(data_PTAU181_1)
qqnorm(log_data_PTAU181_0)
qqnorm(log_data_PTAU181_1)
shapiro.test(log_data_PTAU181_0)
shapiro.test(log_data_PTAU181_1)
