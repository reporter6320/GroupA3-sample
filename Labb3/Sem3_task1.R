#Task 1 

#Explore the dataset
data <- read.csv("Data_T1.csv")
data1 <- read.csv("Data_T1.csv")
str(data)
head(data)
summary(data)
colSums(is.na(data)) #Check for missing data
data <- subset(data, select = -X)

library(GGally)
library(corrplot)
library(ggplot2)

#Ensure the columns are factors
data$Region <- as.factor(data$Region)
data$AgeGroup <- as.factor(data$AgeGroup)


# Convert Sex to binary
data$Sex <- ifelse(data$Sex == "Male", 1, 0)

data_glm <- data  

# Numeric version for correlation
dummies <- model.matrix(~ Region + AgeGroup, data = data_glm)
dummies <- subset(dummies, select = -`(Intercept)`)  # drop intercept
data_numeric <- cbind(subset(data_glm, select = -c(Region, AgeGroup)), dummies)

cor_dat = cor(data_numeric)
corrplot(cor_dat)
#When looking at the corrplot we see that age seems to correlate with New Cases


#NewCases vs Categorical variabels
ggplot(data_glm,aes(NewCases))+geom_histogram(bins=7)

ggplot(data_glm, aes(x = AgeGroup, y = NewCases, fill = AgeGroup)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "NewCases by Age Group",
       x = "Age Group",
       y = "NewCases") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = "none")

ggplot(data1, aes(x = Sex, y = NewCases, fill = Sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "NewCases by Sex",
       x = "Sex",
       y = "NewCases") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = "none")

ggplot(data_glm, aes(x = Region, y = NewCases, fill = Region)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "NewCases by Region",
       x = "Region",
       y = "NewCases") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = "none")

library(ggpubr)

#NewCases vs continous variabels

#Relationship between CLIstd and NewCases
ggplot(data_glm, aes(x = CLIstd, y = NewCases)) +
  geom_point(color = "steelblue", alpha = 0.6) +        # scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # regression line
  stat_cor(method = "spearman", label.x = 0.7*max(data$CLIstd, na.rm=TRUE), 
           label.y = 0.9*max(data$NewCases, na.rm=TRUE)) +   # adds correlation r & p-value
  theme_minimal() +
  labs(title = "Relationship between CLIstd and NewCases",
       x = "CLIstd",
       y = "NewCases")
#Relationship between SmokingPrevalence and NewCases
ggplot(data_glm, aes(x = SmokingPrevalence, y = NewCases)) +
  geom_point(color = "steelblue", alpha = 0.6) +        # scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # regression line
  stat_cor(method = "spearman", label.x = 0.7*max(data$SmokingPrevalence, na.rm=TRUE), 
           label.y = 0.9*max(data$NewCases, na.rm=TRUE)) +   # adds correlation r & p-value
  theme_minimal() +
  labs(title = "Relationship between SmokingPrevalence and NewCases",
       x = "SmokingPrevalence",
       y = "NewCases")

#Relationship between BMImedian  and NewCases
ggplot(data_glm, aes(x = BMImedian , y = NewCases)) +
  geom_point(color = "steelblue", alpha = 0.6) +        # scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # regression line
  stat_cor(method = "spearman", label.x = 0.7*max(data$BMImedian , na.rm=TRUE), 
           label.y = 0.9*max(data$NewCases, na.rm=TRUE)) +   # adds correlation r & p-value
  theme_minimal() +
  labs(title = "Relationship between BMImedian  and NewCases",
       x = "BMImedian ",
       y = "NewCases")


#Intervariable correlation

#BMImedian by Region
ggplot(data_glm, aes(x = Region, y = BMImedian, fill = Region)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "BMImedian by Region",
       x = "Region",
       y = "BMImedian") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = "none")

#Relationship between BMImedian and CLIstd
ggplot(data_glm, aes(x = BMImedian, y = CLIstd)) +
  geom_point(color = "steelblue", alpha = 0.6) +        # scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # regression line
  stat_cor(method = "spearman", label.x = 0.7*max(data$BMImedian, na.rm=TRUE), 
           label.y = 0.9*max(data$CLIstd, na.rm=TRUE)) +   # adds correlation r & p-value
  theme_minimal() +
  labs(title = "Relationship between BMImedian and CLIstd",
       x = "BMImedian",
       y = "CLIstd")

#Split into traing and validation
#Because of the small data set we choose not to split the data
#Start fitting models
library(car)
library(DHARMa)
library(interactions)
library(rsq)
library(MASS)


#Model 1 : using all predictors
model1 <- glm(NewCases ~ Sex + AgeGroup + Region + CLIstd + BMImedian + SmokingPrevalence,
              data = data_glm, family = poisson(link="log"), offset = log(Npopulation))
summary(model1)
BIC(model1)
rsq(model1, adj=TRUE)  
testDispersion(model1)
simres1 <- simulateResiduals(model1)
plot(simres1)
  
#Model 2: Using CLIstd*RegionNum as an interaction term
model2 <- glm(NewCases ~ Sex + AgeGroup + CLIstd*Region + BMImedian + SmokingPrevalence,
              data = data_glm, family = poisson(link="log"), offset = log(Npopulation))
summary(model2)
BIC(model2)
rsq(model2, adj=TRUE) 
interact_plot(model2, pred = "CLIstd", modx = "Region")
testDispersion(model2)
simres2 <- simulateResiduals(model2)
plot(simres2)

#Model 3: Using CLIstd*Sex as an interaction term and we drop Region
model3 <- glm(NewCases ~ AgeGroup + CLIstd*Sex + BMImedian + SmokingPrevalence,
              data = data_glm, family = poisson(link="log"), offset = log(Npopulation))
summary(model3)
BIC(model3)
rsq(model3, adj=TRUE)
interact_plot(model3, pred = "CLIstd", modx = "Sex")
testDispersion(model3)
simres3 <- simulateResiduals(model3)
plot(simres3)

#Model 4: Using CLIstd*BMImedian as an interaction term
model4 <- glm(NewCases ~ AgeGroup + CLIstd*BMImedian + Sex+ SmokingPrevalence,
              data =data_glm, family = poisson(link="log"), offset = log(Npopulation))
summary(model4)
BIC(model4)
rsq(model4, adj=TRUE)  # pseudo-R² for GLMs
interact_plot(model4, pred = "CLIstd", modx = "BMImedian")
testDispersion(model4)
simres4 <- simulateResiduals(model4)
plot(simres4)



#Compare models

# Create a list of models
model_list <- list(
  model1 = model1,
  model2 = model2,
  model3 = model3,
  model4 = model4
)

# Initialize empty data frame
model_summary <- data.frame(
  Model = character(),
  AIC = numeric(),
  BIC = numeric(),
  Rsq_adj = numeric(),
  stringsAsFactors = FALSE
)

# Loop over models to fill the summary
for (i in seq_along(model_list)) {
  m <- model_list[[i]]
  
  model_summary <- rbind(model_summary, data.frame(
    Model = names(model_list)[i],
    AIC = AIC(m),
    BIC = BIC(m),
    Rsq_adj = rsq(m, adj = TRUE)  # pseudo-R² for GLMs
  ))
}

# Print summary table
print(model_summary)



library(glmnet)

#We try using LASSO 
# Create matrix of predictors (without response)
X <- model.matrix(~ AgeGroup + Region + CLIstd*BMImedian+ Sex + SmokingPrevalence, data = data_glm)[, -1]

# Response
y <- data_glm$NewCases

# Offset
offset_var <- log(data_glm$Npopulation)

set.seed(123)
cv_lasso <- cv.glmnet(X, y, 
                      family = "poisson", 
                      offset = offset_var,
                      alpha = 1)  # alpha = 1 -> LASSO

plot(cv_lasso)
cv_lasso$lambda.min     # lambda that minimizes cross-validation error

coef(cv_lasso, s = "lambda.min")

#Model 5: with lambda that minimizes cross-validation error
model5 <- glm(NewCases ~ AgeGroup + CLIstd +SmokingPrevalence + Region,
              data = data_glm, family = poisson(link="log"), offset = log(Npopulation))
summary(model5)
BIC(model5)
rsq(model5, adj=TRUE)
testDispersion(model5)
simres5 <- simulateResiduals(model5)
plot(simres5)


#We try using Elastic net
set.seed(123)
cv_elastic <- cv.glmnet(
  X, y, 
  family = "poisson", 
  offset = offset_var,
  alpha = 0.5,  # Elastic Net
  nfolds = 10
)

# Coefficients
coef(cv_elastic, s = "lambda.min")



#Trying to predict data with the "best" model 

library(gridExtra)

# Predictions
data$pred_newCases1 <- predict(model4, newdata = data, type = "response", na.action = na.pass)
data$pred_newCases2 <- predict(model5, newdata = data, type = "response", na.action = na.pass)

# Plot 1
p1 <- ggplot(data, aes(x = NewCases, y = pred_newCases1)) + 
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme_minimal() +
  labs(title = "Model 4 Predictions vs Observed",
       x = "Observed New Cases", y = "Predicted New Cases")

# Plot 2
p2 <- ggplot(data, aes(x = NewCases, y = pred_newCases2)) + 
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme_minimal() +
  labs(title = "Model 5 Predictions vs Observed",
       x = "Observed New Cases", y = "Predicted New Cases")

# Side-by-side comparison
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
