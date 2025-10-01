#Task 2 

#1-fasted
data_2 <- read.csv("Data_T2.csv") 
summary(data_2)

#We start with plotting the data
plot(data_2$X,data_2$GE_fasted, main = "GE_fasted", xlab = "Time [min]", ylab = "Gastric volume")
#We notice that the observed data follows a decay like curve.


#we try to fit an exponential decay model
fasted_fit <- nls(GE_fasted ~ GE0 * exp(-k * X),
                  data = data_2,
                  start = list(GE0 = max(data_2$GE_fasted), k = 0.01))

#We see the parameters in the summary of the fit
summary(fasted_fit)
plot(fasted_fit)

#We assume the model is a good fit, according to the: Std. Error, residuals and p-value.
#We can now use the k to determine the half life
coef_fasted <- coef(fasted_fit)
t_half_fasted <- log(2)/coef_fasted["k"]


#We now use our model to predict data and compare it to the actual data

time_seq <- seq(min(data_2$X), max(data_2$X), length.out = 60)
pred_fasted <- predict(fasted_fit, newdata_fasted = data.frame(X = time_seq))

plot(data_2$X, data_2$GE_fasted, pch = 16, main = "Observed vs Fitted_fasted",  xlab = "Time [min]", ylab = "Gastric volume")
lines(time_seq, pred_fasted, col = "red", lwd = 2)


#ALternative approash 
#Log and not linear regression?

log_GE_fasted <- log(data_2$GE_fasted)

#We see that we get a linear relationship
plot(data_2$X,log_GE_fasted, main = "Log GE_fasted", xlab = "Time [min]", ylab = "Gastric volume log")

#we can now use a linear model to determine t_half
lm_GE_fasted <- lm(formula = log_GE_fasted ~ X, data=data_2)
summary(lm_GE_fasted)


#2-fed

#By looking at the data we see there are to negative value, we remove them 
GE_fed_data <- data_2$GE_fed[data_2$GE_fed >= 0]
length(GE_fed_data)
X_data <- data_2$X[data_2$GE_fed > 0]

#We start with plotting the data
plot(X_data,GE_fed_data, main = "GE_fed", xlab = "Time [min]", ylab = "Gastric volume")


# Fed-state fit
fed_fit <- nls(
  GE_fed_data ~ GE0 * exp(-(k * X_data)^b),
  start = list(GE0 = max(GE_fed_data), k = 0.01, b = 1),
  control = nls.control(maxiter = 100)
)

summary(fed_fit)

# Predictions
time_seq_fed <- seq(min(X_data), max(X_data), length.out = 58)
pred_fed <- predict(fed_fit, newdata = data.frame(X = time_seq_fed))

# Plot
plot(X_data, GE_fed_data, pch = 16, main = "Observed vs Fitted_fed",
     xlab = "Time [min]", ylab = "Gastric volume")
lines(time_seq_fed, pred_fed, col = "red", lwd = 2)

#We can now use the k to determine the half life
coef_fed <- coef(fed_fit)
t_half_fed <- log(2)/coef_fed["k"]

#Alternative approach 
#Log and not linear regression?

log_GE_fed <- log(Ge_fed_data)

plot(X_data,log_GE_fed, main = "Log GE_fed", xlab = "Time [min]", ylab = "Gastric volume log")

lm_GE_fed <- lm(formula = log_GE_fed ~ X_data)
summary(lm_GE_fed)

#Different shapes can give the same t₅₀. 
#Two subjects can have identical t₅₀ but very different early retention (important clinically)
#The half life calculation assumes expontetial decay, and does not account for lag. 

#example of a another paramter. 
#T-lag25 time until 25% of gastric volume is empatied. 

threshold_fed <- 0.75 * coef_fed["GE0"]
  
idx_fed <- which.min(abs(pred_fed - threshold_fed)) 
T25_fed<- time_seq_fed[idx_fed] 
pred_at_T25_fed <- pred[idx_fed]


threshold_fasted <- 0.75 * coef_fasted["GE0"]

idx_fasted <- which.min(abs(pred_fasted- threshold_fasted)) 
T25_fasted <- time_seq[idx_fasted] 
pred_at_T25_fasted <- pred[idx_fasted]

#example of a another paramter. 
#TIme at Stepest decline (most negatvie derivate)
