library(GGally)
library(corrplot)
library(ggplot2)
library(BlandAltmanLeh)
library(mcr)
library(epiR)
library(lmtest)


#load data
data <- read.csv("data_t2.csv")

data <- subset(data, select = -X)
head(data)
summary(data)

data$diff <- data$Cyfra_Kit1 - data$Cyfra_Kit2

#Normality of Differences
hist(data$diff,
     main = "Histogram of Measurement Differences",
     xlab = "Difference (Kit1 − Kit2)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

qqnorm(data$diff)
qqline(data$diff, col = "red")
shapiro.test(data$diff)

#We assume a normal distribution even tho the test says otherwise

#Scatter Plot

ggplot(data, aes(x = data$Cyfra_Kit1, y = data$Cyfra_Kit2)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  labs(title = "Scatter plot of Chemiluminescence Kits",
       x = "Kit 2 (Cyfra 21-1, ng/mL)",
       y = "Kit 1 (Cyfra 21-1, ng/mL)") +
  theme_minimal()

#Boxplot of the two kits
df_long <- data.frame(
  # Combine all measurement values into one column
  Value = c(data$Cyfra_Kit1,data$Cyfra_Kit2),
  # Create a factor column to identify which kit the value came from
  Kit = factor(rep(c("Kit 1", "Kit 2"), each = nrow(data)))
)

ggplot(df_long, aes(x = Kit, y = Value, fill = Kit)) +
  # Generate the boxplot geometry
  geom_boxplot() +
  # Add points to show every observation (optional, but informative)
  geom_point(position = position_jitter(width = 0.1), alpha = 0.6, size = 1.5) +
  # Customize labels and title
  labs(
    title = "Comparison of Cyfra 21-1 Measurements by Kit",
    y = "Cyfra 21-1 Concentration (ng/mL)",
    x = "Measurement Kit"
  ) +
  # Use a clean theme
  theme_minimal() +
  # Remove the redundant fill legend
  guides(fill = "none")

data_long <- data.frame(
  Sample = rep(1:nrow(data), 2),
  Kit = rep(c("Kit1", "Kit2"), each = nrow(data)),
  Value = c(data$Cyfra_Kit1, data$Cyfra_Kit2)
)

ggplot(data_long, aes(x = Sample, y = Value, color = Kit)) +
  # The 'color = Kit' aesthetic implicitly sets the grouping.
  geom_line(alpha = 0.6) + 
  # geom_point() is often added to highlight the exact measurement location
  geom_point(size = 1.5) + 
  theme_minimal() +
  labs(title = "Spaghetti Plot: Cyfra Levels per Sample by Kit",
       x = "Sample ID", y = "Cyfra Level")

#Bland-Altman Function Call:

#create new column for average measurement
data$avg <- rowMeans(data[, c("Cyfra_Kit1", "Cyfra_Kit2")], na.rm = TRUE) 

#create new column for difference in measurements
mean_diff <- mean(data$diff)

#find lower 95% confidence interval limits
lower <- mean_diff - 1.96*sd(data$diff)

#find upper 95% confidence interval limits
upper <- mean_diff + 1.96*sd(data$diff)

#create Bland-Altman plot
ggplot(data, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement")

#Regression based Bland-Altman 
lm_model <- lm(data$diff ~ data$avg)

fitted_values <- lm_model$fitted.values
residuals <- lm_model$residuals

sd_residuals <- sd(residuals)
loa_upper_regression <- fitted_values + 1.96 * sd_residuals
loa_lower_regression <- fitted_values - 1.96 * sd_residuals

# create plot
plot(data$avg, data$diff, 
     main = "Regression-based Bland-Altman plot", 
     xlab = "Average", 
     ylab = "Difference", 
     pch = 19, col = rgb(0, 0, 1, 0.5), 
     cex = 1.2,
     ylim = c(-4,4))

abline(lm_model, col = "red", lwd = 2)

lines(data$avg, loa_upper_regression, col = "green", lwd = 2, lty = 2)
lines(data$avg, loa_lower_regression, col = "green", lwd = 2, lty = 2)

abline(h = 0, col = "black", lwd = 1, lty = 3)


#  Plotting SDs vs. average data
x_dat2 <- rowMeans(cbind(data$Cyfra_Kit1, data$Cyfra_Kit2))
sd_dat2 <- apply(cbind(data$Cyfra_Kit1, data$Cyfra_Kit2), 1, sd)
plot(x_dat2, sd_dat2, xlab = "Mean", ylab = "SD", main = "standard deviation vs. the mean")

# Deming regression with mcr
#deming_fit = mcreg(data$Cyfra_Kit1, data$Cyfra_Kit2, method.reg = "Deming")
#summary(deming_fit)

# Lin's CCC from epiR
epi.ccc(data$Cyfra_Kit1, data$Cyfra_Kit2)

#Assessing Systematic Bias (Mean different + SD)
bias <- mean(data$diff)
sd_bias <- sd(data$diff)

cat("Mean Bias:", bias, "\nSD of Differences:", sd_bias, "\n")

#Heteroscedasticity Check
ggplot(data, aes(x = avg, y = abs(diff))) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
  labs(
    title = "Heteroscedasticity Check: Absolute Difference vs Average",
    x = "Average Cyfra Concentration",
    y = "|Difference| (Kit1 − Kit2)"
  ) +
  theme_minimal()

#studentized Breusch-Pagan test
lm_model <- lm(diff ~ avg, data = data)
bptest(lm_model)
