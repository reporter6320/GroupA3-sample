data <- read.csv("Data_T3.csv")

# a first look at the data
head(data)
summary(data)

dim(data)

library(ggplot2)
library(dplyr)
library(car)

unique(data$GROUP)

library(ggplot2)

# Create a histogram
ggplot(data, aes(x = NFL)) +
  geom_histogram(bins = 10, color = "black", fill = "lightblue") +
  labs(title = "Histogram of NFL", x = "NFL", y = "Frequency")


# Create a histogram with group information
ggplot(data, aes(x = NFL, fill = GROUP)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 10) +
  labs(title = "Superimposed Histograms of NFL by Group", x = "NFL", y = "Frequency")

ggplot(data, aes(x = ID, y = NFL, color = GROUP)) +
  geom_point() +
  labs(title = "Scatter Plot of NFL by Group", x = "ID", y = "NFL") +
  theme_classic()


# Create a facet plot with group information
ggplot(data, aes(x = ID, y = NFL)) +
  geom_point() +
  facet_wrap(~ GROUP) +
  labs(title = "Scatter Plot of NFL by Group", x = "ID", y = "NFL") +
  theme_classic()

ggplot(data, aes(sample = NFL)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ GROUP) +
  labs(title = "QQ Plot of NFL by Group", x = "Theoretical Quantiles", y = "Observed Quantiles") +
  theme_classic()

# Perform Shapiro-Wilk normality test for NFL concentrations

data %>%
  group_by(GROUP) %>%
  summarise(
    W = shapiro.test(NFL)$statistic,
    p = shapiro.test(NFL)$p.value,
    Result = ifelse(p < 0.05, "fail", "pass")
  )

# Wilcoxon-Mann-Whitney test
wilcox.test(data$NFL[data$GROUP == "Low"], data$NFL[data$GROUP == "Control"])
wilcox.test(data$NFL[data$GROUP == "Medium"], data$NFL[data$GROUP == "Control"])
wilcox.test(data$NFL[data$GROUP == "High"], data$NFL[data$GROUP == "Control"])

# Create a dataframe with results
results <- data.frame(
  Group = c("Low", "Medium", "High"),
  p_value = c(low_result$p.value, medium_result$p.value, high_result$p.value),
  result = ifelse(c(low_result$p.value, medium_result$p.value, high_result$p.value) > 0.05, "pass", "fail")
)

print(results)
