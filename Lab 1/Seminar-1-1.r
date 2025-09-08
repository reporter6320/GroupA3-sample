data <- read.csv("Data_T1.csv")

# a first look at the data
head(data)
summary(data)

dim(data)

library(ggplot2)
library(dplyr)

# Create a data frame for plotting (assuming 'data' is the name of your data frame)
df_plot <- data %>%
  select(GROUP, NFL, PTAU181) %>%
  mutate(
    GROUP = factor(GROUP),
    NFL_label = case_when(
      GROUP == 0 ~ "GROUP 0",
      GROUP == 1 ~ "GROUP 1"
    ),
    PTAU181_label = case_when(
      GROUP == 0 ~ "GROUP 0",
      GROUP == 1 ~ "GROUP 1"
    )
  )

# Histograms for NFL and PTAU181
p_NFL <- ggplot(df_plot, aes(x = NFL, fill = NFL_label)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  labs(title = "Histogram of NfL concentrations", x = "NfL (pg/mL)", y = "Count") +
  theme_classic()


p_PTAU181 <- ggplot(df_plot, aes(x = PTAU181, fill = PTAU181_label)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Histogram of p-tau181 concentrations", x = "p-tau181 (pg/mL)", y = "Count") +
  theme_classic()


# Scatter plots for NFL vs PTAU181 within each group
p_NFL_PTAU181 <- ggplot(df_plot, aes(x = PTAU181, y = NFL)) +
  geom_point(aes(color = NFL_label), size = 2) +
  labs(title = "Scatter plot of NfL vs p-tau181", x = "p-tau181 (pg/mL)", y = "NfL (pg/mL)") +
  theme_classic()

# Display plots
print(p_NFL)
print(p_PTAU181)
print(p_NFL_PTAU181)

ggplot(df_plot, aes(x = NFL_label)) +
  geom_boxplot(aes(y = NFL), color = "black", fill = "lightblue") +
  labs(title = "Boxplot of NfL concentrations by group", x = "Group", y = "NfL (pg/mL)") +
  theme_classic()

ggplot(df_plot, aes(x = PTAU181_label)) +
  geom_boxplot(aes(y = PTAU181), color = "black", fill = "lightblue") +
  labs(title = "Boxplot of p-tau181 concentrations by group", x = "Group", y = "p-tau181 (pg/mL)") +
  theme_classic()

df_plot


# Perform Shapiro-Wilk normality test for NFL concentrations
shapiro_test_NFL_1 <- shapiro.test(df_plot[df_plot$GROUP == 1, ]$NFL) 


print(shapiro_test_NFL_1)

# Perform Shapiro-Wilk normality test for NFL concentrations
shapiro_test_NFL_0 <- shapiro.test(df_plot[df_plot$GROUP == 0, ]$NFL) 


print(shapiro_test_NFL_0)

# Perform Shapiro-Wilk normality test for NFL concentrations
shapiro_test_PTAU181_0 <- shapiro.test(df_plot[df_plot$GROUP == 0, ]$PTAU181) 


print(shapiro_test_PTAU181_0)

# Perform Shapiro-Wilk normality test for NFL concentrations
shapiro_test_PTAU181_1 <- shapiro.test(df_plot[df_plot$GROUP == 1, ]$PTAU181) 


print(shapiro_test_PTAU181_1)

# Perform a two-sample t-test
t_test_result <- t.test(df_plot[df_plot$GROUP == 0, ]$NFL, df_plot[df_plot$GROUP == 1, ]$NFL)

# Print the results
print(t_test_result)


log_PTAU181_1 <- log(df_plot[df_plot$GROUP == 1, ]$PTAU181)

shapiro.test(log_PTAU181_1)

# Perform a two-sample t-test
t_test_result_PTAU181 <- t.test(df_plot[df_plot$GROUP == 0, ]$PTAU181, log_PTAU181_1)

# Print the results
print(t_test_result_PTAU181)

