Raw_1 <- read.csv("D:\\MSc\\Statistics\\Siminar1\\Data_T1.csv")
Raw_1
AD_Raw <- subset(Raw_1, GROUP == 1)
AD_Raw
NFL_AD <- Raw_AD$NFL
log_NFL_AD = log(NFL_AD)
par(mfrow = c(2,1))
hist(log_NFL_AD, prob = TRUE, main = "Histogram: log(NFL)", xlab = "log(NFL)")
qqnorm(log_NFL_AD, main = "QQ plot: NFL (raw)")
shapiro_log <- shapiro.test(log_NFL_AD)
shapiro_log
d_target = abs(log(1 - 0.3))
d_target
sigma_log <- sd(log_NFL_AD, na.rm = TRUE)
sigma_log
install.packages("pwr")
library(pwr)
alpha <- 0.05
power <- 0.85
n <- pwr.t.test(d = d_target / sigma_log, sig.level = alpha, power = power, type = "two.sample")
n
N = ceiling(n$n)
N
install.packages('devtools')
library(devtools)
install_github("adamdarwichkth/CM2018rpackage")
library(CM2018rpackage)
my_dataframe <- ad_trial_data(n_per_arm = N)
my_dataframe
AD_Treat <- subset(my_dataframe, GROUP == 1)
AD_Treat
AD_Control <- subset(my_dataframe, GROUP == 0)
AD_Control
Treat_NFL = AD_Treat$NFL
Control_NFL = AD_Control$NFL
mean_tr = mean(Treat_NFL)
mean_co = mean(Control_NFL)
sd_tr = sd(Treat_NFL)
sd_co = sd(Control_NFL)
log_Tr_NFL = log(Treat_NFL)
log_Co_NFL = log(Control_NFL)
logmean_co = mean(log_Co_NFL)
logmean_tr = mean(log_Tr_NFL)
logsd_tr = sd(log_Tr_NFL)
logsd_co = sd(log_Co_NFL)
logmean_co
logmean_tr
logsd_co
logsd_tr
mean_co
mean_tr
sd_co
sd_tr
par(mfrow = c(2,2))
hist(Treat_NFL, prob = TRUE, main = "Histogram: TreatNFL(Raw)", xlab = "TreatNFL(Raw)")
qqnorm(Treat_NFL, main = "QQ plot: TreatNFL")
hist(log_Tr_NFL, prob = TRUE, main = "Histogram: TreatNFL(Log)", xlab = "TreatNFL(Log)")
qqnorm(log_Tr_NFL, main = "QQ plot: TreatNFL(Log)")
hist(Control_NFL, prob = TRUE, main = "Histogram: ControlNFL(Raw)", xlab = "ControlNFL(Raw)")
qqnorm(Control_NFL, main = "QQ plot: ControlNFL(Raw)")
hist(log_Co_NFL, prob = TRUE, main = "Histogram: ControlNFL(Log)", xlab = "ControlNFL(Log)")
qqnorm(log_Co_NFL, main = "QQ plot: ControlNFL(Log)")
shapiro_Tr <- shapiro.test(Treat_NFL)
shapiro_logTr <- shapiro.test(log_Tr_NFL)
shapiro_Co <- shapiro.test(Control_NFL)
shapiro_logCo <- shapiro.test(log_Co_NFL)
shapiro_Tr
shapiro_logTr
shapiro_Co
shapiro_logCo
t_logNFL = t.test(log_Tr_NFL, log_Co_NFL, var.equal = FALSE)
t_logNFL
w_NFL = wilcox.test(Treat_NFL, Control_NFL, exact = FALSE)
w_NFL