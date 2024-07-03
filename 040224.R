#This code book have codes for the 
#LOD replacment by dividing it with square root.
#Calculating urinary metabolite concentration without unit conversion and with unit conversion for urinary metabolites


getwd()
load("Datafiltered31NEHA.RData")
ls()
summary(filtered_data31)
# 1 LOD value for DPHP
LOD_DPHP <- 0.1 # in micrograms per Liter

# Assuming 'DPHP_concentration' is the column with the DPHP measurements in micrograms per Liter
# Replace concentrations below the LOD with LOD divided by the square root of 2
filtered_data31$DPHP <- ifelse(
  filtered_data31$DPHP < LOD_DPHP,
  LOD_DPHP / sqrt(2),
  filtered_data31$DPHP
)
# Count the number of samples below the LOD value
number_below_LOD <- sum(filtered_data31$DPHP< LOD_DPHP)
# Calculate the percentage of samples below the LOD
percentage_below_LOD <- (number_below_LOD / nrow(filtered_data31)) * 100
# Output the percentage of samples below LOD
print(paste("Percentage of samples below LOD for DPHP:", round(percentage_below_LOD, 3), "%"))
# Filter out non-numeric and NA values for the DPHP variable
dphp_data <- na.omit(filtered_data31$DPHP)
dphp_data <- dphp_data[sapply(dphp_data, is.numeric)]

# Calculate the log-transformed values
log_dphp_data <- log2(dphp_data)

# Calculate the geometric mean on the log scale
gm_log <- mean(log_dphp_data)

# Calculate the standard error on the log scale
se_log <- sd(log_dphp_data) / sqrt(length(log_dphp_data))

# Construct 95% CI on the log scale
ci_log <- gm_log + c(-1, 1) * qnorm(0.975) * se_log

# Convert back to the original scale
gm <- 2^gm_log
ci <- 2^ci_log

# Output the results
cat("Geometric Mean for DPHP: ", gm, "\n",
    "95% CI for DPHP in ng/mL: [", ci[1], ", ", ci[2], "]", "\n")

# 2 LOD value for BDCPP
LOD_BDCPP <- 0.1 # in micrograms per Liter
filtered_data31$BDCPP <- ifelse(
  filtered_data31$BDCPP < LOD_BDCPP,
  LOD_BDCPP / sqrt(2),
  filtered_data31$BDCPP
)
number_below_LOD <- sum(filtered_data31$BDCPP< LOD_BDCPP)
percentage_below_LOD <- (number_below_LOD / nrow(filtered_data31)) * 100
print(paste("Percentage of samples below LOD for BDCPP:", round(percentage_below_LOD, 3), "%"))
# Filter out non-numeric and NA values for the BDCPP variable
bdcpp_data <- na.omit(filtered_data31$BDCPP)
bdcpp_data <- bdcpp_data[sapply(bdcpp_data, is.numeric)]

# Calculate the log-transformed values
log_bdcpp_data <- log2(bdcpp_data)

# Calculate the geometric mean on the log scale
gm_log <- mean(log_bdcpp_data)

# Calculate the standard error on the log scale
se_log <- sd(log_bdcpp_data) / sqrt(length(log_bdcpp_data))

# Construct 95% CI on the log scale
ci_log <- gm_log + c(-1, 1) * qnorm(0.975) * se_log

# Convert back to the original scale
gm <- 2^gm_log
ci <- 2^ci_log

# Output the results
cat("Geometric Mean for bdcpp: ", gm, "\n",
    "95% CI for bdcpp in ng/mL: [", ci[1], ", ", ci[2], "]", "\n")

# 3 LOD value for BCPP
LOD_BCPP <- 0.1 # in micrograms per Liter
filtered_data31$BCPP <- ifelse(
  filtered_data31$BCPP < LOD_BCPP,
  LOD_BCPP / sqrt(2),
  filtered_data31$BCPP
)
number_below_LOD <- sum(filtered_data31$BCPP< LOD_BCPP)
percentage_below_LOD <- (number_below_LOD / nrow(filtered_data31)) * 100
print(paste("Percentage of samples below LOD for BCPP:", round(percentage_below_LOD, 3), "%"))
# Filter out non-numeric and NA values for the BDCPP variable
bcpp_data <- na.omit(filtered_data31$BCPP)
bcpp_data <- bcpp_data[sapply(bcpp_data, is.numeric)]

# Calculate the log-transformed values
log_bcpp_data <- log2(bcpp_data)

# Calculate the geometric mean on the log scale
gm_log <- mean(log_bcpp_data)

# Calculate the standard error on the log scale
se_log <- sd(log_bcpp_data) / sqrt(length(log_bcpp_data))

# Construct 95% CI on the log scale
ci_log <- gm_log + c(-1, 1) * qnorm(0.975) * se_log

# Convert back to the original scale
gm <- 2^gm_log
ci <- 2^ci_log

# Output the results
cat("Geometric Mean for bcpp: ", gm, "\n",
    "95% CI for bcpp in ng/mL: [", ci[1], ", ", ci[2], "]", "\n")

# 4 LOD value for BCEP
LOD_BCEP <- 0.1 # in micrograms per Liter
filtered_data31$BCEP <- ifelse(
  filtered_data31$BCEP < LOD_BCEP,
  LOD_BCEP / sqrt(2),
  filtered_data31$BCEP
)

number_below_LOD <- sum(filtered_data31$BCEP< LOD_BCEP)
percentage_below_LOD <- (number_below_LOD / nrow(filtered_data31)) * 100
print(paste("Percentage of samples below LOD for BCEP:", round(percentage_below_LOD, 2), "%"))
# Filter out non-numeric and NA values for the BDCPP variable
bcep_data <- na.omit(filtered_data31$BCEP)
bcep_data <- bcep_data[sapply(bcep_data, is.numeric)]

# Calculate the log-transformed values
log_bcep_data <- log2(bcep_data)

# Calculate the geometric mean on the log scale
gm_log <- mean(log_bcep_data)

# Calculate the standard error on the log scale
se_log <- sd(log_bcep_data) / sqrt(length(log_bcep_data))

# Construct 95% CI on the log scale
ci_log <- gm_log + c(-1, 1) * qnorm(0.975) * se_log

# Convert back to the original scale
gm <- 2^gm_log
ci <- 2^ci_log

# Output the results
cat("Geometric Mean for bcep: ", gm, "\n",
    "95% CI for bcep in ng/mL: [", ci[1], ", ", ci[2], "]", "\n")


# 5 LOD value for DBUP
LOD_DBUP <- 0.1 # in micrograms per Liter
filtered_data31$DBUP <- ifelse(
  filtered_data31$DBUP < LOD_DBUP,
  LOD_DBUP / sqrt(2),
  filtered_data31$DBUP
)
number_below_LOD <- sum(filtered_data31$DBUP< LOD_DBUP)
percentage_below_LOD <- (number_below_LOD / nrow(filtered_data31)) * 100
print(paste("Percentage of samples below LOD for DBUP:", round(percentage_below_LOD, 3), "%"))
# Filter out non-numeric and NA values for the BDCPP variable
dbup_data <- na.omit(filtered_data31$DBUP)
dbup_data <- dbup_data[sapply(dbup_data, is.numeric)]

# Calculate the log-transformed values
log_dbup_data <- log2(dbup_data)

# Calculate the geometric mean on the log scale
gm_log <- mean(log_dbup_data)

# Calculate the standard error on the log scale
se_log <- sd(log_dbup_data) / sqrt(length(log_dbup_data))

# Construct 95% CI on the log scale
ci_log <- gm_log + c(-1, 1) * qnorm(0.975) * se_log

# Convert back to the original scale
gm <- 2^gm_log
ci <- 2^ci_log

# Output the results
cat("Geometric Mean for dbup: ", gm, "\n",
    "95% CI for dbup in ng/mL: [", ci[1], ", ", ci[2], "]", "\n")

# 6 LOD value for TBBA<-0.05
LOD_TBBA <- 0.05 # in micrograms per Liter
filtered_data31$TBBA <- ifelse(
  filtered_data31$TBBA < LOD_TBBA,
  LOD_TBBA / sqrt(2),
  filtered_data31$TBBA
)
number_below_LOD <- sum(filtered_data31$TBBA< LOD_TBBA)
percentage_below_LOD <- (number_below_LOD / nrow(filtered_data31)) * 100
print(paste("Percentage of samples below LOD for TBBA:", round(percentage_below_LOD, 3), "%"))
#sINCE THE DETECTION FREQUNCY OF TBBA IS 94.13% WE WILL DROP IT
filtered_data31$TBBA<- NULL
####
#After the limit of detection treatment lets look at the summary of the urinanry metabolites 
summary(filtered_data31$DPHP)
summary(filtered_data31$BDCPP)
summary(filtered_data31$BCPP)
summary(filtered_data31$BCEP)
summary(filtered_data31$DBUP)
###
###
#After the LOD treatment lets us do the missing value treatment
library(DataExplorer)
plot_missing(filtered_data31)
## missing value treatment for continuous variables
filtered_data31$BMI[is.na(filtered_data31$BMI)] <- median(filtered_data31$BMI, na.rm = TRUE)
filtered_data31$Urinary_creatinine[is.na(filtered_data31$Urinary_creatinine)] <- median(filtered_data31$Urinary_creatinine, na.rm = TRUE)
###
table(filtered_data31$PIR, useNA = "always")
filtered_data31$PIR <- as.character(filtered_data31$PIR)
filtered_data31$PIR[is.na(filtered_data31$PIR)] <- "Missing"
filtered_data31$PIR <- as.factor(filtered_data31$PIR)
##

##
table(filtered_data31$Alcohol, useNA = "always")
filtered_data31$Alcohol <- as.character(filtered_data31$Alcohol)
filtered_data31$Alcohol[is.na(filtered_data31$Alcohol)] <- "Missing"
filtered_data31$Alcohol <- as.factor(filtered_data31$Alcohol)
##
table(filtered_data31$Smoking, useNA = "always")
filtered_data31$Smoking <- as.character(filtered_data31$Smoking)
filtered_data31$Smoking[is.na(filtered_data31$Smoking)] <- "Missing"
filtered_data31$Smoking <- as.factor(filtered_data31$Smoking)

##
table(filtered_data31$Marital_status, useNA = "always")
filtered_data31$Marital_status <- as.character(filtered_data31$Marital_status)
filtered_data31$Marital_status[is.na(filtered_data31$Marital_status)] <- "Missing"
filtered_data31$Marital_status <- as.factor(filtered_data31$Marital_status)
##
table(filtered_data31$Citizenship, useNA = "always")
filtered_data31$Citizenship <- as.character(filtered_data31$Citizenship)
filtered_data31$Citizenship[is.na(filtered_data31$Citizenship)] <- "Missing"
filtered_data31$Citizenship <- as.factor(filtered_data31$Citizenship)
##
table(filtered_data31$Diabetes, useNA = "always")
filtered_data31$Diabetes <- as.character(filtered_data31$Diabetes)
filtered_data31$Diabetes[is.na(filtered_data31$Diabetes)] <- "Missing"
filtered_data31$Diabetes <- as.factor(filtered_data31$Diabetes)
##
table(filtered_data31$Education, useNA = "always")
filtered_data31$Education <- as.character(filtered_data31$Education)
filtered_data31$Education[is.na(filtered_data31$Education)] <- "Missing"
filtered_data31$Education <- as.factor(filtered_data31$Education)
plot_missing(filtered_data31)
summary(filtered_data31)
###
#first treatment where we will account for dilution without any unit conversion 
#1:DPHP
#  'Urinary_Creatinine' is in mg/dL, 
summary(filtered_data31$Urinary_creatinine)
summary(filtered_data31)
#Now we will try two things one accounting for urinanry dilution without conversion 
filtered_data31$DPHP_Corrected<- filtered_data31$DPHP/ filtered_data31$Urinary_creatinine
summary(filtered_data31$DPHP_Corrected)
##
filtered_data31$DBUP_Corrected<- filtered_data31$DBUP/ filtered_data31$Urinary_creatinine
summary(filtered_data31$DBUP_Corrected)
##
filtered_data31$BDCPP_Corrected<- filtered_data31$BDCPP/ filtered_data31$Urinary_creatinine
summary(filtered_data31$BDCPP_Corrected)
##
filtered_data31$BCEP_Corrected<- filtered_data31$BCEP/ filtered_data31$Urinary_creatinine
summary(filtered_data31$BCEP_Corrected)
##
filtered_data31$BCPP_Corrected <- filtered_data31$BCPP/ filtered_data31$Urinary_creatinine
summary(filtered_data31$BCPP_Corrected)


# Assuming filtered_data31 is your dataframe and it now includes a 'DPHP_Corrected' column



#second treatment where we will account for dilution without any unit conversion




# converting mg/dL to g/L
filtered_data31$Urinary_Creatinine_ug_L <- filtered_data31$Urinary_creatinine * 0.01
summary(filtered_data31$Urinary_Creatinine_ug_L)
# Calculate the DPHP concentration normalized to creatinine levels (Î¼g/g creatinine)
filtered_data31$DPHP_Corrected<- filtered_data31$DPHP/ filtered_data31$Urinary_Creatinine_ug_L
summary(filtered_data31$DPHP_Corrected)
##
filtered_data31$DBUP_Corrected<- filtered_data31$DBUP/ filtered_data31$Urinary_Creatinine_ug_L
summary(filtered_data31$DBUP_Corrected)
##
filtered_data31$BDCPP_Corrected<- filtered_data31$BDCPP/ filtered_data31$Urinary_Creatinine_ug_L
summary(filtered_data31$BDCPP_Corrected)
##
filtered_data31$BCEP_Corrected<- filtered_data31$BCEP/ filtered_data31$Urinary_Creatinine_ug_L
summary(filtered_data31$BCEP_Corrected)
##
filtered_data31$BCPP_Corrected <- filtered_data31$BCPP/ filtered_data31$Urinary_Creatinine_ug_L
summary(filtered_data31$BCPP_Corrected)
### 




























































































# Load necessary library
#install.packages("psych")
library(psych)
# Assuming 'DPHP_concentration' is the column with the DPHP measurements
# Filter out non-positive values
DPHP<- filtered_data31$DPHP[filtered_data31$DPHP > 0]

# Calculate log-transformed values
log_DPHP_values <- log(DPHP)

# Calculate the arithmetic mean and standard error of the log-transformed values
mean_log_DPHP <- mean(log_DPHP_values)
se_log_DPHP <- sd(log_DPHP_values) / sqrt(length(log_DPHP_values))

# Calculate the 95% CI for the log-transformed values
z <- qnorm(0.975) # z-score for 95% CI
ci_log_low <- mean_log_DPHP - z * se_log_DPHP
ci_log_high <- mean_log_DPHP + z * se_log_DPHP

# Back-transform to get the geometric mean and its 95% CI
geometric_mean_DPHP <- exp(mean_log_DPHP)
ci_low_DPHP <- exp(ci_log_low)
ci_high_DPHP <- exp(ci_log_high)

# Output the geometric mean with 95% CI
print(paste("Geometric mean for DPHP:", geometric_mean_DPHP, "μg/L"))
print(paste("95% CI for DPHP: [", ci_low_DPHP, ";", ci_high_DPHP, "] μg/L"))

# Calculate and output min, max, and percentiles (25%, 50%, 75%)
min_DPHP <- min(DPHP)
max_DPHP <- max(DPHP)
percentiles_DPHP <- quantile(DPHP, probs = c(0.25, 0.5, 0.75))

print(paste("Minimum for DPHP:", min_DPHP, "μg/L"))
print(paste("Maximum for DPHP:", max_DPHP, "μg/L"))
print(paste("25th percentile for DPHP:", percentiles_DPHP[1], "μg/L"))
print(paste("Median for DPHP:", percentiles_DPHP[2], "μg/L"))
print(paste("75th percentile for DPHP:", percentiles_DPHP[3], "μg/L"))
##
#2DBUP 
DBUP<- filtered_data31$DBUP[filtered_data31$DBUP > 0]

# Calculate log-transformed values
log_DBUP_values <- log(DBUP)

# Calculate the arithmetic mean and standard error of the log-transformed values
mean_log_DBUP <- mean(log_DBUP_values)
se_log_DBUP <- sd(log_DBUP_values) / sqrt(length(log_DBUP_values))

# Calculate the 95% CI for the log-transformed values
z <- qnorm(0.975) # z-score for 95% CI
ci_log_low <- mean_log_DBUP - z * se_log_DBUP
ci_log_high <- mean_log_DBUP + z * se_log_DBUP

# Back-transform to get the geometric mean and its 95% CI
geometric_mean_DBUP <- exp(mean_log_DBUP)
ci_low_DBUP <- exp(ci_log_low)
ci_high_DBUP <- exp(ci_log_high)

# Output the geometric mean with 95% CI
print(paste("Geometric mean for DBUP:", geometric_mean_DBUP, "μg/L"))
print(paste("95% CI for DBUP: [", ci_low_DBUP, ";", ci_high_DBUP, "] μg/L"))

# Calculate and output min, max, and percentiles (25%, 50%, 75%)
min_DBUP <- min(DBUP)
max_DBUP <- max(DBUP)
percentiles_DBUP <- quantile(DBUP, probs = c(0.25, 0.5, 0.75))

print(paste("Minimum for DPHP:", min_DBUP, "μg/L"))
print(paste("Maximum for DPHP:", max_DBUP, "μg/L"))
print(paste("25th percentile for DBUP:", percentiles_DBUP[1], "μg/L"))
print(paste("Median for DPHP:", percentiles_DBUP[2], "μg/L"))
print(paste("75th percentile for DPHP:", percentiles_DBUP[3], "μg/L"))
##
#3BDCPP 
BDCPP<- filtered_data31$BDCPP[filtered_data31$BDCPP > 0]

# Calculate log-transformed values
log_BDCPP_values <- log(BDCPP)

# Calculate the arithmetic mean and standard error of the log-transformed values
mean_log_BDCPP <- mean(log_BDCPP_values)
se_log_BDCPP<- sd(log_BDCPP_values) / sqrt(length(log_BDCPP_values))

# Calculate the 95% CI for the log-transformed values
z <- qnorm(0.975) # z-score for 95% CI
ci_log_low <- mean_log_BDCPP - z * se_log_BDCPP
ci_log_high <- mean_log_BDCPP + z * se_log_BDCPP

# Back-transform to get the geometric mean and its 95% CI
geometric_mean_BDCPP <- exp(mean_log_BDCPP)
ci_low_BDCPP <- exp(ci_log_low)
ci_high_BDCPP <- exp(ci_log_high)

# Output the geometric mean with 95% CI
print(paste("Geometric mean for BDCPP:", geometric_mean_BDCPP, "μg/L"))
print(paste("95% CI for BDCPP: [", ci_low_BDCPP, ";", ci_high_BDCPP, "] μg/L"))

# Calculate and output min, max, and percentiles (25%, 50%, 75%)
min_BDCPP <- min(BDCPP)
max_BDCPP<- max(BDCPP)
percentiles_BDCPP <- quantile(BDCPP, probs = c(0.25, 0.5, 0.75))

print(paste("Minimum for BDCPP:", min_BDCPP, "μg/L"))
print(paste("Maximum for BDCPP:", max_BDCPP, "μg/L"))
print(paste("25th percentile for BDCPP:", percentiles_BDCPP[1], "μg/L"))
print(paste("Median for BDCPP:", percentiles_BDCPP[2], "μg/L"))
print(paste("75th percentile for BDCPP:", percentiles_BDCPP[3], "μg/L"))
#4BCPP 
BCPP<- filtered_data31$BCPP[filtered_data31$BCPP > 0]

# Calculate log-transformed values
log_BCPP_values <- log(BCPP)

# Calculate the arithmetic mean and standard error of the log-transformed values
mean_log_BCPP <- mean(log_BCPP_values)
se_log_BCPP<- sd(log_BCPP_values) / sqrt(length(log_BCPP_values))

# Calculate the 95% CI for the log-transformed values
z <- qnorm(0.975) # z-score for 95% CI
ci_log_low <- mean_log_BCPP - z * se_log_BCPP
ci_log_high <- mean_log_BCPP + z * se_log_BCPP

# Back-transform to get the geometric mean and its 95% CI
geometric_mean_BCPP <- exp(mean_log_BCPP)
ci_low_BCPP <- exp(ci_log_low)
ci_high_BCPP <- exp(ci_log_high)

# Output the geometric mean with 95% CI
print(paste("Geometric mean for BCPP:", geometric_mean_BCPP, "μg/L"))
print(paste("95% CI for BCPP: [", ci_low_BCPP, ";", ci_high_BCPP, "] μg/L"))

# Calculate and output min, max, and percentiles (25%, 50%, 75%)
min_BCPP <- min(BCPP)
max_BCPP<- max(BCPP)
percentiles_BCPP <- quantile(BCPP, probs = c(0.25, 0.5, 0.75))

print(paste("Minimum for BCPP:", min_BCPP, "μg/L"))
print(paste("Maximum for BCPP:", max_BCPP, "μg/L"))
print(paste("25th percentile for BCPP:", percentiles_BCPP[1], "μg/L"))
print(paste("Median for BCPP:", percentiles_BCPP[2], "μg/L"))
print(paste("75th percentile for BCPP:", percentiles_BCPP[3], "μg/L"))
#5BCEP 
BCEP<- filtered_data31$BCEP[filtered_data31$BCEP > 0]

# Calculate log-transformed values
log_BCEP_values <- log(BCEP)

# Calculate the arithmetic mean and standard error of the log-transformed values
mean_log_BCEP <- mean(log_BCEP_values)
se_log_BCEP<- sd(log_BCEP_values) / sqrt(length(log_BCEP_values))

# Calculate the 95% CI for the log-transformed values
z <- qnorm(0.975) # z-score for 95% CI
ci_log_low <- mean_log_BCEP - z * se_log_BCEP
ci_log_high <- mean_log_BCEP + z * se_log_BCEP

# Back-transform to get the geometric mean and its 95% CI
geometric_mean_BCEP <- exp(mean_log_BCEP)
ci_low_BCEP <- exp(ci_log_low)
ci_high_BCEP <- exp(ci_log_high)

# Output the geometric mean with 95% CI
print(paste("Geometric mean for BCEP:", geometric_mean_BCEP, "μg/L"))
print(paste("95% CI for BCEP: [", ci_low_BCEP, ";", ci_high_BCEP, "] μg/L"))

# Calculate and output min, max, and percentiles (25%, 50%, 75%)
min_BCEP <- min(BCEP)
max_BCEP<- max(BCEP)
percentiles_BCEP <- quantile(BCEP, probs = c(0.25, 0.5, 0.75))

print(paste("Minimum for BCEP:", min_BCEP, "μg/L"))
print(paste("Maximum for BCEP:", max_BCEP, "μg/L"))
print(paste("25th percentile for BCEP:", percentiles_BCEP[1], "μg/L"))
print(paste("Median for BCEP:", percentiles_BCEP[2], "μg/L"))
print(paste("75th percentile for BCEP:", percentiles_BCEP[3], "μg/L"))
save(filtered_data31, file="Datafiltered31NEHA.RData")

