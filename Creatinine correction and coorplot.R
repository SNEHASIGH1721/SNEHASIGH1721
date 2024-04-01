getwd()
load("Datafiltered31NEHA.RData")
ls()
summary(filtered_data31$DPHP)
summary(filtered_data31$BDCPP)
summary(filtered_data31$BCPP)
summary(filtered_data31$BCEP)
summary(filtered_data31$DBUP)
filtered_data31$TBBA<- NULL    

###
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
# Now we will cacluate creatnine 
#1:DPHP
# Assuming 'Urinary_Creatinine' is in mg/dL, convert it to g/L for consistency
summary(filtered_data31$Urinary_creatinine)
summary(filtered_data31)
# converting mg/dL to ug/L
filtered_data31$Urinary_Creatinine_ug_L <- filtered_data31$Urinary_creatinine * 10000
summary(filtered_data31$Urinary_Creatinine_ug_L)
#filtered_data31$Urinary_Creatinine_g_L <- filtered_data31$Urinary_creatinine / 100
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
#filtered_data31$DPHP_ug_g <- (filtered_data31$DPHP/ filtered_data31$Urinary_Creatinine_g_L) * 1000 # Multiply by 1000 to convert from Î¼g/L to Î¼g/g
#2:BDCPP
#filtered_data31$BDCPP_ug_g <- (filtered_data31$BDCPP/ filtered_data31$Urinary_Creatinine_g_L) * 1000 # Multiply by 1000 to convert from Î¼g/L to Î¼g/g
#3BCPP

#filtered_data31$BCPP_ug_g <- (filtered_data31$BCPP/ filtered_data31$Urinary_Creatinine_g_L) * 1000 # Multiply by 1000 to convert from Î¼g/L to Î¼g/g
#4BCEP

#filtered_data31$BCEP_ug_g <- (filtered_data31$BCEP/ filtered_data31$Urinary_Creatinine_g_L) * 1000 
#5DBUP
#
#filtered_data31$DBUP_ug_g <- (filtered_data31$DBUP/ filtered_data31$Urinary_Creatinine_g_L) * 1000 
#6
summary(filtered_data31$DPHP)
summary(filtered_data31$BDCPP)
summary(filtered_data31$BCPP)
summary(filtered_data31$BCEP)
summary(filtered_data31$DBUP)
####
summary(filtered_data31$DPHP_Corrected)
summary(filtered_data31$BDCPP_Corrected)
summary(filtered_data31$BCPP_Corrected)
summary(filtered_data31$BCEP_Corrected)
summary(filtered_data31$DBUP_Corrected)
library(dplyr)



# Select only the relevant metabolite columns
metabolites <- filtered_data31[c("DPHP", "BDCPP", "BCPP", "BCEP", "DBUP")]


library(corrplot)

# Assuming your correlation matrix is named 'spearman_correlation'
spearman_correlation <- cor(metabolites, method = "spearman", use = "complete.obs")

# Create the correlation plot
corrplot(spearman_correlation, method="color", type="upper", 
         order="hclust", addCoef.col = "black", 
         tl.col="black", tl.srt=45, 
         diag=FALSE)
title(main="Spearman correlation among urinary metabolites of flame retardants", 
      col.main="darkblue",  # This changes the color of the title to blue
      font.main=2)
###
# Select only the relevant metabolite columns
metabolites1 <- filtered_data31[c("DPHP_Corrected", "BDCPP_Corrected", "BCPP_Corrected", "BCEP_Corrected", "DBUP_Corrected")]

# Calculate Spearman correlation
spearman_correlation2 <- cor(metabolites1, method = "spearman", use = "complete.obs")
corrplot(spearman_correlation2, method="color", type="upper", 
         order="hclust", addCoef.col = "black", 
         tl.col="black", tl.srt=45, 
         diag=FALSE)
title(main="Spearman correlation among  creatinine corrected metabolites of flame retardants", 
      col.main="darkblue",  # This changes the color of the title to blue
      font.main=2)
save(filtered_data31, file="Datafiltered31UC.RData")
