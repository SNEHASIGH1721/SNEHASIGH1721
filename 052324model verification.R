# This code book contains codes for 
#1 Converting the creatinine corrected in microgram/microgram and log transformed into quarted 
#2 Binomial logistic regression using survey features 
# MODEL 1: Adjusted for Urinary Creatinine in mg/dL 
# Model 2: Adujsted for "Gender" "Age" "Ethnicity" "Marital_status" "Education" "Citizenship" 
#"Urinary_creatinine" "Alcohol" "BMI" "Diabetes" "Smoking" "Age.cat" "Activity_level" "PIR"        

getwd()
load("Datafiltered31NEHA.RData")
ls()
names(filtered_data31)
filtered_data31$DPHP_cr<- NULL
filtered_data31$BDCPP_cr<- NULL
filtered_data31$BCPP_cr<- NULL
filtered_data31$BCEP_cr<- NULL
filtered_data31$DBUP_cr<- NULL
filtered_data31$logDPHP<- NULL
filtered_data31$logBDCPP<- NULL
filtered_data31$logBCPP<- NULL
filtered_data31$logBCEP<- NULL
filtered_data31$logDBUP<- NULL
filtered_data31$DPHP<- NULL
filtered_data31$BDCPP<- NULL
filtered_data31$BCPP<- NULL
filtered_data31$BCEP<- NULL
filtered_data31$DBUP<- NULL
filtered_data31$Urinary_creatinine_log <- log(filtered_data31$Urinary_creatinine)
# Convert RA_status to binary factor variable
filtered_data31$RA_status_binary <- ifelse(filtered_data31$RA_status == "RA", 1, 0)
table(filtered_data31$RA_status_binary)
# Verify the conversion
table(filtered_data31$RA_status_binary)

filtered_data31$RA_status<-NULL
filtered_data31$Urinary_creatinine<-NULL
names(filtered_data31)
# Assuming 'filtered_data31' is your dataframe
require(caret)
require(tableone)
#preproc <- preProcess(filtered_data31[, c("DPHP_cr_ln", "BCPP_cr_ln", "BCEP_cr_ln","DBUP_cr_ln","BMI","Age")], method=c("center", "scale"))
#filtered_data31 <- predict(preproc, filtered_data31)

# converting the metabolites into the quarters
require(dplyr)
#1 DPHP_cr_In
filtered_data31 <- filtered_data31 %>%
  mutate(DPHP_quantiled = cut(DPHP_cr_ln, 
                              breaks = quantile(DPHP_cr_ln, probs = 0:4/4, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = c("Q1", "Q2", "Q3", "Q4")))
#2 BDCPP_cr_ln
filtered_data31 <- filtered_data31 %>%
  mutate(BDCPP_quantiled = cut(BDCPP_cr_ln, 
                               breaks = quantile(BDCPP_cr_ln, probs = 0:4/4, na.rm = TRUE),
                               include.lowest = TRUE, 
                               labels = c("Q1", "Q2", "Q3", "Q4")))
#3 BCPP_cr_ln
filtered_data31 <- filtered_data31 %>%
  mutate(BCPP_quantiled = cut(BCPP_cr_ln, 
                              breaks = quantile(BCPP_cr_ln, probs = 0:4/4, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = c("Q1", "Q2", "Q3", "Q4")))
#4 BCEP_cr_ln
filtered_data31 <- filtered_data31 %>%
  mutate(BCEP_quantiled = cut(BCEP_cr_ln, 
                              breaks = quantile(BCEP_cr_ln, probs = 0:4/4, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = c("Q1", "Q2", "Q3", "Q4")))
#5 DBUP_cr_ln
filtered_data31 <- filtered_data31 %>%
  mutate(DBUP_quantiled = cut(DBUP_cr_ln, 
                              breaks = quantile(DBUP_cr_ln, probs = 0:4/4, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = c("Q1", "Q2", "Q3", "Q4")))
str(filtered_data31)
filtered_data31$ID<-NULL
#converting the urinary_creatinine to log as it is highly right skewed.
#filtered_data31$Urinary_creatinine_log <- log(filtered_data31$Urinary_creatinine)
#filtered_data31$RA_status<-NULL
#filtered_data31$Urinary_creatinine<-NULL
require(survey)
svy_design <- svydesign(ids = ~PSU, 
                        strata = ~STRATA, 
                        weights = ~W.MEC, 
                        nest = TRUE, 
                        data=filtered_data31)
summary(weights(svy_design))
#please note we are using w.MEC weight because we are using the lab components in the analysis

#crude model and fully adjusted model
require(jtools)
require(ggstance)
require(broom.mixed)
require(huxtable)
require(Publish)
require(dplyr)
require(survey)
require(car)#for VIF
names(filtered_data31)
summary(filtered_data31)
str(filtered_data31)
#testing the associations
chisq.gender<-chisq.test(filtered_data31$RA_status_binary, filtered_data31$Gender)
chisq.gender
chisq.age<-chisq.test(filtered_data31$RA_status_binary, filtered_data31$Age.cat)
chisq.age
chisq.eth<-chisq.test(filtered_data31$RA_status_binary, filtered_data31$Ethnicity)
chisq.eth
##testing association with survey features
svychisq(~RA_status_binary+Gender, design = svy_design, statistic = "Chisq")
svychisq(~RA_status_binary+Age.cat, design = svy_design, statistic = "Chisq")
svychisq(~RA_status_binary+Marital_status, design = svy_design, statistic = "Chisq")

svychisq(~RA_status_binary+DPHP_cr_ln, design = svy_design, statistic = "Chisq")
svychisq(~RA_status_binary+BDCPP_cr_ln, design = svy_design, statistic = "Chisq")
svychisq(~RA_status_binary+BCPP_cr_ln, design = svy_design, statistic = "Chisq")
svychisq(~RA_status_binary+BCEP_cr_ln, design = svy_design, statistic = "Chisq")
svychisq(~RA_status_binary+DBUP_cr_ln, design = svy_design, statistic = "Chisq")

vars2=c("Gender","Ethnicity","Marital_status","Education",
       "Citizenship","Alcohol","Diabetes","Smoking","Age.cat","Activity_level","PIR","BMI","DPHP_cr_ln","BDCPP_cr_ln","BCPP_cr_ln","BCEP_cr_ln","DBUP_cr_ln")
kableone<-function(x, ...){
  capture.output(x <-print(x,showAllLevels=TRUE, padColnames = TRUE, insertLevel = TRUE))
  knitr::kable(x,...)
}
Finalmanu<- kableone(svyCreateTableOne(var=vars2, strata="RA_status", data= svy_design, test=TRUE))
Finalmanu
##
Finalgender<- kableone(svyCreateTableOne(var=vars2, strata="Gender", data= svy_design, test=TRUE))
Finalgender
##
Finalage<- kableone(svyCreateTableOne(var=vars2, strata="Age.cat", data= svy_design, test=TRUE))
Finalage
#### Now running the multivariate logistic regression
formula1<- as.formula(I(RA_status_binary)~DPHP_quantiled+Gender+Age.cat)
fit1 <- svyglm(formula1,
               design = svy_design,
               family = binomial(link = "logit"),
               control = glm.control(maxit = 100, epsilon = 1e-8))

# Check the output again
summary(fit1)
publish(fit1)
vif(fit1)
regTermTest(fit1,~BMI)
####
# Binomial Logistic Regression Model 2
formula1.1<- as.formula(I(RA_status_binary)~DPHP_quantiled+Age.cat+Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit1.1<-svyglm(formula1.1,
               design = svy_design,
               family = binomial(link="logit"),
               control = glm.control(maxit = 100, epsilon = 1e-8))
publish(fit1.1)
summary(fit1.1)
vif(fit1.1)
export_summs(fit1,fit1.1)
plot_summs(fit1,fit1.1)

