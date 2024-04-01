getwd()
load("Datafiltered31UC.RData")
ls()

svy_design <- svydesign(ids = ~PSU, 
                        strata = ~STRATA, 
                        weights = ~W.MEC, 
                        nest = TRUE, 
                        data=filtered_data31)
summary(weights(svy_design))
#please note we are using w.MEC weight because we are using the lab components in the analysis
library(survey)
library(tableone)
filtered_data31$RA_status_binary <- ifelse(filtered_data31$RA_status == "RA", 1, 0)
vars2=c("Gender", "Age.cat", "Ethnicity", "Marital_status", "Education","Citizenship", "Alcohol", "Diabetes","Smoking", "Activity_level", "PIR")

kableone<- function(x, ...) {
  capture.output(x<- print(x, showALLLevels=TRUE, padColnames = TRUE, insertLevel = TRUE))
  knitr::kable(x, ...)
}
kableone(svyCreateCatTable(var=vars2, strata = "RA_status_binary", data=svy_design, test= TRUE))
#crude model and fully adjusted model
install.packages("jtools")
library(jtools)
install.packages("ggstance")
library(ggstance)
install.packages("broom.mixed")
library(broom.mixed)
install.packages("huxtable")
library(huxtable)
### Mann-Whitney U tests to compare the differences of continuous variables. 
#The null hypothesis: The median difference between pairs of observation is 0
#The alternative hypothesis is the median difference between pairs of observation is not zero
#binning the continous variables into quatitles
# Assuming analytic17 is your dataframe and 'exposure' is the variable you want to quarter

# Load necessary library
library(dplyr)
# Change the urinanry metabolites into quantiltes 
#1 DBUP
# Divide the exposure variable into quartiles
library(dplyr)

filtered_data31 <- filtered_data31 %>%
  mutate(DBUP_quartiled = cut(log_DBUP, 
                              breaks = quantile(log_DBUP, probs = 0:4/4, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = c("Q1", "Q2", "Q3", "Q4")))

filtered_data31 <- filtered_data31 %>%
  mutate(DBUP_quartiled = cut(log_DBUP, 
                                  breaks = quantile(log_DBUP, probs = 0:4/4, na.rm = TRUE),
                                  include.lowest = TRUE, 
                                  labels = FALSE))
#2
filtered_data31 <- filtered_data31 %>%
  mutate(BDCPP_quartiled = cut(log_BDCPP, 
                              breaks = quantile(log_BDCPP, probs = 0:4/4, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = FALSE))
#3
filtered_data31 <- filtered_data31 %>%
  mutate(BCPP_quartiled = cut(log_BCPP, 
                               breaks = quantile(log_BCPP, probs = 0:4/4, na.rm = TRUE),
                               include.lowest = TRUE, 
                               labels = FALSE))
#4
filtered_data31 <- filtered_data31 %>%
  mutate(BCEP_quartiled = cut(log_BCEP, 
                              breaks = quantile(log_BCEP, probs = 0:4/4, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = FALSE))
#5
filtered_data31 <- filtered_data31 %>%
  mutate(DPHP_quartiled = cut(log_DPHP, 
                              breaks = quantile(log_DPHP, probs = 0:4/4, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = FALSE))
# Fit a logistic regression model using the quartiled exposure variable
# Assuming 'outcome' is your binary dependent variable
model <- glm(RA_status_binary ~ DPHP_quartiled , data = filtered_data31, family = binomial())

# Print the model summary to check the results
summary(model)
library(Publish)
names(filtered_data31)
summary(filtered_data31)
#Model 1 for DPHP with Urinary creatinine

formula1<- as.formula(RA_status_binary ~ DPHP_quartiled+Urinary_Creatinine_ug_L)
fit<-svyglm(formula1,
            design = svy_design,
            family = binomial(link="logit"))
publish(fit)
summary(fit)
coef(fit)

formula2<- as.formula(I(RA_status_binary)~DBUP_quartiled)
fit2<-svyglm(formula2,
            design = svy_design,
            family = binomial(link="logit"))
publish(fit2)
sum
formula3<- as.formula(I(RA_status_binary)~BDCPP_quartiled)
fit3<-svyglm(formula3,
             design = svy_design,
             family = binomial(link="logit"))
publish(fit3)
formula4<- as.formula(I(RA_status_binary)~BCPP_quartiled)
fit4<-svyglm(formula4,
             design = svy_design,
             family = binomial(link="logit"))
publish(fit4)
formula5<- as.formula(I(RA_status_binary)~BCEP_quartiled)
fit5<-svyglm(formula5,
             design = svy_design,
             family = binomial(link="logit"))
publish(fit5)


