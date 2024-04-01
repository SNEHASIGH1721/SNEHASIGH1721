
#THIS CODE FILE INVOVLES THE NON ADJUSTED, ADJUSTED WEIGTHS AND WITHOUT WEIGTHS ANALYSIS
getwd()
load("Datafiltered31UC.RData")
ls()
names(filtered_data31)
#final regression model as per author
filtered_data31$RA_status_binary <- ifelse(filtered_data31$RA_status == "RA", 1, 0)
install.packages("Publish")
library(Publish)
publish(model1)
install.packages("broom")
library(broom)
library(survey)
##log_DBUP
filtered_data31 <- filtered_data31 %>%
  mutate(DBUP_quartiled = cut(log_DBUP, 
                              breaks = quantile(log_DBUP, probs = 0:4/4, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = c("Q1", "Q2", "Q3", "Q4")))
model1<- glm(RA_status_binary ~ DBUP_quartiled + Gender + Age.cat, 
             data = filtered_data31, 
             family = "binomial")
publish(model1)
model1.1 <- glm(RA_status_binary ~ DBUP_quartiled + Ethnicity + Marital_status + 
                  Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                  Smoking + Activity_level + PIR, 
                data = filtered_data31, 
                family = "binomial")
publish(model1.1)
#
crude1_svy <- svyglm(RA_status_binary ~ DBUP_quartiled, 
                     design = svy_design, 
                     family = "binomial")
publish(crude1_svy)
model1_svy <- svyglm(RA_status_binary ~ DBUP_quartiled + Age.cat + Gender, 
                     design = svy_design, 
                     family = "binomial")
publish(model1_svy)
# Adjusting the second model similarly
model1.1_svy <- svyglm(RA_status_binary ~ DBUP_quartiled + Gender + Age + Ethnicity + Marital_status + 
                         Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                         Smoking + Activity_level + PIR, 
                       design = svy_design, 
                       family = "binomial")
publish(model1.1_svy)

##log_BDCPP
filtered_data31 <- filtered_data31 %>%
  mutate(BDCPP_quartiled = cut(log_BDCPP, 
                              breaks = quantile(log_BDCPP, probs = 0:4/4, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = c("Q1", "Q2", "Q3", "Q4")))
model2<- glm(RA_status_binary ~ BDCPP_quartiled + Age.cat + Gender, 
             data = filtered_data31, 
             family = "binomial")
publish(model2)
model2.1<- glm(RA_status_binary ~ BDCPP_quartiled + Gender + Age + Ethnicity + Marital_status + 
                 Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                 Smoking + Activity_level + PIR, 
               data = filtered_data31, 
               family = "binomial")
publish(model2.1)
#
crude2_svy <- svyglm(RA_status_binary ~ BDCPP_quartiled, 
                     design = svy_design, 
                     family = "binomial")
publish(crude2_svy)
model2_svy <- svyglm(RA_status_binary ~ BDCPP_quartiled + Age.cat + Gender, 
                     design = svy_design, 
                     family = "binomial")
publish(model2_svy)
# Adjusting the second model similarly
model2.1_svy <- svyglm(RA_status_binary ~ BDCPP_quartiled + Gender + Age + Ethnicity + Marital_status + 
                         Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                         Smoking + Activity_level + PIR, 
                       design = svy_design, 
                       family = "binomial")
publish(model2.1_svy)

##log_BCPP
filtered_data31 <- filtered_data31 %>%
  mutate(BCPP_quartiled = cut(log_BCPP, 
                               breaks = quantile(log_BCPP, probs = 0:4/4, na.rm = TRUE),
                               include.lowest = TRUE, 
                               labels = c("Q1", "Q2", "Q3", "Q4")))
model3<- glm(RA_status_binary ~ BCPP_quartiled + Age.cat + Gender, 
             data = filtered_data31, 
             family = "binomial")
publish(model3)

model3.1<- glm(RA_status_binary ~ BCPP_quartiled + Gender + Age + Ethnicity + Marital_status + 
                 Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                 Smoking + Activity_level + PIR, 
               data = filtered_data31, 
               family = "binomial")
publish(model3.1)
#
crude3_svy <- svyglm(RA_status_binary ~ BCPP_quartiled, 
                     design = svy_design, 
                     family = "binomial")
publish(crude3_svy)
model3_svy <- svyglm(RA_status_binary ~ BCPP_quartiled + Age.cat + Gender, 
                     design = svy_design, 
                     family = "binomial")
publish(model3_svy)
# Adjusting the second model similarly
model3.1_svy <- svyglm(RA_status_binary ~ BCPP_quartiled + Gender + Age + Ethnicity + Marital_status + 
                         Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                         Smoking + Activity_level + PIR, 
                       design = svy_design, 
                       family = "binomial")
publish(model3.1_svy)


##log_BCEP
filtered_data31 <- filtered_data31 %>%
  mutate(BCEP_quartiled = cut(log_BCEP, 
                               breaks = quantile(log_BCEP, probs = 0:4/4, na.rm = TRUE),
                               include.lowest = TRUE, 
                               labels = c("Q1", "Q2", "Q3", "Q4")))
model4<- glm(RA_status_binary ~ BCEP_quartiled + Age.cat + Gender, 
             data = filtered_data31, 
             family = "binomial")
publish(model4)

model4.1<- glm(RA_status_binary ~ BCEP_quartiled + Gender + Age + Ethnicity + Marital_status + 
                 Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                 Smoking + Activity_level + PIR, 
               data = filtered_data31, 
               family = "binomial")
publish(model4.1)
##
crude4_svy <- svyglm(RA_status_binary ~ BCEP_quartiled, 
                     design = svy_design, 
                     family = "binomial")
publish(crude4_svy)
model4_svy <- svyglm(RA_status_binary ~ BCEP_quartiled + Age.cat + Gender, 
                     design = svy_design, 
                     family = "binomial")
publish(model4_svy)
# Adjusting the second model similarly
model4.1_svy <- svyglm(RA_status_binary ~ BCEP_quartiled + Gender + Age + Ethnicity + Marital_status + 
                         Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                         Smoking + Activity_level + PIR, 
                       design = svy_design, 
                       family = "binomial")
publish(model4.1_svy)

###log_DPHP
filtered_data31 <- filtered_data31 %>%
  mutate(DPHP_quartiled = cut(log_DPHP, 
                               breaks = quantile(log_DPHP, probs = 0:4/4, na.rm = TRUE),
                               include.lowest = TRUE, 
                               labels = c("Q1", "Q2", "Q3", "Q4")))

model4<- glm(RA_status_binary ~ DPHP_quartiled + Age.cat + Gender, 
             data = filtered_data31, 
             family = "binomial")
publish(model4)

model4.1<- glm(RA_status_binary ~ DPHP_quartiled + Gender + Age + Ethnicity + Marital_status + 
                 Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                 Smoking + Activity_level + PIR, 
               data = filtered_data31, 
               family = "binomial")
publish(model4.1)



#Now Using the survey features 
svy_design <- svydesign(ids = ~PSU, 
                        strata = ~STRATA, 
                        weights = ~W.MEC, 
                        nest = TRUE, 
                        data=filtered_data31)
summary(weights(svy_design))




# Adjusting the model to use svyglm instead of glm for the first model
crude5_svy <- svyglm(RA_status_binary ~ DPHP_quartiled, 
                     design = svy_design, 
                     family = "binomial")
publish(crude5_svy)
model4_svy <- svyglm(RA_status_binary ~ DPHP_quartiled + Age.cat + Gender, 
                     design = svy_design, 
                     family = "binomial")
publish(model4_svy)
# Adjusting the second model similarly
model4.1_svy <- svyglm(RA_status_binary ~ DPHP_quartiled + Gender + Age + Ethnicity + Marital_status + 
                         Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                         Smoking + Activity_level + PIR, 
                       design = svy_design, 
                       family = "binomial")
publish(model4.1_svy)
####
#####








