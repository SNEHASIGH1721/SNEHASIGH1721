# This code book contains codes for 
#1 Converting the creatinine corrected in microgram/microgram and log transformed into quarted 
#2 Binomial logistic regression using survey features 
# MODEL 1: Adjusted for Urinary Creatinine in mg/dL 
# Model 2: Adujsted for "Gender" "Age" "Ethnicity" "Marital_status" "Education" "Citizenship" 
#"Urinary_creatinine" "Alcohol" "BMI" "Diabetes" "Smoking" "Age.cat" "Activity_level" "PIR"        

getwd()
load("Datafiltered31NEHA.RData")
ls()
#install.packages("fastDummies")
library(fastDummies)
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
###
names(filtered_data31)
filtered_data31$Urinary_creatinine_log <- log(filtered_data31$Urinary_creatinine)
str(filtered_data31)
# Convert RA_status to binary factor variable
filtered_data31$RA_status_binary <- ifelse(filtered_data31$RA_status == "RA", 1, 0)
table(filtered_data31$RA_status)
#str(filtered_data31$RA_status_binary)
filtered_data31$RA_status<-as.factor(filtered_data31$RA_status)
# Verify the conversion
table(filtered_data31$RA_status_binary)

filtered_data31$RA_status<-NULL
filtered_data31$Urinary_creatinine<-NULL
names(filtered_data31)
# Assuming 'filtered_data31' is your dataframe
library(caret)
#preproc <- preProcess(filtered_data31[, c("DPHP_cr_ln", "BCPP_cr_ln", "BCEP_cr_ln","DBUP_cr_ln","BMI","Age")], method=c("center", "scale"))
#filtered_data31 <- predict(preproc, filtered_data31)

# converting the metabolites into the quarters
library(dplyr)
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
library(rms)
names(filtered_data31)
describe(filtered_data31)
###lets us quickly check for collinear predictors
library(Hmisc)
sel.names<-c("Gender","Ethnicity","Marital_status","Education","Citizenship","Urinary_creatinine","Alcohol","BMI","Diabetes",
             "Smoking","Age.cat","Activity_level","PIR")
var.cluster<-varclus(~., data=filtered_data31[sel.names])
plot(var.cluster)
#exploring relartionship between continous variables 
library(corrplot)
numeric.names<-c("Urinary_creatinine_log","DPHP_cr_ln","BDCPP_cr_ln","BCPP_cr_ln","BCEP_cr_ln","DBUP_cr_ln","BMI")
correlationMatrix<- cor(filtered_data31[numeric.names])
mat.num<-round(correlationMatrix,2)
mat.num[mat.num>0.8 & mat.num < 1]
corrplot(correlationMatrix, method="number", type="upper")
#Examining the descriptive association with the dependent variable
mean(filtered_data31$BMI[filtered_data31$Gender=="Male"])
mean(filtered_data31$BMI[filtered_data31$Gender=="Female"])
mean(filtered_data31$DPHP_cr_ln[filtered_data31$Gender=="Male"])
mean(filtered_data31$DPHP_cr_ln[filtered_data31$Gender=="Female"])
mean(filtered_data31$BDCPP_cr_ln[filtered_data31$Gender=="Male"])
mean(filtered_data31$BDCPP_cr_ln[filtered_data31$Gender=="Female"])
mean(filtered_data31$BCPP_cr_ln[filtered_data31$Gender=="Male"])
mean(filtered_data31$BCPP_cr_ln[filtered_data31$Gender=="Female"])
mean(filtered_data31$BCEP_cr_ln[filtered_data31$Gender=="Male"])
mean(filtered_data31$BCEP_cr_ln[filtered_data31$Gender=="Female"])
mean(filtered_data31$DBUP_cr_ln[filtered_data31$Gender=="Male"])
mean(filtered_data31$DBUP_cr_ln[filtered_data31$Gender=="Female"])
table(filtered_data31$RA_status)
filtered_data31$RA_status<-relevel(filtered_data31$RA_status, ref="RA")
##Model 1
library(car)
formula1x<- as.formula("RA_status_binary~DPHP_quantiled +Gender+Age.cat")
fit1x<-glm(formula1x, family = binomial(), data=filtered_data31)
publish(fit1x)
vif(fit1x)
#Model 2
table(filtered_data31$Marital_status)
formula1.1x<- as.formula("RA_status~DPHP_quantiled+Gender+Age.cat+Education+Alcohol+BMI+Activity_level+Ethnicity+Urinary_creatinine_log+PIR+Smoking+Diabetes+Citizenship+Marital_status")
fit1.1x<-glm(formula1.1x, family = binomial(), data=filtered_data31)
publish(fit1.1x)
vif(fit1.1x)
## Design matrix
head(model.matrix(fit1x))
dim(model.matrix(fit1x))
# Predicting probabilities
pred_probabilities <- predict(fit1x, filtered_data31, type = "response")

# Convert probabilities to binary predictions (0.5 is a common threshold)
pred_class <- ifelse(pred_probabilities > 0.5, 1, 0)

# Access the observed values (assuming they are in the same order)
obs_class <- filtered_data31$RA_status

# Number of predictions
n <- length(pred_class)
print(n)

# Plotting observed vs predicted classes
plot(obs_class, pred_class, main = "Observed vs. Predicted Classes", xlab = "Observed Classes", ylab = "Predicted Classes", pch = 19, col = ifelse(obs_class == pred_class, "green", "red"))
abline(0, 1, col = "blue")  # Adding a 45-degree line for reference

# Adding a lowess smoothing line to see the trend
lines(lowess(obs_class, pred_class), col = "red")

# To improve visualization
legend("topright", legend = c("Correct Prediction", "Incorrect Prediction"), col = c("green", "red"), pch = 19)

### trail with test and train
# Load necessary library
library(dplyr)
library(caret) # For creating training and test splits

# Assuming the data frame is named filtered_data31 and has 5490 observations
set.seed(123) # Setting seed for reproducibility

# Splitting the data into training and test sets (60% training, 40% test)
trainIndex <- createDataPartition(filtered_data31$RA_status_binary, p = 0.6, 
                                  list = FALSE, 
                                  times = 1)
trainingSet <- filtered_data31[trainIndex, ]
testSet <- filtered_data31[-trainIndex, ]

# Defining the model formula
formula1x <- as.formula("RA_status_binary ~ DPHP_quantiled + Gender + Age.cat")

# Fitting the logistic regression model on the training set
fit1x <- glm(formula1x, family = binomial(), data = trainingSet)

# Summarize the model fit
summary(fit1x)

# Predicting on the test set
pred_probabilities <- predict(fit1x, testSet, type = "response")
pred_classes <- ifelse(pred_probabilities > 0.5, 1, 0)

# Accessing the observed classes from the test set
obs_classes <- testSet$RA_status_binary

# Calculate and print confusion matrix to evaluate the model
confusionMatrix <- table(Predicted = pred_classes, Observed = obs_classes)
print(confusionMatrix)

# Plotting observed vs predicted classes
plot(obs_classes, pred_classes, main = "Observed vs. Predicted Classes",
     xlab = "Observed Classes", ylab = "Predicted Classes", pch = 19,
     col = ifelse(obs_classes == pred_classes, "green", "red"))
abline(0, 1, col = "blue") # Ideal line where observed = predicted

# Adding a legend to the plot
legend("topright", legend = c("Correct Prediction", "Incorrect Prediction"), 
       col = c("green", "red"), pch = 19)
### method 2
# Load necessary libraries
library(dplyr)
library(caret)
install.packages("ROCR")
library(ROCR)
library(pROC)

# Assuming the data frame is named filtered_data31 and has 5490 observations
set.seed(123) # Setting seed for reproducibility

# Splitting the data into training and test sets (60% training, 40% test)
trainIndex <- createDataPartition(filtered_data31$RA_status_binary, p = 0.6, 
                                  list = FALSE, 
                                  times = 1)
trainingSet <- filtered_data31[trainIndex, ]
testSet <- filtered_data31[-trainIndex, ]

# Addressing class imbalance with SMOTE (Synthetic Minority Over-sampling Technique)
# Note: You might need to install the DMwR package if not already installed
install.packages("DMwR")
library(DMwR)
trainingSet <- SMOTE(RA_status_binary ~ ., trainingSet, perc.over = 100, k = 5)

# Fitting the logistic regression model on the training set
formula1x <- as.formula("RA_status_binary ~ DPHP_quantiled + Gender + Age.cat")
fit1x <- glm(formula1x, family = binomial(), data = trainingSet)

# Predicting probabilities on the test set
pred_probabilities <- predict(fit1x, testSet, type = "response")

# Exploring different thresholds for binary classification
thresholds <- seq(0.1, 0.9, by = 0.1)
for (thresh in thresholds) {
  pred_classes <- ifelse(pred_probabilities > thresh, 1, 0)
  cat("Threshold:", thresh, "\n")
  print(table(Predicted = pred_classes, Observed = testSet$RA_status_binary))
}

# ROC Curve analysis
roc_obj <- roc(response = testSet$RA_status_binary, predictor = pred_probabilities)
plot(roc_obj, main = "ROC Curve")
auc(roc_obj)

# Optimal cutoff based on Youden's Index
coords <- coords(roc_obj, "best", ret="threshold")
optimal_threshold <- coords[1]
cat("Optimal Threshold: ", optimal_threshold, "\n")

# Re-evaluating with optimal threshold
pred_classes_optimal <- ifelse(pred_probabilities > optimal_threshold, 1, 0)
confusionMatrixOptimal <- table(Predicted = pred_classes_optimal, Observed = testSet$RA_status_binary)
print(confusionMatrixOptimal)

# Additional metrics
print(precision(confusionMatrixOptimal))
print(recall(confusionMatrixOptimal))
print(f1_score(confusionMatrixOptimal))








































#converting the urinary_creatinine to log as it is highly right skewed.
#filtered_data31$Urinary_creatinine_log <- log(filtered_data31$Urinary_creatinine)
#filtered_data31$RA_status<-NULL
#filtered_data31$Urinary_creatinine<-NULL
library(survey)
library(Publish)
svy_design <- svydesign(ids = ~PSU, 
                        strata = ~STRATA, 
                        weights = ~W.MEC, 
                        nest = TRUE, 
                        data=filtered_data31)
summary(weights(svy_design))
#please note we are using w.MEC weight because we are using the lab components in the analysis

#crude model and fully adjusted model
library(jtools)
library(ggstance)
library(broom.mixed)
library(huxtable)
library(Publish)
library(dplyr)
library(survey)
library(car)#for VIF
names(filtered_data31)
summary(filtered_data31)
str(filtered_data31)
# DPHP Binomial Logistic Regression Model 1
formula1<- as.formula(I(RA_status_binary)~DPHP_quantiled+Gender+Age.cat)
formula1<- as.formula("RA_status~DPHP_quantiled+Gender+Age.cat")
# Increase max iterations and adjust convergence tolerance
fit1 <- svyglm(formula1,
               design = svy_design,
               family = binomial(link = "logit"),
               control = glm.control(maxit = 100, epsilon = 1e-8))
# Check the output again
summary(fit1)
publish(fit1)
vif(fit1)
####
# Binomial Logistic Regression Model 2
formula1.1<- as.formula(I(RA_status_binary)~DPHP_quantiled+Age.cat*Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit1.1<-svyglm(formula1.1,
               design = svy_design,
               family = binomial(link="logit"),
               control = glm.control(maxit = 100, epsilon = 1e-8))
publish(fit1.1)
summary(fit1.1)
vif(fit1.1)
export_summs(fit1,fit1.1)
plot_summs(fit1,fit1.1)
library(pROC)
##Assessing discrimination of the model with ROC
filtered_data31$prob1=predict(fit1,type = c("response"))
filtered_data31$prob2=predict(fit1.1,type = c("response"))
roc1<- roc(RA_status_binary~prob1, data=filtered_data31)
roc2<-roc(RA_status_binary~prob2, data=filtered_data31)
##plotting roc
# Plot ROC curves
plot(roc1, col = "blue", main = "ROC Curves for Models (DPHP)", xlim = c(1, 0), ylim = c(0, 1), 
     xlab = "1 - Specificity", ylab = "Sensitivity")
plot(roc2, col = "red", add = TRUE)
legend("bottomright", legend = c("Model 1", "Model 1.1"), col = c("blue", "red"), lwd = 2)
# Calculate and add AUC values to the plot
auc1 <- auc(roc1)
auc2 <- auc(roc2)
text(0.6, 0.2, paste("AUC for Model 1: ", round(auc1, 3)), col = "blue")
text(0.6, 0.1, paste("AUC for Model 1.1: ", round(auc2, 3)), col = "red")
# Print AUC values
auc1 <- auc(roc1)
auc2 <- auc(roc2)
print(paste("AUC for Model 1: ", auc1))
print(paste("AUC for Model 1.1: ", auc2))
# BDCPP Binomial Logistic Regression Model 1
formula2<- as.formula(I(RA_status_binary)~BDCPP_quantiled+Gender+Age.cat)
fit2<-svyglm(formula2,
            design = svy_design,
            family = binomial(link="logit"),
            control = glm.control(maxit = 100, epsilon = 1e-8))
publish(fit2)
summary(fit2)
vif(fit2)
# Binomial Logistic Regression Model 2
formula2.1<- as.formula(I(RA_status_binary)~BDCPP_quantiled+Age.cat+Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit2.1<-svyglm(formula2.1,
             design = svy_design,
             family = binomial(link="logit"),
             control = glm.control(maxit = 100, epsilon = 1e-8))
publish(fit2.1)
summary(fit2.1)
vif(fit2.1)
##Assessing discrimination of the model with ROC
filtered_data31$prob1=predict(fit2,type = c("response"))
filtered_data31$prob2=predict(fit2.1,type = c("response"))
roc1<- roc(RA_status_binary~prob1, data=filtered_data31)
roc2<-roc(RA_status_binary~prob2, data=filtered_data31)
##plotting roc
# Plot ROC curves
plot(roc1, col = "blue", main = "ROC Curves for Models (BDCPP)", xlim = c(1, 0), ylim = c(0, 1), 
     xlab = "1 - Specificity", ylab = "Sensitivity")
plot(roc2, col = "red", add = TRUE)
legend("bottomright", legend = c("Model 2", "Model 2.1"), col = c("blue", "red"), lwd = 2)
# Calculate and add AUC values to the plot
auc1 <- auc(roc1)
auc2 <- auc(roc2)
text(0.6, 0.2, paste("AUC for Model 2: ", round(auc1, 3)), col = "blue")
text(0.6, 0.1, paste("AUC for Model 2.1: ", round(auc2, 3)), col = "red")
# Print AUC values
auc1 <- auc(roc1)
auc2 <- auc(roc2)
print(paste("AUC for Model 2: ", auc1))
print(paste("AUC for Model 2.1: ", auc2))
export_summs(fit2,fit2.1)
plot_summs(fit2,fit2.1)
# BCPP Binomial Logistic Regression Model 1
formula3<- as.formula(I(RA_status_binary)~BCPP_quantiled+Gender+Age.cat)
fit3<-svyglm(formula3,
             design = svy_design,
             family = binomial(link="logit"),
             control = glm.control(maxit = 100, epsilon = 1e-8))
publish(fit3)
summary(fit3)
vif(fit3)
# Binomial Logistic Regression Model 2
formula3.1<- as.formula(I(RA_status_binary)~BCPP_quantiled+Age.cat+Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit3.1<-svyglm(formula3.1,
               design = svy_design,
               family = binomial(link="logit"),
               control = glm.control(maxit = 100, epsilon = 1e-8))
publish(fit3.1)
summary(fit3.1)
vif(fit3.1)
##Assessing discrimination of the model with ROC
filtered_data31$prob1=predict(fit3,type = c("response"))
filtered_data31$prob2=predict(fit3.1,type = c("response"))
roc1<- roc(RA_status_binary~prob1, data=filtered_data31)
roc2<-roc(RA_status_binary~prob2, data=filtered_data31)
##plotting roc
# Plot ROC curves
plot(roc1, col = "blue", main = "ROC Curves for Models (BCPP)", xlim = c(1, 0), ylim = c(0, 1), 
     xlab = "1 - Specificity", ylab = "Sensitivity")
plot(roc2, col = "red", add = TRUE)
legend("bottomright", legend = c("Model 3", "Model 3.1"), col = c("blue", "red"), lwd = 2)
# Calculate and add AUC values to the plot
auc1 <- auc(roc1)
auc2 <- auc(roc2)
text(0.6, 0.2, paste("AUC for Model 3: ", round(auc1, 3)), col = "blue")
text(0.6, 0.1, paste("AUC for Model 3.1: ", round(auc2, 3)), col = "red")
# Print AUC values
auc1 <- auc(roc1)
auc2 <- auc(roc2)
print(paste("AUC for Model 3: ", auc1))
print(paste("AUC for Model 3.1: ", auc2))
export_summs(fit3,fit3.1)
plot_summs(fit3,fit3.1)
# BCEP Binomial Logistic Regression Model 1
formula4<- as.formula(I(RA_status_binary)~BCEP_quantiled+Gender+Age.cat)
fit4<-svyglm(formula4,
             design = svy_design,
             family = binomial(link="logit"),
             control = glm.control(maxit = 100, epsilon = 1e-8))
publish(fit4)
summary(fit4)
# Binomial Logistic Regression Model 2
formula4.1<- as.formula(I(RA_status_binary)~BCEP_quantiled+Age.cat+Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit4.1<-svyglm(formula4.1,
               design = svy_design,
               family = binomial(link="logit"),
               control = glm.control(maxit = 100, epsilon = 1e-8))
publish(fit4.1)
summary(fit4.1)
vif(fit4.1)
##Assessing discrimination of the model with ROC
filtered_data31$prob1=predict(fit4,type = c("response"))
filtered_data31$prob2=predict(fit4.1,type = c("response"))
roc1<- roc(RA_status_binary~prob1, data=filtered_data31)
roc2<-roc(RA_status_binary~prob2, data=filtered_data31)
##plotting roc
# Plot ROC curves
plot(roc1, col = "blue", main = "ROC Curves for Models (BCEP)", xlim = c(1, 0), ylim = c(0, 1), 
     xlab = "1 - Specificity", ylab = "Sensitivity")
plot(roc2, col = "red", add = TRUE)
legend("bottomright", legend = c("Model 4", "Model 4.1"), col = c("blue", "red"), lwd = 2)
# Calculate and add AUC values to the plot
auc1 <- auc(roc1)
auc2 <- auc(roc2)
text(0.6, 0.2, paste("AUC for Model 4: ", round(auc1, 3)), col = "blue")
text(0.6, 0.1, paste("AUC for Model 4.1: ", round(auc2, 3)), col = "red")
# Print AUC values
auc1 <- auc(roc1)
auc2 <- auc(roc2)
print(paste("AUC for Model 4: ", auc1))
print(paste("AUC for Model 4.1: ", auc2))
export_summs(fit4,fit4.1)
plot_summs(fit4,fit4.1)
# DBUP Binomial Logistic Regression Model 1
formula5<- as.formula(I(RA_status_binary)~DBUP_quantiled+Gender+Age.cat)
fit5<-svyglm(formula5,
             design = svy_design,
             family = binomial(link="logit"),
             control = glm.control(maxit = 100, epsilon = 1e-8))
publish(fit5)
summary(fit5)
# Binomial Logistic Regression Model 2
formula5.1<- as.formula(I(RA_status_binary)~DBUP_quantiled+Age.cat+Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit5.1<-svyglm(formula5.1,
               design = svy_design,
               family = binomial(link="logit"),
               control = glm.control(maxit = 100, epsilon = 1e-8))
publish(fit5.1)
vif(fit5.1)
##Assessing discrimination of the model with ROC
filtered_data31$prob1=predict(fit5,type = c("response"))
filtered_data31$prob2=predict(fit5.1,type = c("response"))
roc1<- roc(RA_status_binary~prob1, data=filtered_data31)
roc2<-roc(RA_status_binary~prob2, data=filtered_data31)
##plotting roc
# Plot ROC curves
plot(roc1, col = "blue", main = "ROC Curves for Models (DBUP)", xlim = c(1, 0), ylim = c(0, 1), 
     xlab = "1 - Specificity", ylab = "Sensitivity")
plot(roc2, col = "red", add = TRUE)
legend("bottomright", legend = c("Model 5", "Model 5.1"), col = c("blue", "red"), lwd = 2)
# Calculate and add AUC values to the plot
auc1 <- auc(roc1)
auc2 <- auc(roc2)
text(0.6, 0.2, paste("AUC for Model 5: ", round(auc1, 3)), col = "blue")
text(0.6, 0.1, paste("AUC for Model 5.1: ", round(auc2, 3)), col = "red")
# Print AUC values
auc1 <- auc(roc1)
auc2 <- auc(roc2)
print(paste("AUC for Model 5: ", auc1))
print(paste("AUC for Model 5.1: ", auc2))
export_summs(fit5,fit5.1)
plot_summs(fit5,fit5.1)
# This is the code snippet to plot the final summs from the 5 models created
plot_summs(fit1,fit2,fit3,fit4, fit5)
plot_summs(fit1.1,fit2.1,fit3.1,fit4.1,fit5.1)
#save(filtered_data31, file="Datafiltered31NEHA.RData")
#This code snippet is for stratification analysis between Male and Female
# Subset the design for males
svy_design_male <- subset(svy_design, Gender == "Male")
# Subset the design for females
svy_design_female <- subset(svy_design, Gender == "Female")
###
# Model 2 for males
# Binomial Logistic Regression Model 2
formula1.1s<- as.formula(I(RA_status_binary)~DPHP_quantiled+Age.cat+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit1.1_male <- svyglm(formula1.1s, design = svy_design_male, family = binomial(link = "logit"))
# Model 2 for females
fit1.1_female <- svyglm(formula1.1s, design = svy_design_female, family = binomial(link = "logit"))
# Summaries
publish(fit1.1_male)
publish(fit1.1_female)

# Binomial Logistic Regression Model 2
formula2.1s<- as.formula(I(RA_status_binary)~BDCPP_quantiled+Age.cat+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit2.1_male <- svyglm(formula2.1s, design = svy_design_male, family = binomial(link = "logit"))
# Model 2 for females
fit2.1_female <- svyglm(formula2.1s, design = svy_design_female, family = binomial(link = "logit"))
# Summaries
publish(fit2.1_male)
publish(fit2.1_female)
# Binomial Logistic Regression Model 2
formula3.1s<- as.formula(I(RA_status_binary)~BCPP_quantiled+Age.cat+Marital_status+Alcohol+Citizenship+BMI+Diabetes+Activity_level)
fit3.1_male <- svyglm(formula3.1s, design = svy_design_male, family = binomial(link = "logit"))
# Model 2 for females
fit3.1_female <- svyglm(formula3.1s, design = svy_design_female, family = binomial(link = "logit"))
# Summaries
publish(fit3.1_male)
publish(fit3.1_female)
# Binomial Logistic Regression Model 2
formula4.1s<- as.formula(I(RA_status_binary)~BCEP_quantiled+Age.cat+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit4.1_male <- svyglm(formula4.1s, design = svy_design_male, family = binomial(link = "logit"))
# Model 2 for females
fit4.1_female <- svyglm(formula4.1s, design = svy_design_female, family = binomial(link = "logit"))
# Summaries
publish(fit4.1_male)
publish(fit4.1_female)
# Binomial Logistic Regression Model 2
formula5.1s<- as.formula(I(RA_status_binary)~DBUP_quantiled+Age.cat+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit5.1_male <- svyglm(formula5.1s, design = svy_design_male, family = binomial(link = "logit"))
# Model 2 for females
fit5.1_female <- svyglm(formula5.1s, design = svy_design_female, family = binomial(link = "logit"))
# Summaries
publish(fit5.1_male)
publish(fit5.1_female)

####
table(filtered_data31$Age.cat)
#This code snippet is for stratification analysis between Age.cat: 20-60 years and greaterthan 60 years 
# Subset the design for 20-60
svy_design_20 <- subset(svy_design, Age.cat == "20-60 years")
# Subset the design for greaterthan 60 
svy_design_60 <- subset(svy_design, Age.cat == "greaterthan 60 years")
# Model 2 for 20
#  DPHP Binomial Logistic Regression Model 2
formula1.1a<- as.formula(I(RA_status_binary)~DPHP_quantiled+Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit1.1_20 <- svyglm(formula1.1a, design = svy_design_20, family = binomial(link = "logit"))
# Model 2 for greaterthan 60
fit1.1_60 <- svyglm(formula1.1a, design = svy_design_60, family = binomial(link = "logit"))
# Summaries
publish(fit1.1_20)
publish(fit1.1_60)
#  BDCPP Binomial Logistic Regression Model 2
formula2.1a<- as.formula(I(RA_status_binary)~BDCPP_quantiled+Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit1.1_20 <- svyglm(formula2.1a, design = svy_design_20, family = binomial(link = "logit"))
# Model 2 for greaterthan 60
fit1.1_60 <- svyglm(formula1.1a, design = svy_design_60, family = binomial(link = "logit"))
# Summaries
publish(fit1.1_20)
publish(fit1.1_60)
# BCPP Binomial Logistic Regression Model 2
formula3.1a<- as.formula(I(RA_status_binary)~BCPP_quantiled+Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit3.1_20 <- svyglm(formula3.1a, design = svy_design_20, family = binomial(link = "logit"))
# Model 2 for females
fit3.1_60 <- svyglm(formula3.1a, design = svy_design_60, family = binomial(link = "logit"))
# Summaries
publish(fit3.1_20)
publish(fit3.1_60)
# BCEP Binomial Logistic Regression Model 2
formula4.1a<- as.formula(I(RA_status_binary)~BCEP_quantiled+Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit4.1_20 <- svyglm(formula4.1a, design = svy_design_20, family = binomial(link = "logit"))
# Model 2 for females
fit4.1_60 <- svyglm(formula4.1a, design = svy_design_60, family = binomial(link = "logit"))
# Summaries
publish(fit4.1_20)
publish(fit4.1_60)
# Binomial Logistic Regression Model 2
formula5.1a<- as.formula(I(RA_status_binary)~DBUP_quantiled+Gender+Marital_status+Citizenship+BMI+Diabetes+Activity_level)
fit5.1_20 <- svyglm(formula5.1a, design = svy_design_20, family = binomial(link = "logit"))
# Model 2 for females
fit5.1_60 <- svyglm(formula5.1a, design = svy_design_60, family = binomial(link = "logit"))
# Summaries
publish(fit5.1_20)
publish(fit5.1_60)

###




















