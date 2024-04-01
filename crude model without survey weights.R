getwd()
load("Datafiltered31UC.RData")
ls()
names(filtered_data31)
#final regression model as per author
filtered_data31$RA_status_binary <- ifelse(filtered_data31$RA_status == "RA", 1, 0)
model1<- glm(RA_status_binary ~ log_DPHP + Age.cat + Gender + Ethnicity, 
                          data = filtered_data31, 
                          family = "binomial")
install.packages("Publish")
library(Publish)
publish(model1)

model1.1 <- glm(RA_status_binary ~ log_DPHP + Gender + Age + Ethnicity + Marital_status + 
                            Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                            Smoking + Activity_level + PIR, 
                          data = filtered_data31, 
                          family = "binomial")
publish(model1.1)
install.packages("mgcv")
library(mgcv)
names(filtered_data31)
gam_model1 <- gam(RA_status_binary ~ s(log_DPHP) + Gender + Age + Ethnicity + Marital_status + 
                   Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                   Smoking + Activity_level + PIR, 
                 data = filtered_data31, 
                 family = "binomial")
plot(gam_model1,  main = "Generalized Additive Model plot for DPHP")

##log_BDCPP
model2<- glm(RA_status_binary ~ log_BDCPP + Age.cat + Gender + Ethnicity, 
                          data = filtered_data31, 
                          family = "binomial")
publish(model2)
install.packages("broom")
library(broom)
model2.1<- glm(RA_status_binary ~ log_BDCPP + Gender + Age + Ethnicity + Marital_status + 
                            Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                            Smoking + Activity_level + PIR, 
                          data = filtered_data31, 
                          family = "binomial")
publish(model2.1)
gam_model2 <- gam(RA_status_binary ~ s(log_BDCPP) + Gender + Age + Ethnicity + Marital_status + 
                   Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                   Smoking + Activity_level + PIR, 
                 data = filtered_data31, 
                 family = "binomial")
plot(gam_model2, main = "Generalized Additive Model plot for BDCPP")
##log_BCPP
model3<- glm(RA_status_binary ~ log_BCPP + Age.cat + Gender + Ethnicity, 
             data = filtered_data31, 
             family = "binomial")
publish(model3)

model3.1<- glm(RA_status_binary ~ log_BCPP + Gender + Age + Ethnicity + Marital_status + 
                 Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                 Smoking + Activity_level + PIR, 
               data = filtered_data31, 
               family = "binomial")
publish(model3.1)
gam_model3 <- gam(RA_status_binary ~ s(log_BCPP) + Gender + Age + Ethnicity + Marital_status + 
                    Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                    Smoking + Activity_level + PIR, 
                  data = filtered_data31, 
                  family = "binomial")
plot(gam_model3, main = "Generalized Additive Model plot for BCPP")
##log_BCEP
model4<- glm(RA_status_binary ~ log_BCEP + Age.cat + Gender + Ethnicity, 
             data = filtered_data31, 
             family = "binomial")
publish(model4)

model4.1<- glm(RA_status_binary ~ log_BCEP + Gender + Age + Ethnicity + Marital_status + 
                 Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                 Smoking + Activity_level + PIR, 
               data = filtered_data31, 
               family = "binomial")
publish(model4.1)
gam_model4 <- gam(RA_status_binary ~ s(log_BCEP) + Gender + Age + Ethnicity + Marital_status + 
                    Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                    Smoking + Activity_level + PIR, 
                  data = filtered_data31, 
                  family = "binomial")
plot(gam_model4, main = "Generalized Additive Model plot for BCEP")
###
gam_model5 <- gam(RA_status_binary ~ s(log_DBUP) + Gender + Age + Ethnicity + Marital_status + 
                    Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                    Smoking + Activity_level + PIR, 
                  data = filtered_data31, 
                  family = "binomial")
plot(gam_model5, main = "Generalized Additive Model plot for DBUP")


install.packages("Publish")
library(Publish)


###
# Load necessary libraries
install.packages("rms")
library(rms) 
install.packages("MASS")
library(MASS)
# for the mfp function

# Assuming your data is stored in a dataframe called "data"
# with columns "DPHP" (m-OPFR exposure) and "RA_status" (binary outcome)

# Define data distribution
ddist <- datadist(filtered_data31)
options(datadist = "ddist")

# Assuming your data is stored in a dataframe called "filtered_data31"
# with columns "log_DPHP" (m-OPFR exposure) and "RA_status_binary" (binary outcome)

# Fit a logistic regression model with restricted cubic splines
model <- lrm(RA_status_binary ~ rcs(log_DPHP, 3), data = filtered_data31)
summary(model)
# Plot the dose-response curve
plot(Predict(model, fun = plogis), lwd = 2, xlab = "log_DPHP", ylab = "Probability of RA_status",
     col = "blue", lty = 1)
## dummy code
# Generate predictions and confidence intervals for plotting
pred <- Predict(model, log_DPHP, fun = plogis)
# Plot the dose-response curve
plot(pred, lwd = 2, xlab = "log_DPHP", ylab = "Probability of Rheumatoid Arthritis", col = "pink3", 
     main = "Dose-Response Curve of log transformed and creatinine corrected DPHP")
##tried this code
##
# Load necessary libraries
library(rms)
library(ggplot2)

# Define data distribution for the rms functions
ddist <- datadist(filtered_data31)
options(datadist = "ddist")

# Fit the logistic regression model
model <- lrm(RA_status_binary ~ rcs(log_DPHP, 3), data = filtered_data31)

# Generate predictions and confidence intervals for plotting
pred <- Predict(model, log_DPHP = filtered_data31$log_DPHP, fun = plogis)

# Convert pred object to a data frame for ggplot
pred_df <- data.frame(log_DPHP = pred$log_DPHP, Predicted = pred$y, 
                      Lower = pred$lower, Upper = pred$upper)

# Create the ggplot object with the dose-response curve and confidence interval
ggplot(data = pred_df, aes(x = log_DPHP)) +
  geom_line(aes(y = Predicted), color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_rug(data = filtered_data31, aes(x = log_DPHP), color = "red", sides = "b") +
  labs(x = "log tranformed DPHP", y = "Probability of Rheumatoid Arthritis", 
       title = "Dose-Response Curve of DPHP") +
  theme_minimal()
#####
# Fit the logistic regression model
model <- lrm(RA_status_binary ~ rcs(log_BDCPP, 3), data = filtered_data31)

# Generate predictions and confidence intervals for plotting
pred <- Predict(model, log_BDCPP = filtered_data31$log_BDCPP, fun = plogis)

# Convert pred object to a data frame for ggplot
pred_df <- data.frame(log_BDCPP = pred$log_BDCPP, Predicted = pred$y, 
                      Lower = pred$lower, Upper = pred$upper)

# Create the ggplot object with the dose-response curve and confidence interval
ggplot(data = pred_df, aes(x = log_BDCPP)) +
  geom_line(aes(y = Predicted), color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_rug(data = filtered_data31, aes(x = log_BDCPP), color = "red", sides = "b") +
  labs(x = "log tranformed BDCPP", y = "Probability of Rheumatoid Arthritis", 
       title = "Dose-Response Curve of BDCPP") +
  theme_minimal()

####BCEP###
# Fit the logistic regression model
model <- lrm(RA_status_binary ~ rcs(log_BCEP, 3), data = filtered_data31)

# Generate predictions and confidence intervals for plotting
pred <- Predict(model, log_BCEP = filtered_data31$log_BCEP, fun = plogis)

# Convert pred object to a data frame for ggplot
pred_df <- data.frame(log_BCEP = pred$log_BCEP, Predicted = pred$y, 
                      Lower = pred$lower, Upper = pred$upper)

# Create the ggplot object with the dose-response curve and confidence interval
ggplot(data = pred_df, aes(x = log_BCEP)) +
  geom_line(aes(y = Predicted), color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_rug(data = filtered_data31, aes(x = log_BCEP), color = "red", sides = "b") +
  labs(x = "log tranformed BCEP", y = "Probability of Rheumatoid Arthritis", 
       title = "Dose-Response Curve of BCEP") +
  theme_minimal()
####DBUP###
# Fit the logistic regression model
model <- lrm(RA_status_binary ~ rcs(log_DBUP, 3), data = filtered_data31)

# Generate predictions and confidence intervals for plotting
pred <- Predict(model, log_DBUP = filtered_data31$log_DBUP, fun = plogis)

# Convert pred object to a data frame for ggplot
pred_df <- data.frame(log_DBUP = pred$log_DBUP, Predicted = pred$y, 
                      Lower = pred$lower, Upper = pred$upper)

# Create the ggplot object with the dose-response curve and confidence interval
ggplot(data = pred_df, aes(x = log_DBUP)) +
  geom_line(aes(y = Predicted), color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_rug(data = filtered_data31, aes(x = log_DBUP), color = "red", sides = "b") +
  labs(x = "log tranformed DBUP", y = "Probability of Rheumatoid Arthritis", 
       title = "Dose-Response Curve of DBUP") +
  theme_minimal()
###BCPP###
# Fit the logistic regression model
model <- lrm(RA_status_binary ~ rcs(log_BCPP, 3), data = filtered_data31)

# Generate predictions and confidence intervals for plotting
pred <- Predict(model, log_BCPP = filtered_data31$log_BCPP, fun = plogis)

# Convert pred object to a data frame for ggplot
pred_df <- data.frame(log_BCPP = pred$log_BCPP, Predicted = pred$y, 
                      Lower = pred$lower, Upper = pred$upper)

# Create the ggplot object with the dose-response curve and confidence interval
ggplot(data = pred_df, aes(x = log_BCPP)) +
  geom_line(aes(y = Predicted), color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_rug(data = filtered_data31, aes(x = log_BCPP), color = "red", sides = "b") +
  labs(x = "log tranformed BCPP", y = "Probability of Rheumatoid Arthritis", 
       title = "Dose-Response Curve of BCPP") +
  theme_minimal()

###BDCPP
# Fit a logistic regression model with restricted cubic splines
model2 <- lrm(RA_status_binary ~ rcs(log_BDCPP, 3), data = filtered_data31)
summary(model2)
# Plot the dose-response curve
plot(Predict(model2, fun = plogis), lwd = 2, xlab = "log_BDCPP", ylab = "Probability of RA_status",
     col = "blue", lty = 1)
## dummy code
# Generate predictions and confidence intervals for plotting
pred <- Predict(model2, log_BDCPP, fun = plogis)
# Plot the dose-response curve
plot(pred, lwd = 2, xlab = "log_BDCPP", ylab = "Probability of Rheumatoid Arthritis", col = "plum1", 
     main = "Dose-Response Curve of log transformed and creatinine corrected BDCPP")
###BCPP
# Fit a logistic regression model with restricted cubic splines
model3 <- lrm(RA_status_binary ~ rcs(log_BCPP, 3), data = filtered_data31)
summary(model3)
# Plot the dose-response curve
plot(Predict(model3, fun = plogis), lwd = 2, xlab = "log_BCPP", ylab = "Probability of RA_status",
     col = "blue", lty = 1)
## dummy code
# Generate predictions and confidence intervals for plotting
pred <- Predict(model3, log_BCPP, fun = plogis)
# Plot the dose-response curve
plot(pred, lwd = 2, xlab = "log_BCPP", ylab = "Probability of RA_Status", col = "orange", 
     main = "Dose-Response Curve of log transformed and creatinine corrected BCPP")
##
# Fit a logistic regression model with restricted cubic splines
model4 <- lrm(RA_status_binary ~ rcs(log_BCEP, 3), data = filtered_data31)
summary(model4)
# Plot the dose-response curve
plot(Predict(model4, fun = plogis), lwd = 2, xlab = "log_BCEP", ylab = "Probability of RA_status",
     col = "blue", lty = 1)
## dummy code
# Generate predictions and confidence intervals for plotting
pred <- Predict(model4, log_BCEP, fun = plogis)
# Plot the dose-response curve
plot(pred, lwd = 2, xlab = "log_BCEP", ylab = "Probability of RA_Status", col = "pink", 
     main = "Dose-Response Curve of log transformed and creatinine corrected BCEP")
###
# Fit a logistic regression model with restricted cubic splines
model5 <- lrm(RA_status_binary ~ rcs(log_DBUP, 3), data = filtered_data31)
summary(model5)
# Plot the dose-response curve
plot(Predict(model5, fun = plogis), lwd = 2, xlab = "log_DBUP", ylab = "Probability of RA_status",
     col = "blue", lty = 1)
## dummy code
# Generate predictions and confidence intervals for plotting
pred <- Predict(model5, log_DBUP, fun = plogis)
# Plot the dose-response curve
plot(pred, lwd = 2, xlab = "log_DBUP", ylab = "Probability of RA_Status", col = "cyan2", 
     main = "Dose-Response Curve of log transformed and creatinine corrected DBUP")
rug(filtered_data31$log_DBUP, col = "red")





