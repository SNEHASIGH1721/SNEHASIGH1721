getwd()
load("Datafiltered31UC.RData")
ls()
names(filtered_data31)
filtered_data31$log_DBUP <- log(filtered_data31$DBUP_Corrected)
filtered_data31$log_BDCPP<- log(filtered_data31$BDCPP_Corrected)
filtered_data31$log_BCPP <- log(filtered_data31$BCPP_Corrected)
filtered_data31$log_BCEP <- log(filtered_data31$BCEP_Corrected)
filtered_data31$log_DPHP <- log(filtered_data31$DPHP_Corrected)

hist(filtered_data31$log_DBUP, main="Histogram of DBUP log tranformed", xlab="DPHP", col="blue")
hist(filtered_data31$log_BDCPP, main="Histogram of BDCPP log tranformed", xlab="BDCPP", col="red")
hist(filtered_data31$log_BCPP, main="Histogram of BCPP log tranformed", xlab="BCPP", col="green")

hist(filtered_data31$log_BCEP, main="Histogram of BCEP log tranformed", xlab="BCEP", col="purple")
hist(filtered_data31$log_DPHP, main="Histogram of DPHP log tranformed", xlab="DBUP", col="orange")



# Load necessary libraries
library(dplyr)
library(broom)
library(tableone)
library(Publish)
names(filtered_data31)
str(filtered_data31)
summary(filtered_data31)
names(filtered_data31)
vars=c("Gender", "Age", "Ethnicity", "Marital_status", "Education","Citizenship",
       "Alcohol", "BMI", "Diabetes","Smoking", "Age.cat", 
       "RA_status", "Activity_level", "PIR","Urinary_creatinine")
tableOne<- CreateTableOne(data=filtered_data31, includeNA = TRUE,
                          vars = vars)
tableOne
# Assuming your data frame 'filtered_data31' has columns for the urinary metabolites
# Extract the relevant columns for the metabolites
library(vioplot)
library(kableExtra)
library(survey)
metabolites <- filtered_data31[,c("log_DBUP", "log_BDCPP", "log_BCPP", "log_BCEP", "log_DPHP")]

# Create a list of metabolite data to be used in the violin plot
metabolite_list <- list(metabolites$log_DBUP, metabolites$log_BDCPP, metabolites$log_BCPP, metabolites$log_BCEP,metabolites$log_DPHP)

# Create the violin plot
vioplot(metabolite_list, names = c("log_DBUP", "log_BDCPP", "log_BCPP", "log_BCEP", "log_DPHP"),
        col = rainbow(5),
        main = "Distribution of log transformed and corrected concentrations of metabolites")
title(ylab = "Concentration of log transformed flame retardants")
### spearman correlation plot among 5 FR Metabolites
# Select only the relevant metabolite columns
metabolites1 <- filtered_data31[c("log_DBUP", "log_BDCPP", "log_BCPP", "log_BCEP", "log_DPHP")]

# Calculate Spearman correlation
spearman_correlation2 <- cor(metabolites1, method = "spearman", use = "complete.obs")
corrplot(spearman_correlation2, method="color", type="upper", 
         order="hclust", addCoef.col = "black", 
         tl.col="black", tl.srt=45, 
         diag=FALSE)
title(main="Spearman correlation among creatinine corrected and log tranformed metabolites of flame retardants", 
      col.main="darkblue",  # This changes the color of the title to blue
      font.main=2)
save(filtered_data31, file="Datafiltered31UC.RData")


svy_design <- svydesign(ids = ~PSU, 
                        strata = ~STRATA, 
                        weights = ~W.MEC, 
                        data = filtered_data31, 
                        nest = TRUE)
summary(weights(svy_design))
##
vars2=c("Gender", "Age.cat", "Ethnicity", "Marital_status", "Education","Citizenship", "Alcohol", "Diabetes","Smoking", "Age.cat", 
        "RA_status", "Activity_level", "PIR")

kableone<- function(x, ...) {
  capture.output(x<- print(x, showALLLevels=TRUE, padColnames = TRUE, insertLevel = TRUE))
  knitr::kable(x, ...)
}
kableone(svyCreateCatTable(var=vars2, strata = "Gender", data=svy_design, test= TRUE))

kableone(svyCreateCatTable(var=vars2, strata = "Ethnicity", data=svy_design, test= TRUE))
kableone(svyCreateCatTable(var=vars2, strata = "RA_status", data=svy_design, test= TRUE))
summary(filtered_data31)
### model1, model1.1, model1.2 for DBUP
model1 <- lm(RA_status_binary ~ log_DBUP, data = filtered_data31)
publish(model1)
filtered_data31$RA_status_binary <- ifelse(filtered_data31$RA_status == "RA", 1, 0)
model1.1 <- glm(RA_status_binary ~ log_DBUP+Age.cat+Gender+Ethnicity, family = binomial(link = "logit"), data = filtered_data31)
publish(model1.1)
model1.2 <- glm(RA_status_binary ~ log_DBUP+Age.cat+Gender+Ethnicity+Marital_status+Citizenship+Alcohol+BMI+Diabetes+Smoking+Activity_level+PIR+Urinary_Creatinine_ug_L,
                family = binomial(link = "logit"), data = filtered_data31)
publish(model1.2)
### model2, model2.1, model2.2 for BDCPP
model2 <- lm(log_BDCPP ~ RA_status, data = filtered_data31)
publish(model2)
model2.1 <- glm(RA_status_binary ~ log_BDCPP+Age.cat+Gender+Ethnicity, family = binomial(link = "logit"), data = filtered_data31)
publish(model2.1)
model2.2 <- glm(RA_status_binary ~ log_BDCPP+Age.cat+Gender+Ethnicity+Marital_status+Citizenship+Alcohol+BMI+Diabetes+Smoking+Activity_level+PIR+Urinary_Creatinine_ug_L,
                family = binomial(link = "logit"), data = filtered_data31)
publish(model2.2)
### model3, model3.1, model3.2 for BCPP
model3 <- lm(log_BCPP ~ RA_status, data = filtered_data31)
summary(model3)
model3.1 <- glm(RA_status_binary ~ log_BCPP+Age.cat+Gender+Ethnicity, family = binomial(link = "logit"), data = filtered_data31)
publish(model3.1)
model3.2 <- glm(RA_status_binary ~ log_BCPP+Age.cat+Gender+Ethnicity, family = binomial(link = "logit"), data = filtered_data31)
publish(model3.2)
### model4, model4.1, model4.2 for BCPP
model3 <- lm(log_BCPP ~ RA_status, data = filtered_data31)
summary(model3)
model3.1 <- glm(RA_status_binary ~ log_BCPP+Age.cat+Gender+Ethnicity, family = binomial(link = "logit"), data = filtered_data31)
publish(model3.1)
model3.2 <- glm(RA_status_binary ~ log_BCPP+Age.cat+Gender+Ethnicity, family = binomial(link = "logit"), data = filtered_data31)
publish(model3.2)

# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(broom)

# Load necessary libraries
install.packages("mgcv")
library(mgcv)
library(ggplot2)


library(mgcv)
library(ggplot2)

library(mgcv)
library(ggplot2)

# Fit a GAM with a logit link and a more complex smoother for log_DPHP
gam_model <- gam(RA_status_binary ~ s(log_DPHP, k = 15), data = filtered_data31, family = binomial)

# Predict values for plotting
pred_data <- data.frame(log_DPHP = seq(min(filtered_data31$log_DPHP, na.rm = TRUE), 
                                       max(filtered_data31$log_DPHP, na.rm = TRUE), length = 100))

# Predicting on the link scale (log odds)
predictions <- predict(gam_model, newdata = pred_data, type = "link", se = TRUE)
pred_data$fit <- predictions$fit
pred_data$upper <- predictions$fit + 1.96 * predictions$se.fit
pred_data$lower <- predictions$fit - 1.96 * predictions$se.fit

# Convert link (log odds) to probability for the plot
pred_data$odds_ratio <- exp(pred_data$fit) / (1 + exp(pred_data$fit))
pred_data$upper_odds_ratio <- exp(pred_data$upper) / (1 + exp(pred_data$upper))
pred_data$lower_odds_ratio <- exp(pred_data$lower) / (1 + exp(pred_data$lower))

# Create the plot with confidence intervals
ggplot(filtered_data31, aes(x = log_DPHP)) +
  geom_ribbon(data = pred_data, aes(ymin = lower_odds_ratio, ymax = upper_odds_ratio), alpha = 0.2, fill = "blue") +
  geom_point(aes(y = RA_status_binary), colour = "blue", position = position_jitter(height = 0.05)) +
  geom_line(data = pred_data, aes(y = odds_ratio), colour = "red") +
  labs(x = "log2-transformed DPHP (ng/mL)", y = "Odds Ratio for Obesity", 
       title = "Non-linear Relationship between log2-transformed DPHP and Odds Ratio for Obesity") +
  theme_minimal()
## code ending in line 158 gave an output as desired. 
### 
#final regression model as per author
model1_logistic_RA <- glm(RA_status_binary ~ log_DPHP + Age.cat + Gender + Ethnicity, 
                          data = filtered_data31, 
                          family = "binomial")
publish(model1_logistic_RA)

model2_logistic_RA <- glm(RA_status_binary ~ log_DPHP + Gender + Age + Ethnicity + Marital_status + 
                            Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                            Smoking + Activity_level + PIR, 
                          data = filtered_data31, 
                          family = "binomial")
publish(model2_logistic_RA)
gam_model <- gam(RA_status_binary ~ s(log_DPHP) + Gender + Age + Ethnicity + Marital_status + 
                   Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                   Smoking + Activity_level + PIR, 
                 data = filtered_data31, 
                 family = "binomial")
gam_model
library(ggplot2)
library(broom)

# Extract coefficients from the models
coef1 <- tidy(model1_logistic_RA)
coef2 <- tidy(model2_logistic_RA)


# Combine coefficients into one data frame and add a model identifier
coef1$model <- 'Model1'
coef2$model <- 'Model2'

combined_coefs <- rbind(coef1, coef2)

# Plot
ggplot(combined_coefs, aes(x = term, y = estimate, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                position = position_dodge(width = 0.5), width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Coefficient Estimate", x = "Predictor", title = "Comparison of Model Coefficients")

### probabilites vius(still testing which method is good to visualize)
install.packages("pROC")
library(pROC)
# Generate predicted probabilities
probabilities_model1 <- predict(model1_logistic_RA, filtered_data31, type = "response")
probabilities_model2 <- predict(model2_logistic_RA, filtered_data31, type = "response")

# Create ROC objects
roc_model1 <- roc(filtered_data31$RA_status_binary, probabilities_model1)
roc_model2 <- roc(filtered_data31$RA_status_binary, probabilities_model2)

# Plotting ROC curves for both models in the same plot
plot(roc_model1, col = "blue", main="ROC Curves Comparison", print.auc = TRUE)
plot(roc_model2, col = "red", add = TRUE, print.auc = TRUE)

# Adding legend
legend("bottomright", legend=c("Model 1", "Model 2"), col=c("blue", "red"), lwd=2)

### Another method
library(ggplot2)
library(broom)

# Extract coefficients from the models
tidy_model1 <- tidy(model1_logistic_RA)
tidy_model2 <- tidy(model2_logistic_RA)

# Add a model identifier
tidy_model1$model <- 'Model 1'
tidy_model2$model <- 'Model 2'

# Combine the data from both models
combined_models <- rbind(tidy_model1, tidy_model2)

# Plot the coefficients with confidence intervals
ggplot(combined_models, aes(x = term, y = estimate, color = model)) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.25)) +
  coord_flip() +  # Flip coordinates for easier reading of terms
  labs(title = "Coefficient Estimates from Logistic Regression Models", y = "Coefficient Estimate", x = "") +
  theme(legend.position = "bottom")

# Diagnostic plots for Model 1
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model1_logistic_RA)

# Diagnostic plots for Model 2
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model2_logistic_RA)




# Now we will compute this for the other metabolites 
model1_logistic_RA <- glm(RA_status_binary ~ log_BDCPP + Age.cat + Gender + Ethnicity, 
                          data = filtered_data31, 
                          family = "binomial")
publish(model1_logistic_RA)

model2_logistic_RA <- glm(RA_status_binary ~ log_BDCPP + Gender + Age + Ethnicity + Marital_status + 
                            Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                            Smoking + Activity_level + PIR, 
                          data = filtered_data31, 
                          family = "binomial")
publish(model2_logistic_RA)
gam_model <- gam(RA_status_binary ~ s(log_BDCPP) + Gender + Age + Ethnicity + Marital_status + 
                   Education + Citizenship + Urinary_creatinine + Alcohol + BMI + Diabetes + 
                   Smoking + Activity_level + PIR, 
                 data = filtered_data31, 
                 family = "binomial")
gam_model

