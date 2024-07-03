getwd()
library(dplyr)
load("Datafiltered31NEHA.RData")
ls()
filtered_data31$RA_status_binary <- ifelse(filtered_data31$RA_status == "RA", 1, 0)
names(filtered_data31)
str(filtered_data31)
filtered_data31$Activity_level <- as.factor(filtered_data31$Activity_level)
filtered_data31$RA_status_binary <- factor(filtered_data31$RA_status_binary, levels = c(0, 1))
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
filtered_data31<-filtered_data31%>% select(-c("DPHP","BDCPP", "BCPP", "BCEP","DBUP","logDPHP"))
filtered_data31<-filtered_data31%>% select(-c("logBDCPP","logBCPP","logBCEP","logDBUP","DPHP_cr","BDCPP_cr","BCPP_cr","BCEP_cr","DBUP_cr","DPHP_cr_ln",
                                              "BDCPP_cr_ln","BCPP_cr_ln","BCEP_cr_ln","DBUP_cr_ln"))
###
svy_design <- svydesign(ids = ~PSU, 
                        strata = ~STRATA, 
                        weights = ~W.MEC, 
                        nest = TRUE, 
                        data=filtered_data31)
summary(weights(svy_design))



# Fit the logistic regression model
model <- svyglm(RA_status_binary ~ DPHP_quantiled + BDCPP_quantiled + BCPP_quantiled + BCEP_quantiled + DBUP_quantiled,
             family = binomial(), design = svy_design)

# Display the summary of the model
summary(model)
library(Publish)
publish(model)
library(car)
vif(model)
###
##3
# Fit the logistic regression model
model1 <- glm(RA_status_binary ~ DPHP_quantiled + BDCPP_quantiled + BCPP_quantiled + BCEP_quantiled + DBUP_quantiled,
             family = binomial(), data = filtered_data31)

# Display the summary of the model
summary(model1)
library(Publish)
publish(model1)
vif(model1)
# Load necessary libraries
library(caret)  # For machine learning functions
library(dplyr)  # For data manipulation
###
set.seed(123)  # for reproducibility
# Create training (80%) and test (20%) sets
index <- createDataPartition(filtered_data31$RA_status_binary, p=0.8, list=FALSE)
trainData <- filtered_data31[index, ]
testData <- filtered_data31[-index, ]
###
# Define the control function for training (optional: for cross-validation)
train_control <- trainControl(method="cv", number=10)

# Fit the logistic regression model
model <- train(RA_status_binary ~ ., data=trainData, method="glm",
               family=binomial, trControl=train_control)
publish(model)

###
# Predict on the test data
predictions <- predict(model, newdata=testData)
summary(predictions)
###
# Confusion Matrix and accuracy
confusionMatrix(predictions, testData$RA_status_binary)
##3
# Convert categorical variables to factors
categorical_vars <- c("Gender", "Ethnicity", "Marital_status", "Education", 
                      "Citizenship", "Diabetes", "Smoking", "Age.cat", "Activity_level")

filtered_data31[categorical_vars] <- lapply(filtered_data31[categorical_vars], factor)
# Create dummy variables
dummy_vars <- model.matrix(~ . - 1, data = filtered_data31[categorical_vars])  # '-1' omits the intercept

# Check the structure of the new dummy variable matrix
str(dummy_vars)
# Bind the dummy variables with the original data
# First, remove the original categorical columns
filtered_data31 <- filtered_data31[, !(names(filtered_data31) %in% categorical_vars)]

# Then, bind the dummy variables to the original dataframe
filtered_data31 <- cbind(filtered_data31, dummy_vars)

# View the updated dataframe structure
str(filtered_data31)

