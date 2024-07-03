getwd()
load("Datafiltered31UC.RData")
ls()
names(filtered_data31)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape the data from wide to long format
long_data <- filtered_data31 %>%
  gather(key = "m_OPFR", value = "concentration", DPHP, BDCPP, BCPP, BCEP, DBUP) %>%
  mutate(age_group = cut(Age,
                         breaks = c(19, 60, Inf),
                         labels = c("20-60", ">60"),
                         right = FALSE))

# Calculate geometric means and 95% CIs
summary_data <- long_data %>%
  group_by(m_OPFR, age_group) %>%
  summarise(
    geom_mean = exp(mean(log(concentration))),
    ci_lower = exp(mean(log(concentration)) - qt(0.975, length(concentration)-1) * sd(log(concentration))/sqrt(length(concentration))),
    ci_upper = exp(mean(log(concentration)) + qt(0.975, length(concentration)-1) * sd(log(concentration))/sqrt(length(concentration)))
  ) %>%
  ungroup()

# Plotting the bar graph
agestrati <- ggplot(summary_data, aes(x = m_OPFR, y = geom_mean, fill = age_group)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .2, position = position_dodge(.9)) +
  scale_fill_manual(values = c("dodgerblue4", "goldenrod")) +
  labs(
    #title = "Concentration distribution of OPEs stratisfied by Age group",  # Add your desired title text here
    x = "Urinary biomarkers of OPEs exposure", 
    y = "Geometric Mean  in μg/L (95% CI)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Add this line to slant the x-axis labels
agestrati
###
# Reshape the data from wide to long format
long_data <- filtered_data31 %>%
  gather(key = "m_OPFR", value = "concentration", DPHP, BDCPP, BCPP, BCEP, DBUP) %>%
  mutate(gender = factor(Gender, levels = c("Male", "Female")))

# Calculate geometric means and 95% CIs
summary_data <- long_data %>%
  group_by(m_OPFR, gender) %>%
  summarise(
    geom_mean = exp(mean(log(concentration))),
    ci_lower = exp(mean(log(concentration)) - qt(0.975, length(concentration)-1) * sd(log(concentration))/sqrt(length(concentration))),
    ci_upper = exp(mean(log(concentration)) + qt(0.975, length(concentration)-1) * sd(log(concentration))/sqrt(length(concentration)))
  ) %>%
  ungroup()

# Plotting the bar graph
gender1<- ggplot(summary_data, aes(x = m_OPFR, y = geom_mean, fill = gender)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .2, position = position_dodge(.9)) +
  scale_fill_manual(values = c("dodgerblue4", "goldenrod")) + 
  labs(
    #title = "Concentration distribution of OPEs stratisfied by Gender",  # Add your desired title text here
    x = "Urinary biomarkers of OPEs exposure", 
    y = "Geometric Mean in μg/L (95% CI)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Add this line to slant the x-axis labels# Using blue for males and red for females
gender1 
###
# Reshape the data from wide to long format
long_data <- filtered_data31 %>%
  gather(key = "m_OPFR", value = "concentration", DPHP, BDCPP, BCPP, BCEP, DBUP) %>%
  mutate(ethnicity = factor(Ethnicity, levels = c("Mexican American", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Other race")))

# Calculate geometric means and 95% CIs
summary_data <- long_data %>%
  group_by(m_OPFR, ethnicity) %>%
  summarise(
    geom_mean = exp(mean(log(concentration))),
    ci_lower = exp(mean(log(concentration)) - qt(0.975, length(concentration)-1) * sd(log(concentration))/sqrt(length(concentration))),
    ci_upper = exp(mean(log(concentration)) + qt(0.975, length(concentration)-1) * sd(log(concentration))/sqrt(length(concentration)))
  ) %>%
  ungroup()

# Plotting the bar graph
ethinicity<- ggplot(summary_data, aes(x = m_OPFR, y = geom_mean, fill = ethnicity)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .2, position = position_dodge(.9)) +
  scale_fill_manual(values =c("gold","yellow3","khaki3","goldenrod","dodgerblue4")) + # Using a color palette suitable for categorical data
  labs(x = "Urinary biomarkers of OPEs exposure", y = "Geometric Mean in μg/L (95% CI)", fill = "Ethnicity") +
  theme_minimal()
ethinicity
library(gridExtra)
# Arrange the plots in a 3x2 grid
grid.arrange(gender1, agestrati,ethinicity, nrow = 2, ncol = 2)

###
# Plotting the bar graph
ggplot(summary_data, aes(x = m_OPFR, y = geom_mean, fill = ethnicity)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .2, position = position_dodge(.9)) +
  scale_fill_brewer(palette = "Paired") + # Using a color palette suitable for categorical data
  labs(x = "Urinary biomarkers of OPFRs", y = "Geometric Mean in ug/L (95% CI)", fill = "Ethnicity") +
  theme_minimal()

####
# Reshape the data from wide to long format
long_data <- filtered_data31 %>%
  gather(key = "m_OPFR", value = "concentration", DPHP, BDCPP, BCPP, BCEP, DBUP) %>%
  mutate(rastatus = factor(RA_status, levels = c("RA", "Non-RA")))

# Calculate geometric means and 95% CIs
summary_data <- long_data %>%
  group_by(m_OPFR, RA_status) %>%
  summarise(
    geom_mean = exp(mean(log(concentration))),
    ci_lower = exp(mean(log(concentration)) - qt(0.975, length(concentration)-1) * sd(log(concentration))/sqrt(length(concentration))),
    ci_upper = exp(mean(log(concentration)) + qt(0.975, length(concentration)-1) * sd(log(concentration))/sqrt(length(concentration)))
  ) %>%
  ungroup()

# Plotting the bar graph
ggplot(summary_data, aes(x = m_OPFR, y = geom_mean, fill = RA_status)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .2, position = position_dodge(.9)) +
  scale_fill_manual(values = c("rosybrown3", "palegreen1")) + # Using blue for males and red for females
  labs(x = "Urinary biomarkers of OPFRs", y = "Geometric Mean (95% CI)", fill = "RA status") +
  labs(
    title = "Concentration distribution of Flame Retardants stratisfied by Rheumatoid Arthritis",  # Add your desired title text here
    x = "Urinary biomarkers of Flame Retardants", 
    y = "Geometric Mean in ug/L (95% CI)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Add this line to slant the x-axis labels# Using blue for males and red for females


##
library(GGally)
library(ggplot2)
library(Hmisc)
# Define the data you want to plot
data_to_plot <- filtered_data31[, c("DPHP", "BDCPP", "BCEP", "BCPP", "DBUP")]
# Calculate the correlation matrix
cor_matrix <- cor(data_to_plot, method = "spearman", use = "pairwise.complete.obs")

# Calculate the p-values using Hmisc::rcorr
cor_test <- rcorr(as.matrix(data_to_plot), type = "spearman")
p_matrix <- cor_test$P

# Create a function to get significance stars
signif_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  else if (p_value < .001) return("***")
  else if (p_value < .01) return("**")
  else if (p_value < .05) return("*")
  else return("")
}

# Combine the correlation values and significance stars for the upper triangle
cor_matrix_formatted <- cor_matrix
for (i in 1:nrow(cor_matrix)) {
  for (j in 1:ncol(cor_matrix)) {
    if (i < j) {
      cor_matrix_formatted[i, j] <- as.numeric(sprintf("%.2f%s", cor_matrix[i, j], signif_stars(p_matrix[i, j])))
    } else {
      cor_matrix_formatted[i, j] <- NA
    }
  }
}

# Function to plot the upper triangle with numbers
plot_upper <- function(corr, col = "black", ...) {
  text(corr, col = col, ...)
}

# Plot using corrplot
corrplot.mixed(
  cor_matrix,
  lower = "ellipse",
  upper = "number",
  tl.col = "black",
  number.cex = 1.5,
  col = colorRampPalette(c("blue", "white", "red"))(200),
  p.mat = p_matrix,
  sig.level = 0.05,
  insig = "label_sig",
  pch.cex = 1.5,
  pch.col = "black",
  diag = FALSE
)

