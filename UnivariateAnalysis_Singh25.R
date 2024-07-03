
getwd()
load("CompleteData25.RData")
#used to extract the object stored in this data set stored in the folder 
ls()
# use the names 
library(car)
names(combined.data25)
summary(combined.data22$BMXBMI)
summary(combined.data22$URXUCR)
summary(combined.data22)
str(combined.data22)
library(dplyr)
library(plyr)

#DEMOGRAPHIC COVARIATES
# Variable        Target population   
# "SEQN"         Both males and females 0 YEARS - 150 YEARS
# "RIAGENDR"     Both males and females 0 YEARS - 150 YEARS
# "RIDAGEYR"     Both males and females 0 YEARS - 150 YEARS
# "RIDRETH3"     Both males and females 0 YEARS - 150 YEARS
# "DMDMARTL"     Both males and females 20 YEARS - 150 YEARS
# "WTINT2YR"     Both males and females 0 YEARS - 150 YEARS
# "WTMEC2YR"     Both males and females 0 YEARS - 150 YEARS
# "SDMVPSU"      Both males and females 0 YEARS - 150 YEARS
# "SDMVSTRA"     Both males and females 0 YEARS - 150 YEARS
# "DMDEDUC2"     Both males and females 20 YEARS - 150 YEARS
# ""INDFMIN2"   Both males and females 20 YEARS - 150 YEARS
# "INDFMPIR"     Both males and females 0 YEARS - 150 YEARS
# DMDBORN4"      Both males and females 0 YEARS - 150 YEARS
#Examination Covariates
#"BMXBMI"        Both males and females 2 YEARS - 150 YEARS
#Questionnaire data
# "SMQ040"       Both males and females 18 YEARS - 150 YEAR
# "ALQ130"       Both males and females 18 YEARS - 150 YEARS
#"MCQ160A"      Both males and females 20 YEARS - 150 YEARS
#"MCQ195"      Both males and females 20 YEARS - 150 YEARS
#laboratory data
#"SSDPHP"   Both males and females 3 YEARS - 150 YEARS
# "SSBDCPP"  Both males and females 3 YEARS - 150 YEARS
#"SSBCPP"  Both males and females 3 YEARS - 150 YEARS
#"SSBCEP"  Both males and females 3 YEARS - 150 YEARS
#"SSDBUP" Both males and females 3 YEARS - 150 YEARS
#"URXUCR" Both males and females 6 YEARS - 150 YEARS

# 2: race
summary(combined.data25$RIDRETH3)
combined.data25$RIDRETH3<-recode(combined.data25$RIDRETH3,
                                 "c('Other Race - Including Multi-Rac','Other Hispanic')='Other race';
                                 c('Mexican American')='Mexican American';
                                 c('Non-Hispanic White')='Non-Hispanic White';
                                 c('Non-Hispanic Asian')='Non-Hispanic Asian';
                                  c('Non-Hispanic Black')='Non-Hispanic Black';
                                 else=NA")
summary(combined.data25$RIDRETH3)
# 3: Age
summary(combined.data25$RIDAGEYR)
combined.data25$age.cat <- cut(
  combined.data25$RIDAGEYR, 
  breaks = c(20, 60, Inf), 
  labels = c("20–60 years", "≥60 years"), 
  right = FALSE
)
combined.data25$RIDAGEYR<- NULL
table(combined.data25$age.cat)
# 4: Gender
summary(combined.data25$RIAGENDR)
# 5: Marital status
# before we modify let us different observation categories 
summary(combined.data25$DMDMARTL)
combined.data25$DMDMARTL<-recode(combined.data25$DMDMARTL,
                                 "c('Widowed', 'Divorced','Seperated')='Previously married';
                               c('Living with partner', 'Married')='Married';
                               'Never married'= 'Never married';
                               else=NA")
summary(combined.data25$DMDMARTL)
#6: EDUCATION
summary(combined.data25$DMDEDUC2)
combined.data25$DMDEDUC2<-recode(combined.data25$DMDEDUC2,
                                 "c('Less than 9th grade')='Under high school';
                                 c('9-11th grade (Includes 12th grad','High school graduate/GED or equi','Some college or AA degree','College graduate or above')='High school and above';
                                 else=NA")
summary(combined.data25$DMDEDUC2)
# 7: Country of Birth
summary(combined.data25$DMDBORN4)
combined.data25$DMDBORN4<-recode(combined.data25$DMDBORN4,
                                 "c('Born in 50 US states or Washingt') ='Citizen';
                                 c('Others')= 'Not a citizen';
                                  else=NA")
summary(combined.data25$DMDBORN4)
# 8: alcohol 
# before we modify let us different observation categories 
summary(combined.data25$ALQ101)

# 9: alcohol 
summary(combined.data25$SMQ040)
combined.data25$SMQ040<-recode(combined.data25$SMQ040,
                               "c('Every day') ='Every day';
                                 c('Some days')= 'Some days';
                                 c('Not at all')= 'Not at all';
                                  else=NA")
summary(combined.data25$SMQ040)

#10: Arthritis
summary(combined.data25$MCQ160A)
summary(combined.data25$MCQ195)
combined.data25$RA_status <- ifelse(combined.data25$MCQ160A == 'Yes' & combined.data25$MCQ195 == 'Rheumatoid arthritis', 'RA', 'Non-RA')
combined.data25$MCQ160A<- NULL
combined.data25$MCQ195<- NULL

#11 URINARY CRETINE
summary(combined.data25$URXUCR)
#12 diabetes
summary(combined.data25$DIQ010)
combined.data25$DIQ010<-recode(combined.data25$DIQ010,
                               "c(' Yes','Borderline') =' Yes';
                                 c('No')= 'No';
                                  else=NA")
summary(combined.data25$DIQ010)
#13
summary(combined.data25$PAD630)
summary(combined.data25$PAD660)
combined.data25$MVPA <- ifelse(combined.data25$PAD630 > 10 | combined.data25$PAD660 > 10, 1, 0)
summary(combined.data25$MVPA)
combined.data25$PAD630<- NULL
combined.data25$PAD660<- NULL
str(combined.data25)
filtered_participants <- combined.data25 %>%
  filter(RIDAGEYR >= 20)


## Renaming all the variables
names(combined.data25)
#[1] "SEQN"      "SSDPHP"    "SSBDCPP"   "SSBCPP"    "SSBCEP"    "SSDBUP"    "SSDBZP"    "SSTBBA"    "RIAGENDR"  "RIDAGEYR"  "RIDRETH3"  "DMDMARTL" 
#[13] "WTINT2YR"  "WTMEC2YR"  "SDMVPSU"   "SDMVSTRA"  "DMDEDUC2"  "DMDBORN4"  "INDFMPIR"  "BMXBMI"    "ALQ101"    "SMQ040"    "DIQ010"    "URXUCR"   
#[25] "age.cat"   "RA_status" "MVPA" 

names(combined.data25)<-c("Id","DPHP", "BDCPP", "BCPP", "BCEP","DBUP","DBZP","TBBA","Gender","Age","Ethnicity","Marital_status",
                          "w.all","w.MEC", "PSU","STRATA","Education","Citizenship","PIR","BMIkg/m2", "Alcohol", "Smoking","Diabetes",
                          "Urinary_creatinine", "Age_cat","RA_status","MVPA")


names(combined.data25)
str(combined.data25)
# filtering complete cases in RA_status
complete.data25c <- combined.data25[complete.cases(combined.data25$RA_status), ]
summary(complete.data25c)

#Regression Summary
save(complete.data25c, file= "NHANESanalytic25.Rdata")

library(DataExplorer)

plot_missing(complete.data25c)
names(complete.data24c)


#####
library("tableone")
vars=c( "Age_cat","Smoking","Ethnicity", "Gender", "Marital_status", 
        "Alcohol", "Education","RA_status","Citizenship","BMIkg/m2","Urinary_creatinine","Diabetes", "MVPA","PIR")
CreateTableOne(data=complete.data25c, includeNA = FALSE,
               vars=vars)
# complete case analysis


# imputed data 
library(mice)
library(dplyr)
str(combined.data21)

# Handling missing data for categorical variables by creating a missing indicator category
summary(complete.data25c$Citizenship)
complete.data25c$Citizenship <- as.character(complete.data25c$Citizenship)
complete.data25c$Citizenship[is.na(complete.data25c$Citizenship)] <- "Missing"
complete.data25c$Citizenship <- as.factor(complete.data25c$Citizenship)

summary(complete.data25c$Education)
complete.data25c$Education <- as.character(complete.data25c$Education)
complete.data25c$Education[is.na(complete.data25c$Education)] <- "Missing"
complete.data25c$Education <- as.factor(complete.data25c$Education)

summary(complete.data25c$Marital_status)
complete.data25c$Marital_status <- as.character(complete.data25c$Marital_status)
complete.data25c$Marital_status[is.na(complete.data25c$Marital_status)] <- "Missing"
complete.data25c$Marital_status <- as.factor(complete.data25c$Marital_status)

summary(complete.data25c$Alcohol)
complete.data25c$Alcohol <- as.character(complete.data25c$Alcohol)
complete.data25c$Alcohol[is.na(complete.data25c$Alcohol)] <- "Missing"
complete.data25c$Alcohol <- as.factor(complete.data25c$Alcohol)

summary(complete.data25c$Diabetes)
complete.data25c$Diabetes <- as.character(complete.data25c$Diabetes)
complete.data25c$Diabetes[is.na(complete.data25c$Diabetes)] <- "Missing"
complete.data25c$Diabetes <- as.factor(complete.data25c$Diabetes)

summary(complete.data25c$Smoking)
complete.data25c$Smoking <- as.character(complete.data25c$Smoking)
complete.data25c$Smoking[is.na(complete.data25c$Smoking)] <- "Missing"
complete.data25c$Smoking <- as.factor(complete.data25c$Smoking)

# Handling missing data for continuous variables by imputing with the median
complete.data25c$`BMIkg/m2`[is.na(complete.data25c$`BMIkg/m2`)] <- median(complete.data25c$`BMIkg/m2`, na.rm = TRUE)
complete.data25c$DPHP[is.na(complete.data25c$DPHP)] <- median(complete.data25c$DPHP, na.rm = TRUE)
complete.data25c$BDCPP[is.na(complete.data25c$BDCPP)] <- median(complete.data25c$BDCPP, na.rm = TRUE)
complete.data25c$BCPP[is.na(complete.data25c$BCPP)] <- median(complete.data25c$BCPP, na.rm = TRUE)
complete.data25c$BCEP[is.na(complete.data25c$BCEP)] <- median(complete.data25c$BCEP, na.rm = TRUE)
complete.data25c$DBUP[is.na(complete.data25c$DBUP)] <- median(complete.data25c$DBUP, na.rm = TRUE)
complete.data25c$DBZP[is.na(complete.data25c$DBZP)] <- median(complete.data25c$DBZP, na.rm = TRUE)
complete.data25c$TBBA[is.na(complete.data25c$TBBA)] <- median(complete.data25c$TBBA, na.rm = TRUE)
complete.data25c$Urinary_creatinine[is.na(complete.data25c$Urinary_creatinine)] <- median(complete.data25c$Urinary_creatinine, na.rm = TRUE)
complete.data25c$PIR[is.na(complete.data25c$PIR)] <- median(complete.data25c$PIR, na.rm = TRUE)
complete.data25c$MVPA[is.na(complete.data25c$MVPA)] <- median(complete.data25c$MVPA, na.rm = TRUE)



#combined.data21$income[is.na(combined.data21$income)] <- median(combined.data21$income, na.rm = TRUE)
combined.data21$urinarycreatinine[is.na(combined.data21$urinarycreatinine)] <- median(combined.data21$urinarycreatinine, na.rm = TRUE)
combined.data21$alcohol[is.na(combined.data21$alcohol)] <-median(combined.data21$alcohol,na.rm = TRUE)


# Number of iteration; mostly useful for convergence
analytic.data.imputation1<-complete(imputation1)
dim(analytic.data.imputation1)
str(analytic.data.imputation1)
plot_missing(analytic.data.imputation1)
plot_missing(combined.data21)
CreateTableOne(data=combined.data21, includeNA=TRUE,
               vars=vars)
# let us see the results after imputation 
fit23i<-glm(BCEP~gender+age+race+marital+smoke+alcohol+education, data=combined.data21)
library(knitr)
summary(fit23i)
publish(fit23i)
install.packages("rmarkdown")
library(rmarkdown)
library(jtools)
library(ggstance)
library(broom.mixed)
library(huxtable)
export_summs(fit13, fit23i)
plot_summs(fit13, fit23i)
install.packages("Hmisc")
library(Hmisc)
sel.name<-c("diastolic","gender","age.centred","race","marital","systolic","smoke","alcohol","education")
var.cluster<-varclus(~.,data=combined.data1[sel.name])
plot(var.cluster)

