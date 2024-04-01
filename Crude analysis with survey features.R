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
#log_DPHP
formula1<- as.formula(I(RA_status_binary)~log_DPHP+Age.cat+Gender+Ethnicity)
fit<-svyglm(formula1,
            design = svy_design,
            family = binomial(link="logit"))
publish(fit)
formula2<- as.formula(I(RA_status_binary)~log_DPHP+Age.cat+Gender+Ethnicity+Marital_status+Citizenship+Alcohol+BMI+Diabetes+Smoking+Activity_level+PIR+Urinary_Creatinine_ug_L)
fit1.2<-svyglm(formula2,
             design = svy_design,
             family = binomial(link="logit"))
publish(fit2)
export_summs(fit,fit1.2)
plot_summs(fit,fit1.2)
#log_BDCPP
formula1<- as.formula(I(RA_status_binary)~log_BDCPP+Age.cat+Gender+Ethnicity)
fit2<-svyglm(formula1,
            design = svy_design,
            family = binomial(link="logit"))
publish(fit2)
formula2<- as.formula(I(RA_status_binary)~log_BDCPP+Age.cat+Gender+Ethnicity+Marital_status+Citizenship+Alcohol+BMI+Diabetes+Smoking+Activity_level+PIR+Urinary_Creatinine_ug_L)
fit2.1<-svyglm(formula2,
               design = svy_design,
               family = binomial(link="logit"))
publish(fit2.1)
export_summs(fit2,fit2.1)
plot_summs(fit2,fit2.1)
#log_BCPP
formula1<- as.formula(I(RA_status_binary)~log_BCPP+Age.cat+Gender+Ethnicity)
fit3<-svyglm(formula1,
             design = svy_design,
             family = binomial(link="logit"))
publish(fit3)
formula2<- as.formula(I(RA_status_binary)~log_BCPP+Age.cat+Gender+Ethnicity+Marital_status+Citizenship+Alcohol+BMI+Diabetes+Smoking+Activity_level+PIR+Urinary_Creatinine_ug_L)
fit3.1<-svyglm(formula2,
               design = svy_design,
               family = binomial(link="logit"))
publish(fit3.1)
export_summs(fit3,fit3.1)
plot_summs(fit3,fit3.1)
#log_BCEP
formula1<- as.formula(I(RA_status_binary)~log_BCEP+Age.cat+Gender+Ethnicity)
fit4<-svyglm(formula1,
             design = svy_design,
             family = binomial(link="logit"))
publish(fit2)
formula2<- as.formula(I(RA_status_binary)~log_BCEP+Age.cat+Gender+Ethnicity+Marital_status+Citizenship+Alcohol+BMI+Diabetes+Smoking+Activity_level+PIR+Urinary_Creatinine_ug_L)
fit4.1<-svyglm(formula2,
               design = svy_design,
               family = binomial(link="logit"))
publish(fit4.1)
export_summs(fit4,fit4.1)
plot_summs(fit4,fit4.1)
#log_DBUP
formula1<- as.formula(I(RA_status_binary)~log_DBUP+Age.cat+Gender+Ethnicity)
fit5<-svyglm(formula1,
             design = svy_design,
             family = binomial(link="logit"))
publish(fit5)
formula2<- as.formula(I(RA_status_binary)~log_DBUP+Age.cat+Gender+Ethnicity+Marital_status+Citizenship+Alcohol+BMI+Diabetes+Smoking+Activity_level+PIR+Urinary_Creatinine_ug_L)
fit5.1<-svyglm(formula2,
               design = svy_design,
               family = binomial(link="logit"))
publish(fit5.1)
export_summs(fit5,fit5.1)
plot_summs(fit5,fit5.1)

plot_summs(fit,fit2,fit3,fit4, fit5)
plot_summs(fit1.2,fit2.1,fit3.1,fit4.1,fit5.1)






