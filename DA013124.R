# on 03/28/24 I cross checked my analysis 
#2011-2012
library(nhanesA)
library(knitr)
demo1<- nhanes('DEMO_G')# Both males and females 0 YEARS -150 YEARS
names(demo1)
# subsetting the useful variables, using the combine function 
demo7<- demo1[c("SEQN",# Respondent sequence number
                "RIAGENDR", # gender 
                "RIDAGEYR", # age in year at screening
                "RIDRETH3", # Race/Hispanic origin w/ NH Asian
                "DMDMARTL", # Marital status: 20 YEARS-150 YEARS
                "WTINT2YR", # # Full sample 2 year weights
                "WTMEC2YR",# Full sample 2 year weights
                "SDMVPSU", #Masked variance pseudo-PSU
                "SDMVSTRA",#Masked variance pseudo-stratum
                "DMDEDUC2",# Education level 20+
                "DMDBORN4", # Country of birth
                "INDFMPIR")]# ratio of family income to poverty
demo1_vars<- names(demo7)
#nhanestranslate converts the numeric inputs in the actual observation
#demo2 final subsetted data from demographic 
demo78<- nhanesTranslate('DEMO_G', demo1_vars, data=demo7)
head(demo78)
#bodymass index  from the examination data
#creating a new data frame to store the examination data
bmx1<-nhanes('BMX_G')
names(bmx1)
#subsetting the variables to be used for the study
bmx7<- bmx1[c("SEQN",#Respondents sequence number
              "BMXBMI")] # Body mass index(kg/m**2)

bmx1_vars<-names(bmx7)
bmx78<- nhanesTranslate('BMX_G', bmx1_vars, data=bmx7)
head(bmx78)
# now we as per the research questions we need smoking information
smq1<- nhanes('SMQ_G')
names(smq1)
smq7<- smq1[c("SEQN", # Respondant
              "SMQ040")]# Do you smoke cigarattes?: 18 YEARS-150 YEARS
smq1_vars<-names(smq7)
smq78<-nhanesTranslate('SMQ_G', smq1_vars, data=smq7)
head(smq78)
# alcohol data acquition 
alq1<- nhanes('ALQ_G')
alq7<- alq1[c("SEQN",# Respondent sequence number
              "ALQ101")]# Had at least 12 alcohol drinks/1 yr?
alq1_vars<-names(alq7)
alq78<-nhanesTranslate('ALQ_G', alq1_vars, data=alq7)
head(alq78)
# flame retardant metabolites total 9 metabolites
flrt1<-nhanes('SSFR_G')
flrt7<-flrt1[c("SEQN", #Respondent sequence number
               "SSDPHP",#Diphenyl phosphate (ug/L)
               "SSBDCPP",# Bis(1,3-dichloro-2-propyl) phospht(ug/L)
               "SSBCPP", # Bis(1-chloro-2-propyl) phosphate (ug/L)
               "SSBCEP", # Bis(2-chloroethyl) phosphate (ug/L)
               "SSDBUP",# Dibutyl phosphate (ug/L)
               "SSDBZP",#Dibenzyl phosphate (µg/L)
               "SSTBBA")]#2,3,4,5-tetrabromobenzoic acid (µg/L)
flrt1_vars<-names(flrt7)
flrt78<-nhanesTranslate('SSFR_G',flrt1_vars, data=flrt7)
head(flrt78)

##
mcq1<- nhanes('MCQ_G')
mcq7<- mcq1[c("SEQN",# Respondent sequence number
              "MCQ160A",# Has a doctor or other health professional ever told {you/SP} that {you/s/he} . . .had arthritis (ar-thry-tis)?
              "MCQ195")] # Which type of arthritis was it?
mcq1_vars<-names(mcq7)
mcq78<-nhanesTranslate('MCQ_G', mcq1_vars, data=mcq7)
head(mcq78)
##
diq1<- nhanes('DIQ_G')
diq7<-diq1[c("SEQN",#Respondent sequence number
             "DIQ010")]#Doctor told you have diabetes
diq1_vars<-names(diq7)
diq78<-nhanesTranslate('DIQ_G',diq1_vars, data=diq7)
head(diq78)
##
paq1<- nhanes('PAQ_G')
names(paq1)
paq7<-paq1[c("SEQN", #Respondent sequence number
             "PAQ605",# vigrous work activity
             "PAQ650",#vigorous recreational activities
             "PAQ620",# moderate work activity
             "PAQ665")]# moderate recreational activites
paq1_vars<-names(paq7)
paq78<-nhanesTranslate('PAQ_G',paq1_vars, data=paq7)
head(paq78)
##
albc1<-nhanes('ALB_CR_G')
albc7<-albc1[c("SEQN",#Respondent sequence number
               "URXUCR")] #Creatinine, urine (mg/dL)
albc1_vars<-names(albc7)
albc78<-nhanesTranslate('ALB_CR_G', albc1_vars, data=albc7)
head(albc78)
# merging all the data 
# to use the join_all we will need plyr package
library(plyr)
library(dplyr)
#demo2_filtered <- demo78 %>% filter(RIDAGEYR >= 20)
#summary(demo2_filtered1)
#analytic.data11_12<- join_all(list(demo78,bmx78,alq78,paq78,diq78,albc78,mcq78,flrt78,smq78), by = "SEQN", type='full')
#head(analytic.data11_12)
data11_12 <- inner_join(demo78, flrt78, by = "SEQN")
data11_12_complete <- data11_12[complete.cases(flrt78), ]
analytic.data11_12 <- data11_12_complete %>%
  left_join(albc78,by = "SEQN")%>%
  left_join(alq78,by = "SEQN")%>%
  left_join(bmx78,by = "SEQN")%>%
  left_join(diq78,by = "SEQN")%>%
  left_join(paq78,by = "SEQN")%>%
  left_join(smq78, by = "SEQN")%>%
  left_join(mcq78, by = "SEQN")
head(analytic.data11_12)
str(analytic.data11_12)
#2013-2014
demo2<- nhanes('DEMO_H')# Both males and females 0 YEARS -150 YEARS
# subsetting the useful variables, using the combine function 
demo13_14<- demo2[c("SEQN",# Respondent sequence number
                    "RIAGENDR", # gender 
                    "RIDAGEYR", # age in year at screening
                    "RIDRETH3", # Race/Hispanic origin w/ NH Asian
                    "DMDMARTL", # Marital status: 20 YEARS-150 YEARS
                    "WTINT2YR", # # Full sample 2 year weights
                    "WTMEC2YR",# Full sample 2 year weights
                    "SDMVPSU", #Masked variance pseudo-PSU
                    "SDMVSTRA",#Masked variance pseudo-stratum
                    "DMDEDUC2",# Education level 20+
                    "DMDBORN4", # Country of birth
                    "INDFMPIR")]# ratio of family income to poverty
demoh_vars<- names(demo13_14)
#nhanestranslate converts the numeric inputs in the actual observation
#demo2 final subsetted data from demographic 
demo34<- nhanesTranslate('DEMO_H', demoh_vars, data=demo13_14)
head(demo34)
#bodymass index  from the examination data
#creating a new data frame to store the examination data
bmx2<-nhanes('BMX_H')
#subsetting the variables to be used for the study
bmx13_14<- bmx2[c("SEQN",#Respondents sequence number
                  "BMXBMI")] # Body mass index(kg/m**2)
bmxh_vars<-names(bmx13_14)
bmx34<- nhanesTranslate('BMX_H', bmxh_vars, data=bmx13_14)
head(bmx34)
# now we as per the research questions we need smoking information
smq2<- nhanes('SMQ_H')
names(smq2)
smq13_14<- smq2[c("SEQN", # Respondant
                  "SMQ040")]# Do you smoke cigarattes?: 18 YEARS-150 YEARS
smqh_vars<-names(smq13_14)
smq34<-nhanesTranslate('SMQ_H', smqh_vars, data=smq13_14)
head(smq34)
# alcohol data acquition 
alq2<- nhanes('ALQ_H')
alq13_14<- alq2[c("SEQN",# Respondent sequence number
                  "ALQ101")]# Had at least 12 alcohol drinks/1 yr?
alqh_vars<-names(alq13_14)
alq34<-nhanesTranslate('ALQ_H', alqh_vars, data=alq13_14)
head(alq34)
# flame retardant metabolites total 9 metabolites
flrt2<-nhanes('SSFLRT_H')
flrt13_14<-flrt2[c("SEQN", #Respondent sequence number
                   "SSDPHP",#Diphenyl phosphate (ug/L)
                   "SSBDCPP",# Bis(1,3-dichloro-2-propyl) phospht(ug/L)
                   "SSBCPP", # Bis(1-chloro-2-propyl) phosphate (ug/L)
                   "SSBCEP", # Bis(2-chloroethyl) phosphate (ug/L)
                   "SSDBUP",# Dibutyl phosphate (ug/L)
                   "SSDBZP",#Dibenzyl phosphate (ÃÂµg/L)
                   "SSTBBA")]#2,3,4,5-tetrabromobenzoic acid (ÃÂµg/L)
flrth_vars<-names(flrt13_14)
flrt34<-nhanesTranslate('SSFLRT_H',flrth_vars, data=flrt13_14)
head(flrt34)
##
mcq2<- nhanes('MCQ_H')
mcq13_14<- mcq2[c("SEQN",# Respondent sequence number
                  "MCQ160A",# Has a doctor or other health professional ever told {you/SP} that {you/s/he} . . .had arthritis (ar-thry-tis)?
                  "MCQ195")] # Which type of arthritis was it?
mcqh_vars<-names(mcq13_14)
mcq34<-nhanesTranslate('MCQ_H', mcqh_vars, data=mcq13_14)
head(mcq34)
##
diq2<- nhanes('DIQ_H')
diq13_14<-diq2[c("SEQN",#Respondent sequence number
                 "DIQ010")]#Doctor told you have diabetes
diqh_vars<-names(diq13_14)
diq34<-nhanesTranslate('DIQ_H',diqh_vars, data=diq13_14)
head(diq34)
##
paq2<- nhanes('PAQ_H')
paq13_14<-paq2[c("SEQN", #Respondent sequence number
             "PAQ605",# vigrous work activity
             "PAQ650",#vigorous recreational activities
             "PAQ620",# moderate work activity
             "PAQ665")]# moderate recreational activites
paqh_vars<-names(paq13_14)
paq34<-nhanesTranslate('PAQ_H',paqh_vars, data=paq13_14)
head(paq34)
##
albc2<-nhanes('ALB_CR_H')
albc13_14<-albc2[c("SEQN",#Respondent sequence number
                   "URXUCR")] #Creatinine, urine (mg/dL)
albch_vars<-names(albc13_14)
albc34<-nhanesTranslate('ALB_CR_H', albch_vars, data=albc13_14)
head(albc34)
#will comtinue from here
# merging all the data 
# to use the join_all we will need plyr package
#analytic.data13_14<- join_all(list(demo34, bmx34,alq34,paq34,diq34,albc34,mcq34,flrt34,smq34), by = "SEQN", type='full')
#summary(analytic.data13_14)
#head(analytic.data13_14)
#str(analytic.data13_14)
data13_14 <- inner_join(demo34, flrt34, by = "SEQN")
data13_14_complete <- data13_14[complete.cases(flrt34), ]
analytic.data13_14 <- data13_14_complete %>%
  left_join(albc34,by = "SEQN")%>%
  left_join(alq34,by = "SEQN")%>%
  left_join(bmx34,by = "SEQN")%>%
  left_join(diq34,by = "SEQN")%>%
  left_join(paq34,by = "SEQN")%>%
  left_join(smq34, by = "SEQN")%>%
  left_join(mcq34, by = "SEQN")
head(analytic.data13_14)
str(analytic.data13_14)
## 2015-2016
demo3<- nhanes('DEMO_I')# Both males and females 0 YEARS -150 YEARS
# subsetting the useful variables, using the combine function 
demo15_16<- demo3[c("SEQN",# Respondent sequence number
                    "RIAGENDR", # gender 
                    "RIDAGEYR", # age in year at screening
                    "RIDRETH3", # Race/Hispanic origin w/ NH Asian
                    "DMDMARTL", # Marital status: 20 YEARS-150 YEARS
                    "WTINT2YR", # # Full sample 2 year weights
                    "WTMEC2YR",# Full sample 2 year weights
                    "SDMVPSU", #Masked variance pseudo-PSU
                    "SDMVSTRA",#Masked variance pseudo-stratum
                    "DMDEDUC2",# Education level 20+
                    "DMDBORN4", # Country of birth
                    "INDFMPIR")]# ratio of family income to poverty
demoi_vars<- names(demo15_16)
#nhanestranslate converts the numeric inputs in the actual observation
#demo2 final subsetted data from demographic 
demo56<- nhanesTranslate('DEMO_I', demoi_vars, data=demo15_16)
head(demo56)
#bodymass index  from the examination data
#creating a new data frame to store the examination data
bmx3<-nhanes('BMX_I')
#subsetting the variables to be used for the study
bmx15_16<- bmx3[c("SEQN",#Respondents sequence number
                  "BMXBMI")] # Body mass index(kg/m**2)
bmxi_vars<-names(bmx15_16)
bmx56<- nhanesTranslate('BMX_I', bmxi_vars, data=bmx15_16)
head(bmx56)
# now we as per the research questions we need smoking information
smq3<- nhanes('SMQ_I')
names(smq3)
smq15_16<- smq3[c("SEQN", # Respondant
                  "SMQ040")]# Do you smoke cigarattes?: 18 YEARS-150 YEARS
smqi_vars<-names(smq15_16)
smq56<-nhanesTranslate('SMQ_I', smqi_vars, data=smq15_16)
head(smq56)
# alcohol data acquition 
alq3<- nhanes('ALQ_I')
alq15_16<- alq3[c("SEQN",# Respondent sequence number
                  "ALQ101")]# Had at least 12 alcohol drinks/1 yr?
alqi_vars<-names(alq15_16)
alq56<-nhanesTranslate('ALQ_I', alqi_vars, data=alq15_16)
head(alq56)
# flame retardant metabolites total 9 metabolites
flrt3<-nhanes('SSFR_I')
flrt15_16<-flrt3[c("SEQN", #Respondent sequence number
                   "SSDPHP",#Diphenyl phosphate (ug/L)
                   "SSBDCPP",# Bis(1,3-dichloro-2-propyl) phospht(ug/L)
                   "SSBCPP", # Bis(1-chloro-2-propyl) phosphate (ug/L)
                   "SSBCEP", # Bis(2-chloroethyl) phosphate (ug/L)
                   "SSDBUP",# Dibutyl phosphate (ug/L)
                   "SSDBZP",#Dibenzyl phosphate (ÃÂµg/L)
                   "SSTBBA")]#2,3,4,5-tetrabromobenzoic acid (ÃÂµg/L)
flrti_vars<-names(flrt15_16)
flrt56<-nhanesTranslate('SSFR_I',flrti_vars, data=flrt15_16)
head(flrt56)
##
mcq3<- nhanes('MCQ_I')
mcq15_16<- mcq3[c("SEQN",# Respondent sequence number
                  "MCQ160A",# Has a doctor or other health professional ever told {you/SP} that {you/s/he} . . .had arthritis (ar-thry-tis)?
                  "MCQ195")] # Which type of arthritis was it?
mcqi_vars<-names(mcq15_16)
mcq56<-nhanesTranslate('MCQ_I', mcqi_vars, data=mcq15_16)
head(mcq56)
##
diq3<- nhanes('DIQ_I')
diq15_16<-diq3[c("SEQN",#Respondent sequence number
                 "DIQ010")]#Doctor told you have diabetes
diqi_vars<-names(diq15_16)
diq56<-nhanesTranslate('DIQ_I',diqi_vars, data=diq15_16)
head(diq56)
##
paq3<- nhanes('PAQ_I')
paq15_16<-paq2[c("SEQN", #Respondent sequence number
                 "PAQ605",# vigrous work activity
                 "PAQ650",#vigorous recreational activities
                 "PAQ620",# moderate work activity
                 "PAQ665")]# moderate recreational activites
paqi_vars<-names(paq15_16)
paq56<-nhanesTranslate('PAQ_I',paqi_vars, data=paq15_16)
head(paq56)
##
albc3<-nhanes('ALB_CR_I')
albc15_16<-albc3[c("SEQN",#Respondent sequence number
                   "URXUCR")] #Creatinine, urine (mg/dL)
albci_vars<-names(albc15_16)
albc56<-nhanesTranslate('ALB_CR_I', albci_vars, data=albc15_16)
head(albc56)
#will continue from here
# merging all the data 
# to use the join_all we will need plyr package
#analytic.data15_16<- join_all(list(demo56, bmx56,alq56,paq56,diq56,albc56,mcq56,flrt56,smq56), by = "SEQN", type='full')
data15_16 <- inner_join(demo56,flrt56, by = "SEQN")
data15_16_complete <- data15_16[complete.cases(flrt56), ]
analytic.data15_16 <- data15_16_complete %>%
  left_join(albc56,by = "SEQN")%>%
  left_join(alq56,by = "SEQN")%>%
  left_join(bmx56,by = "SEQN")%>%
  left_join(diq56,by = "SEQN")%>%
  left_join(paq56,by = "SEQN")%>%
  left_join(smq56, by = "SEQN")%>%
  left_join(mcq56, by = "SEQN")
head(analytic.data15_16)
str(analytic.data15_16)
## 2017-2018
demo4<- nhanes('DEMO_J')# Both males and females 0 YEARS -150 YEARS
# subsetting the useful variables, using the combine function 
demo17_18<- demo4[c("SEQN",# Respondent sequence number
                    "RIAGENDR", # gender 
                    "RIDAGEYR", # age in year at screening
                    "RIDRETH3", # Race/Hispanic origin w/ NH Asian
                    "DMDMARTL", # Marital status: 20 YEARS-150 YEARS
                    "WTINT2YR", # # Full sample 2 year weights
                    "WTMEC2YR",# Full sample 2 year weights
                    "SDMVPSU", #Masked variance pseudo-PSU
                    "SDMVSTRA",#Masked variance pseudo-stratum
                    "DMDEDUC2",# Education level 20+
                    "DMDBORN4", # Country of birth
                    "INDFMPIR")]# ratio of family income to poverty
demoj_vars<- names(demo17_18)
#nhanestranslate converts the numeric inputs in the actual observation
#demo2 final subsetted data from demographic 
demoUX<- nhanesTranslate('DEMO_J', demoj_vars, data=demo17_18)
head(demoUX)
#bodymass index  from the examination data
#creating a new data frame to store the examination data
bmx4<-nhanes('BMX_J')
#subsetting the variables to be used for the study
bmx17_18<- bmx4[c("SEQN",#Respondents sequence number
                  "BMXBMI")] # Body mass index(kg/m**2)
bmxj_vars<-names(bmx17_18)
bmxUX<- nhanesTranslate('BMX_J', bmxj_vars, data=bmx17_18)
head(bmxUX)
# now we as per the research questions we need smoking information
smq4<- nhanes('SMQ_J')
smq17_18<- smq4[c("SEQN", # Respondant
                  "SMQ040")]# Do you smoke cigarattes?: 18 YEARS-150 YEARS
smqj_vars<-names(smq17_18)
smqUX<-nhanesTranslate('SMQ_J', smqj_vars, data=smq17_18)
head(smqUX)
# alcohol data acquition 
alq4<- nhanes('ALQ_J')
alq17_18<- alq4[c("SEQN",# Respondent sequence number
                  "ALQ111")]# Had at least 12 alcohol drinks/1 yr?
alqj_vars<-names(alq17_18)
alqUX<-nhanesTranslate('ALQ_J', alqj_vars, data=alq17_18)
head(alqUX)
names(alqUX)[names(alqUX) == "ALQ111"] <- "ALQ101"
# flame retardant metabolites total 9 metabolites
flrt4<-nhanes('FR_J')
flrt17_18<-flrt4[c("SEQN", #Respondent sequence number
               "URXDPHP",#Diphenyl phosphate (ug/L)
               "URXBDCP",# Bis(1,3-dichloro-2-propyl) phospht(ug/L)
               "URXBCPP", # Bis(1-chloro-2-propyl) phosphate (ug/L)
               "URXBCEP", # Bis(2-chloroethyl) phosphate (ug/L)
               "URXDBUP",# Dibutyl phosphate (ug/L)
               "URXTBBA")]#2,3,4,5-tetrabromobenzoic acid (Âµg/L)
flrtj_vars<-names(flrt17_18)
flrtUX<-nhanesTranslate('FR_J',flrtj_vars, data=flrt17_18)
head(flrtUX)
##
# To rename "URXDPHP" to "SSDPHP" and so on for the other variables
names(flrtUX)[names(flrtUX) == "URXDPHP"] <- "SSDPHP"
names(flrtUX)[names(flrtUX) == "URXBDCP"] <- "SSBDCPP"
names(flrtUX)[names(flrtUX) == "URXBCPP"] <- "SSBCPP"
names(flrtUX)[names(flrtUX) == "URXBCEP"] <- "SSBCEP"
names(flrtUX)[names(flrtUX) == "URXDBUP"] <- "SSDBUP"
names(flrtUX)[names(flrtUX) == "URXTBBA"] <- "SSTBBA"

##
mcq4<- nhanes('MCQ_J')
mcq17_18<- mcq4[c("SEQN",# Respondent sequence number
                  "MCQ160A",# Has a doctor or other health professional ever told {you/SP} that {you/s/he} . . .had arthritis (ar-thry-tis)?
                  "MCQ195")] # Which type of arthritis was it?
mcqj_vars<-names(mcq17_18)
mcqUX<-nhanesTranslate('MCQ_J', mcqj_vars, data=mcq17_18)
head(mcqUX)
##
diq4<- nhanes('DIQ_J')
diq17_18<-diq4[c("SEQN",#Respondent sequence number
                 "DIQ010")]#Doctor told you have diabetes
diqj_vars<-names(diq17_18)
diqUX<-nhanesTranslate('DIQ_J',diqj_vars, data=diq17_18)
head(diqUX)
##
paq4<- nhanes('PAQ_J')
paq17_18<-paq4[c("SEQN", #Respondent sequence number
                 "PAQ605",# vigrous work activity
                 "PAQ650",#vigorous recreational activities
                 "PAQ620",# moderate work activity
                 "PAQ665")]# moderate recreational activites
paqj_vars<-names(paq17_18)
paqUX<-nhanesTranslate('PAQ_J',paqj_vars, data=paq17_18)
head(paqUX)
##
albc4<-nhanes('ALB_CR_J')
albc17_18<-albc4[c("SEQN",#Respondent sequence number
                   "URXUCR")] #Creatinine, urine (mg/dL)
albcj_vars<-names(albc17_18)
albcUX<-nhanesTranslate('ALB_CR_J', albcj_vars, data=albc17_18)
head(albcUX)
#will comtinue from here
# merging all the data 
# to use the join_all we will need plyr package
#analytic.data15_16<- join_all(list(demo56, bmx56,alq56,paq56,diq56,albc56,mcq56,flrt56,smq56), by = "SEQN", type='full')
data17_18 <- inner_join(demoUX,flrtUX, by = "SEQN")
data17_18_complete <- data17_18[complete.cases(flrtUX), ]
analytic.data17_18 <- data17_18_complete %>%
  left_join(albcUX,by = "SEQN")%>%
  left_join(alqUX,by = "SEQN")%>%
  left_join(bmxUX,by = "SEQN")%>%
  left_join(diqUX,by = "SEQN")%>%
  left_join(paqUX,by = "SEQN")%>%
  left_join(smqUX, by = "SEQN")%>%
  left_join(mcqUX, by = "SEQN")
head(analytic.data17_18)
str(analytic.data17_18)

#head(analytic.data15_16)
#str(analytic.data15_16)
data.acquisition31<-join_all(list(analytic.data11_12,analytic.data13_14,analytic.data15_16,analytic.data17_18), by= "SEQN", type= 'full')
summary(data.acquisition31)
getwd()
library(dplyr)
filtered_data31 <- data.acquisition31 %>%
  filter(RIDAGEYR >= 20)
summary(filtered_data31)
save(filtered_data31, file="Datafiltered31.RData")
# Complete with 4 cycles



