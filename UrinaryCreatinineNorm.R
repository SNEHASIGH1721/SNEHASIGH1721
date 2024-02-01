getwd()
load("Datafiltered31NEHA.RData")
ls()


# Now we will cacluate creatnine 
#1:DPHP
# Assuming 'Urinary_Creatinine' is in mg/dL, convert it to g/L for consistency
filtered_data31$Urinary_Creatinine_g_L <- filtered_data31$Urinary_creatinine / 100
# Calculate the DPHP concentration normalized to creatinine levels (μg/g creatinine)
filtered_data31$DPHP_ug_g <- (filtered_data31$DPHP/ filtered_data31$Urinary_Creatinine_g_L) * 1000 # Multiply by 1000 to convert from μg/L to μg/g
#2:BDCPP

filtered_data31$BDCPP_ug_g <- (filtered_data31$BDCPP/ filtered_data31$Urinary_Creatinine_g_L) * 1000 # Multiply by 1000 to convert from μg/L to μg/g
#3BCPP

filtered_data31$BCPP_ug_g <- (filtered_data31$BCPP/ filtered_data31$Urinary_Creatinine_g_L) * 1000 # Multiply by 1000 to convert from μg/L to μg/g
#4BCEP

filtered_data31$BCEP_ug_g <- (filtered_data31$BCEP/ filtered_data31$Urinary_Creatinine_g_L) * 1000 
#5DBUP

filtered_data31$DBUP_ug_g <- (filtered_data31$DBUP/ filtered_data31$Urinary_Creatinine_g_L) * 1000 
#6

filtered_data31$TBBA_ug_g <- (filtered_data31$TBBA/ filtered_data31$Urinary_Creatinine_g_L) * 1000 
head(filtered_data31)