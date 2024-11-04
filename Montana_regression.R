# Load packages
library(pacman)
p_load(dplyr, lubridate, tidyr, magrittr, did, fixest)

# Load data
maindir <- "C:/Users/vando/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation"
setwd(maindir)
load("df_final.RData")
load("df_final_mn.RData")

# Filter for Montana
df_MT_wb <- df_final %>% filter(StateCd=="30") # waterbody level data
df_MT_mn <- df_final_mn %>% filter(StateCd=="30") # monitoring station level data


# Waterbody level analysis (fixed effects: waterbody + year)


      # Model 1: Covariates - Headstream; cluster - HUCEgDC
      model1 <- feols(yearlyPlevel ~ Tstatus + Headstream + y_ppt + y_ppt2  + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf  | UniqueID  + Year, data = df_MT_wb, cluster="HUCEgDC" )
      print(summary(model1))
      
      # Model 2: Covariates - Headstream; cluster - HUCE6
      model2 <- feols(yearlyPlevel ~ Tstatus + Headstream + y_ppt + y_ppt2 + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf | UniqueID + Year, data = df_MT_wb, cluster="HUCE6" )
      print(summary(model2))
      
      # Model 3: Covariates - Headstream; cluster - HUCE4
      model3 <- feols(yearlyPlevel ~ Tstatus + Headstream + y_ppt + y_ppt2 + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | UniqueID + Year, data = df_MT_wb, cluster="HUCE4" )
      print(summary(model3))

      # Model 4: Covariates - Headstream + one-year P level lag; cluster - HUCEgDC
      df_MT_1lag <- df_MT_wb  %>% filter(lag == 1 | is.na(lag)) # Filter for one-year lags or first periods
      model4 <- feols(yearlyPlevel ~ Tstatus + Headstream + Plevel_lag + y_ppt + y_ppt2 + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | UniqueID + Year, data = df_MT_1lag, cluster="HUCEgDC" )
      print(summary(model4))
      
      # Model 5: Covariates - Headstream + one-year P level lag; cluster - HUCE6
      model5 <- feols(yearlyPlevel ~ Tstatus + Headstream + Plevel_lag + y_ppt + y_ppt2 + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | UniqueID + Year, data = df_MT_1lag, cluster="HUCE6" )
      print(summary(model5))
      
      # Model 6: Covariates - Headstream + one-year P level lag; cluster - HUCE4
      model6 <- feols(yearlyPlevel ~ Tstatus + Headstream + Plevel_lag + y_ppt + y_ppt2 + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | UniqueID + Year, data = df_MT_1lag, cluster="HUCE4" )
      print(summary(model6))
      
      
      
      
# Monitoring station analysis (fixed effects: monitoring station + year)
# In all models, the dummy variable Headstream has been removed due to collinearity
      
      # Model 1: Covariates - Headstream; cluster - HUCEgDC
      model1 <- feols(yrlyPlv ~ Tstatus  + Headstream + ppt + ppt2  + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf  | ID + Year, data = df_MT_mn, cluster="HUCEgDC" )
      summary(model1)
      
      # Model 2: Covariates - Headstream; cluster - HUCE6
      model2 <- feols(yrlyPlv ~ Tstatus  + Headstream + ppt + ppt2  + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | ID + Year, data = df_MT_mn,  cluster="HUCE6" )
      summary(model2)
      
      # Model 3: Covariates - Headstream; cluster - HUCE4
      model3 <- feols(yrlyPlv ~ Tstatus  + Headstream + ppt + ppt2  + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | ID + Year, data = df_MT_mn,  cluster="HUCE4" )
      summary(model3)
      
      # Model 4: Covariates - Headstream + one-year P level lag; cluster - HUCEgDC
      df_MT_1lag_mn <- df_MT_mn  %>% filter(lag == 1 | is.na(lag)) # Filter for one-year lags or first periods
      model4 <- feols(yrlyPlv ~ Tstatus  + Headstream + Plvl_lg + ppt + ppt2  + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | ID + Year, data = df_MT_1lag_mn,  cluster="HUCEgDC" )
      summary(model4)
      
      # Model 5: Covariates - Headstream + one-year P level lag; cluster - HUCE6
      model5 <- feols(yrlyPlv ~ Tstatus + Headstream + Plvl_lg + Headstream + ppt + ppt2  + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | ID + Year, data = df_MT_1lag_mn,  cluster="HUCE6" )
      summary(model5)
      
      # Model 6: Covariates - Headstream + one-year P level lag; cluster - HUCE4
      model6 <- feols(yrlyPlv ~ Tstatus + Headstream + Plvl_lg + Headstream + ppt + ppt2  + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | ID + Year, data = df_MT_1lag_mn, cluster="HUCE4" )
      summary(model6)
      
# Summary statistics
summary(df_MT_wb)
summary(df_MT_mn)
table(df_MT_mn$Treated, df_MT_mn$Headstream)
#     Headstream
    #     0   1 
    # 0 686 144
    # 1 934 254
# Treated