# Intensity model

# Select data
df_final %<>% mutate(Intensity = ifelse(Treated == 0, NA, Intensity)) # Make sure you run this!

df_final_AZ <- df_final %>% filter(StateCd=="04")
df_final_CA <- df_final %>% filter(StateCd=="06")
df_final_FL <- df_final %>% filter(StateCd=="12")
df_final_HI <- df_final %>% filter(StateCd=="15")
df_final_MT <- df_final %>% filter(StateCd=="30")
df_final_NV <- df_final %>% filter(StateCd=="32")
df_final_OK <- df_final %>% filter(StateCd=="40")
df_final_UT <- df_final %>% filter(StateCd=="49")
df_final_WI <- df_final %>% filter(StateCd=="55")

WB_final <- list(df_final_AZ, df_final_CA, df_final_FL, df_final_MT, df_final_NV, df_final_OK, df_final_UT, df_final_WI)

names(WB_final) <- c("AZ", "CA", "FL", "MT", "NV", "OK", "UT", "WI")

# Missing values
for (name in names(WB_final)) {
  df <- WB_final[[name]]
  missing <- df %>%
    distinct(UniqueID, .keep_all = TRUE) %>%
    summarize(missing_intensity = sum(Treated == 1 & Intensity == 0))
  
  print(paste("Missing intensity for", name, ":", missing$missing_intensity))
}

# [1] "Missing intensity for AZ : 0"
# [1] "Missing intensity for CA : 36"
# [1] "Missing intensity for FL : 2"
# [1] "Missing intensity for MT : 152"
# [1] "Missing intensity for NV : 5"
# [1] "Missing intensity for OK : 0"
# [1] "Missing intensity for UT : 0"
# [1] "Missing intensity for WI : 496"

for (name in names(WB_final)) {
  df <- WB_final[[name]]
  
  # Model 1: FE - Year^StateCd + UniqueID; cluster - HUCEgDC
  print(paste("Processing data frame for:", name))
  model1 <- feols(yrlyPlv ~ Tstatus*Intensity + y_ppt + y_ppt2  + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf  | UniqueID  + Year, data = df, cluster="HUCEgDC" )
  print(summary(model1))
  
  # Model 2: FE - Year^StateCd ; cluster - HUCEgDC
  print(paste("Processing data frame for:", name))
  model2 <- feols(yrlyPlv ~ Tstatus*Intensity + y_ppt + y_ppt2 + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf | UniqueID + Year, data = df, cluster="HUCE6" )
  print(summary(model2))
  
  # Model 3: FE - Year^StateCd ; cluster - HUCE6
  print(paste("Processing data frame for:", name))
  model3 <- feols(yrlyPlv ~ Tstatus*Intensity + y_ppt + y_ppt2 + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | UniqueID + Year, data = df, cluster="HUCE4" )
  print(summary(model3))
  
}
