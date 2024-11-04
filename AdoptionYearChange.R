df_AZ_adoptionyear <- df_final_AZ %>% mutate(AdoptionYear= AdoptionYear+27)
df_AZ_adoptionyear %<>%
  mutate(Tstatus = case_when(
    Treated == 1 & AdoptionYear <= Year ~ 1,  # Assign 1 if treated and AdoptionDate is on or before Year
    Treated == 1 & AdoptionYear > Year | Treated == 0 ~ 0  # Assign 0 if treated but AdoptionDate is after Year or if not treated
  ))

feols(yrlyPlv ~ Tstatus + above_avg_days + rainydays + ppt + ppt2  + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf  | Year + ID, data = df_AZ_adoptionyear, cluster="HUCEgDC" )
table(df_AZ_adoptionyear$AdoptionYear) # 2004 low 2014~ up


df_MT_adoptionyear <- df_final_MT %>% mutate(AdoptionYear= AdoptionYear+6)
table(df_MT_adoptionyear$AdoptionYear) # 2003-7 (better) 2008 (draft) - 2013 (got worse) 2014 (official)
df_MT_adoptionyear %<>%
  mutate(Tstatus = case_when(
    Treated == 1 & AdoptionYear <= Year ~ 1,  # Assign 1 if treated and AdoptionDate is on or before Year
    Treated == 1 & AdoptionYear > Year | Treated == 0 ~ 0  # Assign 0 if treated but AdoptionDate is after Year or if not treated
  ))
feols(yrlyPlv ~ Tstatus + above_avg_days + rainydays + ppt + ppt2  + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf  | Year + ID, data = df_MT_adoptionyear, cluster="HUCEgDC" )


df_NV_adoptionyear <- df_final_NV %>% mutate(AdoptionYear= AdoptionYear+19)
table(df_NV_adoptionyear$AdoptionYear) # 1994 (none) 1995 (worse) - 1998 (official) - 2000 (worse) non 2006 (better)
df_NV_adoptionyear %<>%
  mutate(Tstatus = case_when(
    Treated == 1 & AdoptionYear <= Year ~ 1,  # Assign 1 if treated and AdoptionDate is on or before Year
    Treated == 1 & AdoptionYear > Year | Treated == 0 ~ 0  # Assign 0 if treated but AdoptionDate is after Year or if not treated
  ))
feols(yrlyPlv ~ Tstatus + above_avg_days + rainydays + ppt + ppt2  + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf  | Year + ID, data = df_NV_adoptionyear, cluster="HUCEgDC" )


df_WI_adoptionyear <- df_final_WI %>% mutate(AdoptionYear= AdoptionYear+0)
table(df_WI_adoptionyear$AdoptionYear) # 2010
df_WI_adoptionyear %<>%
  mutate(Tstatus = case_when(
    Treated == 1 & AdoptionYear <= Year ~ 1,  # Assign 1 if treated and AdoptionDate is on or before Year
    Treated == 1 & AdoptionYear > Year | Treated == 0 ~ 0  # Assign 0 if treated but AdoptionDate is after Year or if not treated
  ))
feols(yrlyPlv ~ Tstatus + above_avg_days + rainydays + ppt + ppt2  + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf  | Year + ID, data = df_WI_adoptionyear, cluster="HUCEgDC" )


for (state_name in unique(df_final_mn$State_full)) {
  
  # Filter the data for the current state
  df_ <- df_final_mn %>% filter(State_full == state_name)
  
  # Print the state name
  print(state_name)
  
  # Group by Year and summarize yrlyPlv
  summary_stats <- df_ %>%
    group_by(Year) %>%
    summarize(
      count = n(),
      mean_yrlyPlv = mean(yrlyPlv, na.rm = TRUE),
      median_yrlyPlv = median(yrlyPlv, na.rm = TRUE),
      sd_yrlyPlv = sd(yrlyPlv, na.rm = TRUE)
    )
  
  # Print the summary statistics for each year
  print(summary_stats, n=42)
}
