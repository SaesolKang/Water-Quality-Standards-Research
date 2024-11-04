# Compare within state treated and control stations to see whether pollution was the determinant of selection
df_eventstudy_mn <- df_final_mn %>%  
  select(Treated, Tstatus, yrlyPlv, Year, ID, AdoptionYear, State_full, HUCEgDC, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)  %>% 
  filter(State_full != "Wisconsin" & State_full != "New Jersey") %>%   # Exclude if you want to control for the state fixed effect
  mutate(first_treat = AdoptionYear - min(Year) + 1) %>% 
  mutate(rel_time = Year - AdoptionYear) %>% 
  mutate(first_treat = ifelse(Treated==0, Inf, first_treat)) %>% 
  mutate(rel_time = ifelse(Treated==0, -Inf, rel_time))


# Simple model
for (state in unique(df_eventstudy_mn$State_full)) {
  # Filter for each state
  df <- df_eventstudy_mn %>% filter(State_full == state)
  
  # Run event study model
  eventstudy <- feols(
    yrlyPlv ~ sunab(first_treat, rel_time) + above_avg_days + rainydays + ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf | Year + ID,
    data = df,
    cluster = "HUCEgDC"
  )
  
  # Print summary of the model
  # print(summary(eventstudy))
  
  # Plot the results, limiting lead and lag periods to -9:9
  iplot(
    eventstudy,
    main = state,
    xlab = "Time to treatment",
    drop     = "[[:digit:]]{2}",    # Limit lead and lag periods to -9:9
    ref.line = 1
  )
}





df_eventstudy_AZ <- df_final_mn %>%  
  select(Treated, Tstatus, yrlyPlv, Year, ID, AdoptionYear, State_full, HUCEgDC, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)  %>% 
  # filter(State_full != "Wisconsin" & State_full != "New Jersey") %>%   # Exclude if you want to control for the state fixed effect
  filter(State_full == "Arizona")

df_eventstudy_CA <- df_final_mn %>%  
  select(Treated, Tstatus, yrlyPlv, Year, ID, AdoptionYear, State_full, HUCEgDC, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)  %>% 
  # filter(State_full != "Wisconsin" & State_full != "New Jersey") %>%   # Exclude if you want to control for the state fixed effect
  filter(State_full == "California")

df_eventstudy_FL <- df_final_mn %>%  
  select(Treated, Tstatus, yrlyPlv, Year, ID, AdoptionYear, State_full, HUCEgDC, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)  %>% 
  # filter(State_full != "Wisconsin" & State_full != "New Jersey") %>%   # Exclude if you want to control for the state fixed effect
  filter(State_full == "Florida")

df_eventstudy_MT <- df_final_mn %>%  
  select(Treated, Tstatus, yrlyPlv, Year, ID, AdoptionYear, State_full, HUCEgDC, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)  %>% 
  # filter(State_full != "Wisconsin" & State_full != "New Jersey") %>%   # Exclude if you want to control for the state fixed effect
  filter(State_full == "Montana")

df_eventstudy_NV <- df_final_mn %>%  
  select(Treated, Tstatus, yrlyPlv, Year, ID, AdoptionYear, State_full, HUCEgDC, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)  %>% 
  # filter(State_full != "Wisconsin" & State_full != "New Jersey") %>%   # Exclude if you want to control for the state fixed effect
  filter(State_full == "Nevada")

df_eventstudy_OK <- df_final_mn %>%  
  select(Treated, Tstatus, yrlyPlv, Year, ID, AdoptionYear, State_full, HUCEgDC, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)  %>% 
  # filter(State_full != "Wisconsin" & State_full != "New Jersey") %>%   # Exclude if you want to control for the state fixed effect
  filter(State_full == "Oklahoma")

df_eventstudy_UT <- df_final_mn %>%  
  select(Treated, Tstatus, yrlyPlv, Year, ID, AdoptionYear, State_full, HUCEgDC, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)  %>% 
  # filter(State_full != "Wisconsin" & State_full != "New Jersey") %>%   # Exclude if you want to control for the state fixed effect
  filter(State_full == "Utah")


df_eventstudy <- rbind(df_eventstudy_AZ, df_eventstudy_ctrlmn) 
df_eventstudy <- rbind(df_eventstudy_CA, df_eventstudy_ctrlmn) 
df_eventstudy <- rbind(df_eventstudy_FL, df_eventstudy_ctrlmn) 
df_eventstudy <- rbind(df_eventstudy_MT, df_eventstudy_ctrlmn) 
df_eventstudy <- rbind(df_eventstudy_NV, df_eventstudy_ctrlmn) 
df_eventstudy <- rbind(df_eventstudy_OK, df_eventstudy_ctrlmn) 
df_eventstudy <- rbind(df_eventstudy_UT, df_eventstudy_ctrlmn) 

table(df_eventstudy$State_full)

df_eventstudy <- df_eventstudy  %>% 
  mutate(first_treat = AdoptionYear - min(Year) + 1) %>% 
  mutate(rel_time = Year - AdoptionYear) %>% 
  mutate(first_treat = ifelse(Treated==0, Inf, first_treat)) %>% 
  mutate(rel_time = ifelse(Treated==0, -Inf, rel_time))


eventstudy <- feols(
  yrlyPlv ~ sunab(first_treat, rel_time) + above_avg_days + rainydays + ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf | Year + ID,
  data = df_eventstudy,
  cluster = "HUCEgDC"
)
summary(eventstudy)
eventstudy |>
  iplot(
    main     = "",
    xlab     = "Time to treatment",
    drop     = "[[:digit:]]{2}",    # Limit lead and lag periods to -9:9
    ref.line = 1
  )


eventstudy <- feols(
  yrlyPlv ~ sunab(first_treat, rel_time) + above_avg_days + rainydays + ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf | Year + ID,
  data = df_final_AZ,
  cluster = "HUCEgDC"
)
summary(eventstudy)
eventstudy |>
  iplot(
    main     = "",
    xlab     = "Time to treatment",
    drop     = "[[:digit:]]{2}",    # Limit lead and lag periods to -9:9
    ref.line = 1
  )








df_mn_eventstudy <- df_final_CA  %>% 
  mutate(first_treat = AdoptionYear - min(Year) + 1) %>% 
  mutate(rel_time = Year - AdoptionYear) %>% 
  mutate(first_treat = ifelse(Treated==0, Inf, first_treat)) %>% 
  mutate(rel_time = ifelse(Treated==0, -Inf, rel_time))

# 3. run staggered did with residuals
model1 <- feols(
  yrlyPlv ~ sunab(first_treat, rel_time) + rainydays + above_avg_days + ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf | Year^State_full + ID,
  
  data = df_mn_eventstudy,
  cluster = "HUCEgDC"
)
summary(model1)
