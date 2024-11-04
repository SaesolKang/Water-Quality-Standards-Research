pacman::p_load(fixest, did, stats)
df_sdid_mn <- df_7T23C_8122

# IPW
# Estimate the propensity scores with logistic regression
ps_model <- glm(Treated ~ rainydays + above_avg_days + ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf +factor(Year)*factor(State_full) + factor(ID), family = binomial(), data = df_sdid_mn)

# Extract predicted probabilities (propensity scores)
df_sdid_mn$pscore <- predict(ps_model, type = "response")

# Compute the inverse probability weights
df_sdid_mn$weight <- ifelse(df_sdid_mn$Treated == 1, 1 / df_sdid_mn$pscore, 1 / (1 - df_sdid_mn$pscore))

# Apply the IPW weights in the feols model
model_ipw <- feols(outcome ~ Treated + rainydays + above_avg_days + ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf, 
                   data = df_sdid_mn, 
                   weights = ~weight)

# Summarize the model
summary(model_ipw)


# Sunab


# State-specific analyses

df_final_AZ <- df_final_mn %>% filter(StateCd=="04")
df_final_CA <- df_final_mn %>% filter(StateCd=="06")
df_final_FL <- df_final_mn %>% filter(StateCd=="12")
df_final_HI <- df_final_mn %>% filter(StateCd=="15")
df_final_MT <- df_final_mn %>% filter(StateCd=="30")
df_final_NV <- df_final_mn %>% filter(StateCd=="32")
df_final_OK <- df_final_mn %>% filter(StateCd=="40")
df_final_UT <- df_final_mn %>% filter(StateCd=="49")
df_final_WI <- df_final_mn %>% filter(StateCd=="55")


MN_final <- list(df_final_AZ, df_final_CA, df_final_FL, df_final_MT, df_final_NV, df_final_OK, df_final_UT)
names(MN_final) <- c("AZ", "CA", "FL", "MT", "NV", "OK", "UT")

for (name in names(MN_final)) {
  df <- MN_final[[name]]
  
  df <- df %>%
    mutate(
      first_treat = ifelse(Treated == 0, Inf, AdoptionYear - min(Year) + 1),
      rel_time = ifelse(Treated == 0, -Inf, Year - AdoptionYear)
    )
  
  MN_final[[name]] <- df
}

  
  
  for (name in names(MN_final)) {
    df <- MN_final[[name]]
    
    # Model 1: Clustering HUCEgdc
    print(paste("Processing data frame for:", name))
    model1 <- feols(
      yrlyPlv ~ sunab(first_treat, rel_time) + rainydays + above_avg_days + ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf | Year^State_full + ID,
      
      data = df,
      cluster = "HUCEgDC"
    )
    print(summary(model1))
    
    # # Model 2: HUCE6
    # print(paste("Processing data frame for:", name))
    # model2 <- feols(yrlyPlv ~ Tstatus + above_avg_days + rainydays + ppt + ppt2 + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf | ID + Year, data = df, cluster="HUCE6" )
    # print(summary(model2))
    # 
    # # Model 3: HUCE4 
    # print(paste("Processing data frame for:", name))
    # model3 <- feols(yrlyPlv ~ Tstatus + above_avg_days + rainydays + ppt + ppt2 + t_05 + t_510 +  t_1015 +  t_1520 + t_2025 + t_25Inf  | ID + Year, data = df, cluster="HUCE4" )
    # print(summary(model3))
  }
  









# state-wide regression
df_sdid_mn <- df_7T23C_8122
df_sdid_mn %<>% mutate(tmean2 = tmean^2)
df_sdid_mn %<>% mutate(HUC6 = substr(HUCEgDC, 1, 6))

table(df_sdid_mn$State_full)
df_sdid_mn <- df_sdid_mn %>%
  # mutate(AdoptionYear = ifelse(AdoptionYear == 0, NA, AdoptionYear)) %>% 
  # mutate(tmax = max(Year)-min(Year)+1) %>% 
  mutate(first_treat = AdoptionYear - min(Year) + 1) %>% 
  mutate(rel_time = Year - AdoptionYear) 

df_sdid_mn %<>% mutate(first_treat = ifelse(Treated==0, Inf, first_treat)) %>% 
  mutate(rel_time = ifelse(Treated==0, -Inf, rel_time))

# 3. run staggered did 
model1 <- feols(
  yrlyPlv ~ sunab(first_treat, rel_time) + rainydays + above_avg_days + ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf | Year^State_full + ID,
  
  data = df_sdid_mn,
  cluster = "HUCEgDC"
)
summary(model1)

model2 <- feols(
  yrlyPlv ~ sunab(first_treat, rel_time) + rainydays + above_avg_days + ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf | Year^State_full + ID,
  
  data = df_sdid_mn,
  cluster = "HUC6"
)
summary(model2)

iplot(model1)
# Get 90% confidence intervals
confint(model1, level = 0.90)

# Vanilla option (above) is fine, but we can tweak a bit...
model1 |>
  iplot(
    main     = "",
    xlab     = "Time to treatment",
    drop     = "[[:digit:]]{2}",    # Limit lead and lag periods to -9:9
    ref.line = 1,
    ci.level = 0.90
  )

model1 |>
  iplot(
    main     = "",
    xlab     = "Time to treatment",
    drop     = "[[:digit:]]{2}",    # Limit lead and lag periods to -9:9
    ref.line = 1,
    ci.level = 0.95
  )

# Extracting coefficients and confidence intervals manually
coef_summary <- as.data.frame(coeftable(model1))
coef_summary$term <- rownames(coef_summary)
coef_summary <- coef_summary %>%
  mutate(term_numeric = as.numeric(substr(term, 11, 13))) %>%
  filter(term_numeric >= -9 & term_numeric <= 9)  # Keep only the terms between -9 and 9

# Plot with 90% confidence interval
ggplot(coef_summary, aes(x = term, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - 1.64 * `Std. Error`, ymax = Estimate + 1.64 * `Std. Error`), # 90% CI
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Time to treatment", y = "Coefficient", title = "Event Study") +
  theme_minimal()

# Sunab dataset construction for unbalanced data
df_sunab_mn <- df_final_mn %>% 
  filter(State_full != "New Jersey" & State_full != "Wisconsin") %>% 
  # filter(State_full == "Oklahoma" | State_full == "Florida" | State_full == "Utah") %>% 
  select(HUCEgDC, Treated, yrlyPlv, Year, ID, AdoptionYear, State_full, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)# 336 

table(df_sunab_mn$State_full)

df_sunab_ctrlmn <- df_control_pptmean %>% 
  mutate(ppt2=ppt*ppt, AdoptionYear=0) %>%  # Set the adoption year as 0 for the control group  
  select(yearlyPlevel, Year, ID, AdoptionYear, State_full, HUCEightDigitCode, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`) %>% 
  rename(yrlyPlv = yearlyPlevel, HUCEgDC = HUCEightDigitCode) %>% 
  mutate(Treated=0)
df_sunab_mn <- rbind(df_sunab_mn, df_sunab_ctrlmn)

table(df_sunab_mn$State_full)
table(df_sunab_ctrlmn$State_full)

table(df_sunab_mn$AdoptionYear)
table(df_sunab_mn$Year)

df_sunab_mn <- df_sunab_mn %>%
  mutate(first_treat = AdoptionYear - min(Year) + 1) %>% 
  mutate(rel_time = Year - AdoptionYear) 

df_sunab_mn %<>% mutate(first_treat = ifelse(Treated==0, Inf, first_treat)) %>% 
  mutate(rel_time = ifelse(Treated==0, -Inf, rel_time))

# 3. run staggered did 
model1 <- feols(
  yrlyPlv ~ sunab(first_treat, rel_time) + rainydays + above_avg_days + ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf | Year^State_full + ID,
  
  data = df_sunab_mn,
  cluster = "HUCEgDC"
)
summary(model1)

# Three Treated States (Oklahoma, Florida, Utah) and 23 Control States (1981-2022)
model1 |>
  iplot(
    main     = "",
    xlab     = "Time to treatment",
    drop     = "[[:digit:]]{2}",    # Limit lead and lag periods to -9:9
    ref.line = 1,
    ci.level = 0.90
  )


