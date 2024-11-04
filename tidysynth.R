require(tidysynth)
data("smoking")
smoking %>% dplyr::glimpse()

## Rows: 1,209
## Columns: 7
## $ state     <chr> "Rhode Island", "Tennessee", "Indiana", "Nevada", "Louisiana…
## $ year      <dbl> 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, …
## $ cigsale   <dbl> 123.9, 99.8, 134.6, 189.5, 115.9, 108.4, 265.7, 93.8, 100.3,…
## $ lnincome  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ beer      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ age15to24 <dbl> 0.1831579, 0.1780438, 0.1765159, 0.1615542, 0.1851852, 0.175…
## $ retprice  <dbl> 39.3, 39.9, 30.6, 38.9, 34.3, 38.4, 31.4, 37.3, 36.7, 28.8, …


smoking_out <-
  
  smoking %>%
  
  # initial the synthetic control object
  synthetic_control(outcome = cigsale, # outcome
                    unit = state, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "California", # unit where the intervention occurred
                    i_time = 1988, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  
  # average log income, retail price of cigarettes, and proportion of the
  # population between 15 and 24 years of age from 1980 - 1988
  generate_predictor(time_window = 1980:1988,
                     ln_income = mean(lnincome, na.rm = T),
                     ret_price = mean(retprice, na.rm = T),
                     youth = mean(age15to24, na.rm = T)) %>%
  
  # average beer consumption in the donor pool from 1984 - 1988
  generate_predictor(time_window = 1984:1988,
                     beer_sales = mean(beer, na.rm = T)) %>%

  # Lagged cigarette sales
  generate_predictor(time_window = 1975,
                     cigsale_1975 = cigsale) %>%
  generate_predictor(time_window = 1980,
                     cigsale_1980 = cigsale) %>%
  generate_predictor(time_window = 1988,
                     cigsale_1988 = cigsale) %>%

  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1970:1988, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

smoking_out %>% plot_trends()
#################################################################################
# Application
#################################################################################
library(pacman)
p_load(dplyr, lubridate, tidyr, magrittr, zoo, ggplot2, stringr, knitr)

df_final_AZ <- df_final %>% filter(StateCd=="04")
df_final_CA <- df_final %>% filter(StateCd=="06")
df_final_FL <- df_final %>% filter(StateCd=="12")
df_final_HI <- df_final %>% filter(StateCd=="15")
df_final_MT <- df_final %>% filter(StateCd=="30")
df_final_NV <- df_final %>% filter(StateCd=="32")
df_final_OK <- df_final %>% filter(StateCd=="40")
df_final_UT <- df_final %>% filter(StateCd=="49")
df_final_WI <- df_final %>% filter(StateCd=="55")

df_final_AZ <- df_final_mn %>% filter(StateCd=="04")
df_final_CA <- df_final_mn %>% filter(StateCd=="06")
df_final_FL <- df_final_mn %>% filter(StateCd=="12")
df_final_HI <- df_final_mn %>% filter(StateCd=="15")
df_final_MT <- df_final_mn %>% filter(StateCd=="30")
df_final_NV <- df_final_mn %>% filter(StateCd=="32")
df_final_NJ <- df_final_mn %>% filter(State_full=="New Jersey")

df_final_OK <- df_final_mn %>% filter(StateCd=="40")
df_final_UT <- df_final_mn %>% filter(StateCd=="49")
df_final_WI <- df_final_mn %>% filter(StateCd=="55")

# Proportion of the treated waterbodies in each state
# [1] "Arizona proportion for treated stations: 0.266401590457256 1006"
# [1] "Arizona proportion for treated waterbodies: 0.146666666666667 300"
# [1] "California proportion for treated stations: 0.0904545454545455 2200"
# [1] "California proportion for treated waterbodies: 0.0727513227513227 756"
# [1] "Florida proportion for treated stations: 0.500396196513471 5048"
# [1] "Florida proportion for treated waterbodies: 0.72884012539185 638"
# [1] "Nevada proportion for treated stations: 0.705756929637527 469"
# [1] "Nevada proportion for treated waterbodies: 0.517766497461929 197"
# [1] "Montana proportion for treated stations: 0.452933151432469 733"
# [1] "Montana proportion for treated waterbodies: 0.507552870090634 331"
# [1] "Oklahoma proportion for treated stations: 0.061493411420205 683"
# [1] "Oklahoma proportion for treated waterbodies: 0.0124378109452736 402"
# [1] "Utah proportion for treated stations: 0.742690058479532 171"
# [1] "Utah proportion for treated waterbodies: 0.704545454545455 88"

# Wisconsin (New Jersey adopted in 1981, so no pre trend at all)
df_WI_mn <- list_of_dfs_WI_2000[["stations_23_obs"]] %>% 
  select(Year, ID, yrlyPlv, HUCEgDC, AdoptionYear, State_full, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`) %>% 
  left_join(df_tmean, by = c("ID" = "ID", "Year" = "Year"))  

table(df_WI_mn$Year)

df_ctrlmn_synth <- list_of_dfs_ctrlmn_2000[["mn_23_obs"]] %>% 
                    mutate(ppt2=ppt*ppt, AdoptionYear=0) %>%  # Set the adoption year as 0 for the control group  
                    select(Year, ID, yearlyPlevel,  HUCEightDigitCode, AdoptionYear, State_full,ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`) %>% 
                    rename(yrlyPlv = yearlyPlevel, HUCEgDC = HUCEightDigitCode) %>% 
                    filter(Year > 1999) %>% 
                    left_join(tmean_control, by = c("ID" = "ID", "Year" = "Year"))  # 1771 
table(df_ctrlmn_synth$Year)

df_WI_synth <- rbind(df_WI_mn, df_ctrlmn_synth) %>%
  group_by(State_full, Year) %>%
  mutate(state_plv = mean(yrlyPlv, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-yrlyPlv) %>% 
  distinct(Year, State_full, .keep_all = TRUE) %>% 
  arrange(State_full, Year) # 957

table(df_WI_synth$Year)

wqs_out <-  df_WI_synth %>%
  
  synthetic_control(outcome = state_plv, 
                    unit = State_full, 
                    time = Year, 
                    i_unit = "Wisconsin", 
                    i_time = 2010, 
                    generate_placebos=F ) %>% 
  

  generate_predictor(time_window = 1981:2010,
                     AVG_above_avg = mean(above_avg_days, na.rm = T),
                     AVG_rainydays = mean(rainydays, na.rm = T),
                     AVG_ppt = mean(ppt, na.rm = T),
                     AVG_ppt2 = mean(ppt2, na.rm = T)
  ) %>%
  # average beer consumption in the donor pool from 1984 - 1988
  generate_predictor(time_window = 1981:2010,
                     AVG_tmean = mean(tmean, na.rm = T),
                     AVG_t_05 = mean(t_05, na.rm = T),
                     AVG_t_510 = mean(t_510, na.rm = T),
                     AVG_t_1015 = mean(t_1015, na.rm = T),
                     AVG_t_1520 = mean(t_1520, na.rm = T),
                     AVG_t_2025 = mean(t_2025, na.rm = T),
                     AVG_t_25Inf = mean(t_25Inf, na.rm = T)
  ) %>%
  
   # Hold off from using lagged p levels for now to maintain more obs
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1981:2010, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

wqs_out %>% plot_trends()
wqs_out %>% plot_differences()
wqs_out %>% plot_weights()
wqs_out %>% grab_balance_table()
wqs_out %>% plot_placebos()
wqs_out %>% plot_placebos(prune = FALSE)
wqs_out %>% plot_mspe_ratio()


# Error in synth_method(treatment_unit_covariates = treatment_unit_covariates,  : 
#                         
#                         NAs in Z0 
# The error message NAs in Z0 indicates that there are missing values (NAs) in the covariate matrix (Z0) used in the optimization step for creating the synthetic control.                      
# Check for NA or infinite values in the predictors
# Check the number of missing values in each column
colSums(is.na(df_WI_synth))
summary(df_WI_synth)

df_final_FL %>%
  summarise(
    Any_NA = any(is.na(yrlyPlv) | is.na(above_avg_days) | is.na(rainydays) | is.na(ppt) | is.na(ppt2)),
    Any_Inf = any(is.infinite(yrlyPlv) | is.infinite(above_avg_days) | is.infinite(rainydays) | is.infinite(ppt) | is.infinite(ppt2))
  )

df_final_FL %>%
  summarise(
    NA_above_avg_days = sum(is.na(above_avg_days)),
    NA_rainydays = sum(is.na(rainydays)),
    NA_ppt = sum(is.na(ppt)),
    NA_ppt2 = sum(is.na(ppt2)),
    NA_t_05 = sum(is.na(t_05)),
    NA_t_510 = sum(is.na(t_510)),
    NA_t_1015 = sum(is.na(t_1015)),
    NA_t_1520 = sum(is.na(t_1520)),
    NA_t_2025 = sum(is.na(t_2025)),
    NA_t_25Inf = sum(is.na(t_25Inf))
  )




# Waterbody level
df_final_FL <- df_final %>% 
  filter(StateCd=="12") %>% 
  filter(Treated == 0 | UniqueID == "39316") # 1719 OBS

wqs_out <-  df_final_FL %>%
  
  synthetic_control(outcome = yearlyPlevel, 
                    unit = UniqueID, 
                    time = Year, 
                    i_unit = "39316", 
                    i_time = 2014, 
                    generate_placebos=T ) %>% 
  
  
  generate_predictor(time_window = 1981:2014,
                     AVG_above_avg = mean(above_avg_days, na.rm = T),
                     AVG_rainydays = mean(rainydays, na.rm = T),
                     AVG_ppt = mean(y_ppt, na.rm = T),
                     AVG_ppt2 = mean(y_ppt2, na.rm = T),
                     AVG_t_05 = mean(t_05, na.rm = T),
                     AVG_t_510 = mean(t_510, na.rm = T),
                     AVG_t_1015 = mean(t_1015, na.rm = T),
                     AVG_t_1520 = mean(t_1520, na.rm = T),
                     AVG_t_2025 = mean(t_2025, na.rm = T),
                     AVG_t_25Inf = mean(t_25Inf, na.rm = T),
  ) %>% 
  # Hold off from using lagged p levels for now to maintain more obs
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1981:2014, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()


df_final_FL %>%
  summarise(
    NA_above_avg_days = sum(is.na(above_avg_days)),
    NA_rainydays = sum(is.na(rainydays)),
    NA_ppt = sum(is.na(y_ppt)),
    NA_ppt2 = sum(is.na(y_ppt2)),
    NA_t_05 = sum(is.na(t_05)),
    NA_t_510 = sum(is.na(t_510)),
    NA_t_1015 = sum(is.na(t_1015)),
    NA_t_1520 = sum(is.na(t_1520)),
    NA_t_2025 = sum(is.na(t_2025)),
    NA_t_25Inf = sum(is.na(t_25Inf))
  )

save(df_final, df_final_mn, file = "tidysynth_data.RData")




# Station level
df_final_AZ <- df_final_mn %>% filter(StateCd=="04")
df_final_CA <- df_final_mn %>% filter(StateCd=="06")
df_final_FL <- df_final_mn %>% filter(StateCd=="12")
df_final_HI <- df_final_mn %>% filter(StateCd=="15")
df_final_MT <- df_final_mn %>% filter(StateCd=="30")
df_final_NV <- df_final_mn %>% filter(StateCd=="32")
df_final_OK <- df_final_mn %>% filter(StateCd=="40")
df_final_UT <- df_final_mn %>% filter(StateCd=="49")
df_final_WI <- df_final_mn %>% filter(StateCd=="55")

# Florida
df_final_FL <- df_final_mn %>% 
  filter(StateCd=="12") %>% 
  filter(Treated == 0 | ID == "12112") # 7983 OBS

wqs_out <-  df_final_FL %>%
  
  synthetic_control(outcome = yrlyPlv, 
                    unit = ID, 
                    time = Year, 
                    i_unit = "12112", 
                    i_time = 2014, 
                    generate_placebos=T ) %>% 
  
  
  generate_predictor(time_window = 1981:2014,
                     AVG_above_avg = mean(above_avg_days, na.rm = T),
                     AVG_rainydays = mean(rainydays, na.rm = T),
                     AVG_ppt = mean(ppt, na.rm = T),
                     AVG_ppt2 = mean(ppt2, na.rm = T),
                     AVG_t_05 = mean(t_05, na.rm = T),
                     AVG_t_510 = mean(t_510, na.rm = T),
                     AVG_t_1015 = mean(t_1015, na.rm = T),
                     AVG_t_1520 = mean(t_1520, na.rm = T),
                     AVG_t_2025 = mean(t_2025, na.rm = T),
                     AVG_t_25Inf = mean(t_25Inf, na.rm = T),
  ) %>% 
  # Hold off from using lagged p levels for now to maintain more obs
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1981:2014, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

# Check for NA or infinite values in the predictors
df_final_FL %>%
  summarise(
    Any_NA = any(is.na(yrlyPlv) | is.na(above_avg_days) | is.na(rainydays) | is.na(ppt) | is.na(ppt2)),
    Any_Inf = any(is.infinite(yrlyPlv) | is.infinite(above_avg_days) | is.infinite(rainydays) | is.infinite(ppt) | is.infinite(ppt2))
  )

df_final_FL %>%
  summarise(
    NA_above_avg_days = sum(is.na(above_avg_days)),
    NA_rainydays = sum(is.na(rainydays)),
    NA_ppt = sum(is.na(ppt)),
    NA_ppt2 = sum(is.na(ppt2)),
    NA_t_05 = sum(is.na(t_05)),
    NA_t_510 = sum(is.na(t_510)),
    NA_t_1015 = sum(is.na(t_1015)),
    NA_t_1520 = sum(is.na(t_1520)),
    NA_t_2025 = sum(is.na(t_2025)),
    NA_t_25Inf = sum(is.na(t_25Inf))
  )
