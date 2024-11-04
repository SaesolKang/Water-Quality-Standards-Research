# Load packages
library(pacman)
p_load(dplyr, did, tidyr, magrittr)

#########################################################################
# Callaway Sant'Anna residual analysis
#########################################################################
# df_sdid_mn %<>% mutate(tmean2 = tmean^2)

# 1. run without Tstatus
# Model 1: FE - Year^StateCd + ID; cluster - HUCEgDC
model1 <- feols(yrlyPlv ~ rainydays + above_avg_days + ppt + ppt2  + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf  | Year^State_full + ID, data = df_sdid_mn, cluster="HUCEgDC" )
summary(model1)

# 2. obtain residuals
model1_resid <- as.data.frame(resid(model1))
colnames(model1_resid) <- "Plv_resid"

df_sdid_mn_resid <- cbind(df_sdid_mn, model1_resid)

# Callaway Sant'Anna
# 1. Simple & balanced panel (3 treated states + control states, 2000-2022)
result_att1 <- att_gt(
  yname = "Plv_resid",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn_resid,                 # Dataset
)
aggte(result_att1, type = "dynamic", na.rm=TRUE)

result_att1 <- att_gt(
  yname = "Plv_resid",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  control_group="notyettreated", # default is never treated
  data = df_sdid_mn_resid,                 # Dataset
)
aggte(result_att1, type = "dynamic", na.rm=TRUE)



# 2. Simple & unbalanced panel (7 treated states + control states, 1981-2022)
result_att2 <- att_gt(
  yname = "Plv_resid",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",
  data = df_sdid_mn_resid,     # Ensure df_final_did is correctly prepared
  allow_unbalanced_panel = TRUE
)
aggte(result_att2, type = "dynamic", na.rm=TRUE)

result_att2 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",
  control_group="notyettreated", # default is never treated
  data = df_sdid_mn_resid,     # Ensure df_final_did is correctly prepared
  allow_unbalanced_panel = TRUE
)
aggte(result_att2, type = "dynamic", na.rm=TRUE)

# 3. Simple & unbalanced panel (3 treated states + control states, 1981-2022)
result_att2 <- att_gt(
  yname = "Plv_resid",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",
  data = df_sdid_mn_resid,     
  allow_unbalanced_panel = TRUE
)
aggte(result_att2, type = "dynamic", na.rm=TRUE)

result_att2 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",
  control_group="notyettreated", # default is never treated
  data = df_sdid_mn_resid,     # Ensure df_final_did is correctly prepared
  allow_unbalanced_panel = TRUE
)
aggte(result_att2, type = "dynamic", na.rm=TRUE)

# 4. Simple & unbalanced panel (7 treated states + control states, 1981-2022)
result_att2 <- att_gt(
  yname = "Plv_resid",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",
  data = df_sdid_mn_resid,     # Ensure df_final_did is correctly prepared
  allow_unbalanced_panel = TRUE
)
aggte(result_att2, type = "dynamic", na.rm=TRUE)

result_att2 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",
  control_group="notyettreated", # default is never treated
  data = df_sdid_mn_resid,     # Ensure df_final_did is correctly prepared
  allow_unbalanced_panel = TRUE
)
aggte(result_att2, type = "dynamic", na.rm=TRUE)


##########################################################################
#station level data (42 consecutive observations)
##########################################################################

# 1. load the treated states data
df_final_mn

# 2. filter for the 42 consecutive observations
# Processed in Summary_stats_on_the_number_of_waterbodies(stations)
df_stations_42obs <- list_of_dfs_mn[["stations_42_obs"]] %>%  
                      select(yrlyPlv, Year, ID, AdoptionYear, State_full, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)# 336 

  # How many unique stations? Should be 8.
  # It is 8. Cali 1, Flo 1, New Jersey 6
  table(df_stations_42obs$State_full)
# 3. load the control states data
df_control_pptmean 

# 4. filter for the 42 consecutive observations
# Processed in Summary_stats_on_the_number_of_waterbodies(stations)
df_ctrlmn_42obs <- list_of_dfs_ctrlmn[["mn_42_obs"]] %>% 
                    mutate(ppt2=ppt*ppt, AdoptionYear=0) %>%  # Set the adoption year as 0 for the control group  
                    select(yearlyPlevel, Year, ID, AdoptionYear, State_full, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`) %>% 
                    rename(yrlyPlv = yearlyPlevel) # 1092 

  # How many unique stations? should be 26
df_ctrlmn_42obs %>% select(ID) %>% distinct() %>% count()

# 5. append them.
df_sdid_mn <- rbind(df_stations_42obs, df_ctrlmn_42obs)

# Check the missing variables
table(df_sdid_mn$AdoptionYear, useNA = "ifany")
#   0 1981 1995 2014 
# 1092  252   42   42 
for (col in names(df_sdid_mn)) {
  var_na <- sum(is.na(df_sdid_mn[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}

#####################Staggered DID###############################
# 1. Simple & balanced panel
result_att1 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  # control_group="notyettreated", # default is never treated
  data = df_sdid_mn,                 # Dataset
)
# Warning messages:
#   1: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
#                           Dropped 6 units that were already treated in the first period.
#                         2: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
#                                                 Be aware that there are some small groups in your dataset.
#                                               Check groups: 1995,2014.
#                                               3: In att_gt(yname = "yrlyPlv", tname = "Year", idname = "ID", gname = "AdoptionYear",  :
#                                                              Not returning pre-test Wald statistic due to singular covariance matrix
aggte(result_att1, type = "dynamic", na.rm=TRUE)

# Plot the coefficients by the year

# 2. Covariates & balanced panel
result_att2 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + rainydays + above_avg_days ,  # Covariates
)
# No pre-treatment periods to test
# There were 50 or more warnings (use warnings() to see the first 50)
aggte(result_att2, type = "dynamic", na.rm=TRUE)
# Error in max(t) : invalid 'type' (list) of argument

##########################################################################
#station level data (23 consecutive observations from 2000)
##########################################################################

# 1. load the main data
df_tmean <- df_final_mn %>% select(ID, Year, tmean)

# 2. filter for 23 consecutive observations
df_stations_23obs <- list_of_dfs_mn_2000[["stations_23_obs"]] %>%  
  select(Treated, Tstatus, StateCd, HUCEgDC, yrlyPlv, Year, ID, AdoptionYear, State_full, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`) %>% 
  filter(AdoptionYear>2000) %>% # Remove the states who adopted  before 2000
  filter(Year>1999) %>% 
  filter(State_full != "Wisconsin") %>% 
  left_join(df_tmean, by = c("ID" = "ID", "Year" = "Year"))  

  # How many unique stations? 8.
    df_stations_23obs %>% select(ID) %>% distinct() %>% count()
    table(df_stations_23obs$State_full)
    table(df_stations_23obs$AdoptionYear)
    table(df_stations_23obs$Year)
    
# 3. load the control state data
df_control_pptmean 

# 4. filter for the 23 consecutive observations
# Processed in Summary_stats_on_the_number_of_waterbodies(stations)
df_ctrlmn_23obs <- list_of_dfs_ctrlmn_2000[["mn_23_obs"]] %>% 
  mutate(ppt2=ppt*ppt, AdoptionYear=0) %>%  # Set the adoption year as 0 for the control group  
  select(yearlyPlevel, Year, ID, AdoptionYear, State_full, StateCode, HUCEightDigitCode, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`) %>% 
  rename(yrlyPlv = yearlyPlevel, StateCd = StateCode, HUCEgDC = HUCEightDigitCode) %>% 
  filter(Year > 1999) %>% 
  mutate(Tstatus = 0, Treated = 0) %>% 
  left_join(tmean_control, by = c("ID" = "ID", "Year" = "Year"))  # 1771 

table(df_ctrlmn_23obs$State_full)
# Alabama       Arkansas    Connecticut          Idaho        Indiana           Iowa 
# 23            138            506             23             92             69 
# Kansas       Kentucky      Louisiana       Maryland    Mississippi       New York 
# 92             23             69             92             46             23 
# North Carolina   North Dakota           Ohio   Pennsylvania          Texas 
# 115            345             23             23             69 
# How many unique stations? 77
View(summary_list2_ctrlmn_2000[["summary_23_years"]])
df_ctrlmn_23obs %>% select(ID) %>% distinct() %>% count()
            
            # # Selecting the 'mn_23_obs' data frame from the list
            # df_mn_23_obs <- list_of_dfs_ctrlmn_2000[["mn_23_obs"]] 
            # 
            # # Performing an anti_join to find IDs in df_ctrlmn_23obs not in df_mn_23_obs
            # unique_ids <- anti_join(df_mn_23_obs, df_ctrlmn_23obs,  by = "ID")
            # 
            # # Viewing the result
            # unique_ids
            # 
            # df_mn_23_obs %<>% select(ID, Year, yearlyPlevel, everything()) %>% 
            #   arrange(ID, Year) # 47505
            # df_mn_42_obs <- list_of_dfs_ctrlmn[["mn_42_obs"]] %>% 
            #                 select(ID, Year, yearlyPlevel, everything()) %>% 
            #                 arrange(ID, Year)
            # df_stations_42obs <- list_of_dfs_mn[["stations_42_obs"]] %>% 
            #                       select(ID, Year, yrlyPlv, everything()) %>% 
            #                       arrange(ID, Year)
            # df_stations_23obs <- list_of_dfs_mn_2000[["stations_23_obs"]] %>% 
            #   select(ID, Year, yrlyPlv, everything()) %>% 
            #   arrange(ID, Year) %>% 
            #   filter(Year > 1999)
            
            
# 5. append them.
df_sdid_mn <- rbind(df_stations_23obs, df_ctrlmn_23obs) 

# Check the missing variables
table(df_sdid_mn$AdoptionYear, useNA = "ifany")
#     0 2002 2010 2014 2020 
# 1771   46  184  115   23
table(df_sdid_mn$Year, useNA = "ifany")

for (col in names(df_sdid_mn)) {
  var_na <- sum(is.na(df_sdid_mn[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}

##########################################################################
#station level data (27 consecutive observations from 1996)
##########################################################################

# 1. load the main data
df_tmean <- df_final_mn %>% select(ID, Year, tmean)

# 2. filter for 23 consecutive observations
df_stations_27obs <- list_of_dfs_mn_1996[["stations_27_obs"]] %>%  
  select(Treated, Tstatus, StateCd, HUCEgDC, yrlyPlv, Year, ID, AdoptionYear, State_full, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`) %>% 
  filter(AdoptionYear>2000) %>% # Remove the states who adopted  before 2000
  filter(Year>1995) %>% 
  filter(State_full != "Wisconsin") %>% 
  left_join(df_tmean, by = c("ID" = "ID", "Year" = "Year"))  

# How many unique stations? 6.
df_stations_27obs %>% select(ID) %>% distinct() %>% count()
table(df_stations_27obs$State_full)
table(df_stations_27obs$AdoptionYear)
table(df_stations_27obs$Year)

# 3. load the control state data
df_control_pptmean 

# 4. filter for the 23 consecutive observations
# Processed in Summary_stats_on_the_number_of_waterbodies(stations)
df_ctrlmn_27obs <- list_of_dfs_ctrlmn_1996[["mn_27_obs"]] %>% 
  mutate(ppt2=ppt*ppt, AdoptionYear=0) %>%  # Set the adoption year as 0 for the control group  
  select(yearlyPlevel, Year, ID, AdoptionYear, State_full, StateCode, HUCEightDigitCode, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`) %>% 
  rename(yrlyPlv = yearlyPlevel, StateCd = StateCode, HUCEgDC = HUCEightDigitCode) %>% 
  filter(Year > 1995) %>% 
  mutate(Tstatus = 0, Treated = 0) %>% 
  left_join(tmean_control, by = c("ID" = "ID", "Year" = "Year"))  # 1771 

table(df_ctrlmn_27obs$State_full)
# Alabama       Arkansas    Connecticut          Idaho        Indiana           Iowa 
# 27            162            540             27            108             81 
# Kentucky      Louisiana       Maryland    Mississippi       New York North Carolina 
# 27             81             81             54             27            108 
# North Dakota           Ohio          Texas 
# 324             27             54 
# How many unique stations? 
View(summary_list2_ctrlmn_1996[["summary_27_years"]])
df_ctrlmn_23obs %>% select(ID) %>% distinct() %>% count()

# # Selecting the 'mn_23_obs' data frame from the list
# df_mn_23_obs <- list_of_dfs_ctrlmn_2000[["mn_23_obs"]] 
# 
# # Performing an anti_join to find IDs in df_ctrlmn_23obs not in df_mn_23_obs
# unique_ids <- anti_join(df_mn_23_obs, df_ctrlmn_23obs,  by = "ID")
# 
# # Viewing the result
# unique_ids
# 
# df_mn_23_obs %<>% select(ID, Year, yearlyPlevel, everything()) %>% 
#   arrange(ID, Year) # 47505
# df_mn_42_obs <- list_of_dfs_ctrlmn[["mn_42_obs"]] %>% 
#                 select(ID, Year, yearlyPlevel, everything()) %>% 
#                 arrange(ID, Year)
# df_stations_42obs <- list_of_dfs_mn[["stations_42_obs"]] %>% 
#                       select(ID, Year, yrlyPlv, everything()) %>% 
#                       arrange(ID, Year)
# df_stations_23obs <- list_of_dfs_mn_2000[["stations_23_obs"]] %>% 
#   select(ID, Year, yrlyPlv, everything()) %>% 
#   arrange(ID, Year) %>% 
#   filter(Year > 1999)


# 5. append them.
df_sdid_mn <- rbind(df_stations_27obs, df_ctrlmn_27obs) 

# Check the missing variables
table(df_sdid_mn$AdoptionYear, useNA = "ifany")
#     0 2002 2010 2014 2020 
# 1771   46  184  115   23
table(df_sdid_mn$Year, useNA = "ifany")

for (col in names(df_sdid_mn)) {
  var_na <- sum(is.na(df_sdid_mn[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}

#######################Staggered DID#######################################
# 1. Simple & balanced panel
result_att3 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
)
# Warning messages:
#   1: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
#                           Be aware that there are some small groups in your dataset.
#                         Check groups: 2002,2020.
#                         2: In att_gt(yname = "yrlyPlv", tname = "Year", idname = "ID", gname = "AdoptionYear",  :
#                                        Not returning pre-test Wald statistic due to singular covariance matrix

aggte(result_att3, type = "dynamic", na.rm=TRUE)

# plot the results
ggdid(result_att3)

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  # xformla = ~ State_full,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)


# 2. Covariates & balanced panel
result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ State_full + ppt + ppt2 + rainydays + above_avg_days + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)



result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + rainydays + above_avg_days + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + above_avg_days + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 +  t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 ,  # Covariates
) # no pre-treatment warning disappears
aggte(result_att4, type = "dynamic", na.rm=TRUE)

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  control_group="notyettreated", # default is never treated
  xformla = ~ `t_-Inf0` + t_05 + t_510 + t_1520 + t_2025 + t_25Inf,  # Covariates
) # Error in inffunc1[, whichones] : subscript out of bounds
# The "subscript out of bounds" error in aggte suggests that the output from att_gt may not be correctly structured or expected values are missing due to earlier errors.
# No pre-treatment periods to test
# There were 50 or more warnings (use warnings() to see the first 50)
aggte(result_att4, type = "dynamic", na.rm=TRUE)
# Error in max(t) : invalid 'type' (list) of argument
          # Check types and contents of critical variables
          str(df_sdid_mn$Year)
          str(df_sdid_mn$AdoptionYear)
          
          # Convert Year to numeric if it's not
          df_sdid_mn$Year <- as.numeric(as.character(df_sdid_mn$Year))
          df_sdid_mn$AdoptionYear <- as.numeric(as.character(df_sdid_mn$AdoptionYear))
          
          # Check for unique years to confirm pre-treatment data
          unique(df_sdid_mn$Year)

          # Check the temp vars
          df_sdid_mn %<>% mutate(t_sum = t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf + `t_-Inf0`) %>% select(Year, t_sum, everything())
          df_sdid_mn %<>% mutate(t_5 = `t_-Inf0` + t_05 ) 
          df_sdid_mn %<>% mutate(t_10 = `t_-Inf0` + t_05 +  + t_510) 
          df_sdid_mn %<>% mutate(t_15 = `t_-Inf0` + t_05 +  + t_510 + t_1015) 
          
          table(df_test$t_5) # 107
          table(df_test$t_10) # 4
          table(df_test$t_15) # No 0's
          
          table(df_sdid_mn$`t_-Inf0`) # 172
          table(df_sdid_mn$`t_05`) # 107
          table(df_sdid_mn$`t_510`) # 4 zero's
          table(df_sdid_mn$`t_1015`) # no zero's
          table(df_sdid_mn$`t_1520`) # no zero's
          table(df_sdid_mn$`t_2025`) # no zero's
          table(df_sdid_mn$`t_25Inf`) # 40
          
result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + rainydays + above_avg_days ,  # Covariates
)

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + rainydays + above_avg_days+tmean,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)

# Warning messages:
#   1: In pre_process_did(yname = yname, tname = tname, idname = idname,  ... :
#                           Be aware that there are some small groups in your dataset.
#                         Check groups: 2002,2010,2014,2020.
#                         2: glm.fit: algorithm did not converge
#                         3: glm.fit: fitted probabilities numerically 0 or 1 occurred
aggte(result_att4, type = "dynamic", na.rm=TRUE)

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ State_full + ppt + ppt2 + rainydays + above_avg_days ,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)
# Error in max(t) : invalid 'type' (list) of argument

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + rainydays + above_avg_days+ t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + rainydays + above_avg_days+ t_15 + t_1520 + t_2025 ,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + rainydays + above_avg_days ,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)

##########################################################################
#station level data (all obs)
##########################################################################
# 1. load the main data
df_final_mn 

# 2. filter the treated data
df_stations <- df_final_mn %>%  
  select(Treated, Tstatus, yrlyPlv, Year, ID, AdoptionYear, State_full, HUCEgDC, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`)  %>% 
  # filter(State_full == "Oklahoma" | State_full == "Florida" | State_full == "Utah") %>%
  # filter(Year>1999) %>% 
  filter(State_full != "Wisconsin" & State_full != "New Jersey") %>%  # Exclude if you want to control for the state fixed effect
  filter(State_full != "New Jersey")  # Exclude if you want to control for the state fixed effect

# How many unique stations? 10310
df_stations %>% select(ID) %>% distinct() %>% count()
table(df_stations$State_full)
table(df_stations$AdoptionYear)
table(df_stations$Year)

# 3. load the control state data
tmean_control <- df_control_pptmean %>% select(ID, Year, tmean)

# 4. filter the untreated data
# Processed in Summary_stats_on_the_number_of_waterbodies(stations)
df_ctrlmn <- list_of_dfs_ctrlmn[["mn_1_obs"]] %>% 
  mutate(ppt2 = ppt * ppt, AdoptionYear = 0, Tstatus=0) %>%  # Set the adoption year as 0 for the control group  
  select(Tstatus, yearlyPlevel, Year, ID, AdoptionYear, State_full, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`, HUCEightDigitCode) %>% 
  rename(yrlyPlv = yearlyPlevel, HUCEgDC = HUCEightDigitCode) %>%  
  # left_join(tmean_control, by = c("ID" = "ID", "Year" = "Year")) %>% 
  # filter(Year>1999) %>% 
  mutate(Tstatus = 0, Treated = 0)  



table(df_ctrlmn$State_full)

  # How many unique stations? 13694
View(summary_list2_ctrlmn[["summary_1_years"]])
df_ctrlmn %>% select(ID) %>% distinct() %>% count()

# 5. append them.
df_sdid_mn <- rbind(df_stations, df_ctrlmn) 

# Check the missing variables
table(df_sdid_mn$AdoptionYear, useNA = "ifany")
# 0  1992  1995  1998  2002  2008  2014  2020 
# 47505  3950  5491  2184  2728  2018 15567   540 

table(df_sdid_mn$Year, useNA = "ifany")

for (col in names(df_sdid_mn)) {
  var_na <- sum(is.na(df_sdid_mn[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}

# Save data frames with different names
df_7T23C_8122<- df_sdid_mn
save(df_7T23C_8122, file = "df_7T23C_8122.RData")

#####################Staggered DID###############################
# 1. Simple & unbalanced panel
result_att5 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  allow_unbalanced_panel = TRUE
  )
aggte(result_att5, type = "dynamic", na.rm=TRUE)
# plot the results
ggdid(result_att5)

result_att5 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  control_group="notyettreated", # default is never treated
  allow_unbalanced_panel = TRUE
)
aggte(result_att5, type = "dynamic", na.rm=TRUE)

ggdid(result_att5)


# 2. Covariates & balanced panel
result_att6 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + rainydays + above_avg_days ,  # Covariates
  allow_unbalanced_panel = TRUE
)
aggte(result_att6, type = "dynamic", na.rm=TRUE)

result_att6 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + rainydays + above_avg_days ,  # Covariates
  allow_unbalanced_panel = TRUE
)
aggte(result_att6, type = "dynamic", na.rm=TRUE)


df_mn_unbalanced <- df_sdid_mn

save(df_mn_unbalanced, file="df_mn_unbalanced.RData")

summary(df_sdid_mn)
summary(df_mn_unbalanced)

















###############################################################################
# Monitoring Station Level

df_final_mn_did <- df_final_mn

# Set the adoption year as 0 for the control group  
df_final_mn_did %<>% mutate(AdoptionYear = ifelse(Treated==0, 0, AdoptionYear))
table(df_final_mn_did$AdoptionYear, useNA = "ifany")


# 1. Simple & balanced panel
result_att1 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_final_mn_did,                 # Dataset
)
aggte(result_att1, type = "dynamic", na.rm=TRUE)

result_att1 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  control_group="notyettreated", # default is never treated
  data = df_final_mn_did,                 # Dataset
)
aggte(result_att1, type = "dynamic", na.rm=TRUE)


# 2. Simple & unbalanced panel
result_att2 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_final_mn_did,                 # Dataset
  allow_unbalanced_panel = TRUE,
)
aggte(result_att2, type = "dynamic", na.rm=TRUE)


# 3. Covariates & balanced panel
result_att3 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_final_mn_did,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf,  # Covariates
)
aggte(result_att3, type = "dynamic", na.rm=TRUE)

# Error 1.
# Error in pre_process_did(yname = yname, tname = tname, idname = idname,  : 
#                            never treated group is too small, try setting control_group="notyettreated"
# Error 2.
# No pre-treatment periods to test

# 4. Covariates & unbalanced panel
result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_final_mn_did,                 # Dataset
  control_group="notyettreated", # default is never treated
  allow_unbalanced_panel = TRUE,
  xformla = ~ ppt + ppt2 + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)

# Error 1.
# Error in qr.solve(crossprod(wols.x.pre, int.cov)/n) : 
#   singular matrix 'a' in solve



######################33 years#################################
##########################################################################
#station level data (23 consecutive observations from 2000)
##########################################################################

# 1. load the main data
df_tmean <- df_final_mn %>% select(ID, Year, tmean)

# 2. filter for 33 consecutive observations
df_stations_33obs <- list_of_dfs_mn_1990[["stations_33_obs"]] %>%  
  select(Treated, Tstatus, StateCd, HUCEgDC, yrlyPlv, Year, ID, AdoptionYear, State_full, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`) %>% 
  filter(AdoptionYear>2000) %>% # Remove the states who adopted  before 2000
  filter(Year>1989) %>% 
  filter(State_full != "Wisconsin") %>% 
  left_join(df_tmean, by = c("ID" = "ID", "Year" = "Year"))  


# How many unique stations? 6.
df_stations_33obs %>% select(ID) %>% distinct() %>% count()
table(df_stations_33obs$State_full)
table(df_stations_33obs$AdoptionYear)
table(df_stations_33obs$Year)

# 3. load the control state data
df_control_pptmean 

# 4. filter for the 23 consecutive observations
# Processed in Summary_stats_on_the_number_of_waterbodies(stations)
df_ctrlmn_23obs <- list_of_dfs_ctrlmn_2000[["mn_23_obs"]] %>% 
  mutate(ppt2=ppt*ppt, AdoptionYear=0) %>%  # Set the adoption year as 0 for the control group  
  select(yearlyPlevel, Year, ID, AdoptionYear, State_full, StateCode, HUCEightDigitCode, ppt, ppt2, above_avg_days, below_avg_days, rainydays, t_05, t_510, t_1015, t_1520, t_2025, t_25Inf, `t_-Inf0`) %>% 
  rename(yrlyPlv = yearlyPlevel, StateCd = StateCode, HUCEgDC = HUCEightDigitCode) %>% 
  filter(Year > 1999) %>% 
  mutate(Tstatus = 0, Treated = 0) %>% 
  left_join(tmean_control, by = c("ID" = "ID", "Year" = "Year"))  # 1771 
table(df_ctrlmn_23obs$State_full)
# Alabama       Arkansas    Connecticut          Idaho        Indiana           Iowa 
# 23            138            506             23             92             69 
# Kansas       Kentucky      Louisiana       Maryland    Mississippi       New York 
# 92             23             69             92             46             23 
# North Carolina   North Dakota           Ohio   Pennsylvania          Texas 
# 115            345             23             23             69 
# How many unique stations? 77
View(summary_list2_ctrlmn_2000[["summary_23_years"]])
df_ctrlmn_23obs %>% select(ID) %>% distinct() %>% count()

# # Selecting the 'mn_23_obs' data frame from the list
# df_mn_23_obs <- list_of_dfs_ctrlmn_2000[["mn_23_obs"]] 
# 
# # Performing an anti_join to find IDs in df_ctrlmn_23obs not in df_mn_23_obs
# unique_ids <- anti_join(df_mn_23_obs, df_ctrlmn_23obs,  by = "ID")
# 
# # Viewing the result
# unique_ids
# 
# df_mn_23_obs %<>% select(ID, Year, yearlyPlevel, everything()) %>% 
#   arrange(ID, Year) # 47505
# df_mn_42_obs <- list_of_dfs_ctrlmn[["mn_42_obs"]] %>% 
#                 select(ID, Year, yearlyPlevel, everything()) %>% 
#                 arrange(ID, Year)
# df_stations_42obs <- list_of_dfs_mn[["stations_42_obs"]] %>% 
#                       select(ID, Year, yrlyPlv, everything()) %>% 
#                       arrange(ID, Year)
# df_stations_23obs <- list_of_dfs_mn_2000[["stations_23_obs"]] %>% 
#   select(ID, Year, yrlyPlv, everything()) %>% 
#   arrange(ID, Year) %>% 
#   filter(Year > 1999)


# 5. append them.
df_sdid_mn <- rbind(df_stations_23obs, df_ctrlmn_23obs) 

# Check the missing variables
table(df_sdid_mn$AdoptionYear, useNA = "ifany")
#     0 2002 2010 2014 2020 
# 1771   46  184  115   23
table(df_sdid_mn$Year, useNA = "ifany")

for (col in names(df_sdid_mn)) {
  var_na <- sum(is.na(df_sdid_mn[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}

#######################Staggered DID#######################################
# 1. Simple & balanced panel
result_att3 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
)
# Warning messages:
#   1: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
#                           Be aware that there are some small groups in your dataset.
#                         Check groups: 2002,2020.
#                         2: In att_gt(yname = "yrlyPlv", tname = "Year", idname = "ID", gname = "AdoptionYear",  :
#                                        Not returning pre-test Wald statistic due to singular covariance matrix

aggte(result_att3, type = "dynamic", na.rm=TRUE)

# plot the results
ggdid(result_att3)

result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  # xformla = ~ State_full,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)


# 2. Covariates & balanced panel
result_att4 <- att_gt(
  yname = "yrlyPlv",               # Dependent variable: yearly Phosphorus level
  tname = "Year",                  # Time variable
  idname = "ID",             # ID variable
  gname = "AdoptionYear",          # Group variable (time of first treatment)
  data = df_sdid_mn,                 # Dataset
  # control_group="notyettreated", # default is never treated
  xformla = ~ State_full + ppt + ppt2 + rainydays + above_avg_days + t_05 + t_510 + t_1015 + t_1520 + t_2025 + t_25Inf,  # Covariates
)
aggte(result_att4, type = "dynamic", na.rm=TRUE)
