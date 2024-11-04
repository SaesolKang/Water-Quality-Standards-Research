library(pacman)
p_load(dplyr, lubridate, tidyr, magrittr) 
       
###############################################################################
# Waterbody level
###############################################################################

HUCE_summary <- df_final %>%
                group_by(HUCE_cluster) %>%
                summarise(
                  Total_UniqueIDs = n_distinct(UniqueID)
                ) 
summary(HUCE_summary$Total_UniqueIDs)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   5.000   7.023   9.000  73.000 

HUCE_summary2 <- df_final %>%
                  group_by(HUCE_cluster) %>%
                  select(UniqueID, Treated, HUCE_cluster) %>% 
                  distinct() %>% 
                  group_by(HUCE_cluster) %>% 
                  summarise(
                  Treated_UniqueIDs = sum(Treated==1)
                  ) 

HUCE_summary3 <- HUCE_summary %>% left_join(HUCE_summary2, by = "HUCE_cluster") %>% 
                  group_by(HUCE_cluster) %>% 
                  summarize(
                    Proportion_Treated = Treated_UniqueIDs / Total_UniqueIDs
                  )
summary(HUCE_summary3$Proportion_Treated)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.3758  0.9412  1.0000 

###############################################################################
# Monitoring station level
###############################################################################

HUCE_summary_mn <- df_final_mn %>%
  group_by(HUCEgDC) %>%
  summarise(
    Total_IDs = n_distinct(ID)
  ) # Count the number of stations within each watershed

summary(HUCE_summary_mn$Total_IDs)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    4.00   10.00   24.76   27.00  451.00 

HUCE_summary2_mn <- df_final_mn %>%
  group_by(HUCEgDC) %>%
  select(ID, Treated, HUCEgDC) %>% 
  distinct() %>% 
  group_by(HUCEgDC) %>% 
  summarise(
    Treated_IDs = sum(Treated==1)
  ) # Count the number of treated stations

HUCE_summary3_mn <- HUCE_summary_mn %>% left_join(HUCE_summary2_mn, by = "HUCEgDC") %>% 
  group_by(HUCEgDC) %>% 
  summarize(
    Proportion_Treated = Treated_IDs / Total_IDs
  ) # Count the proportion

summary(HUCE_summary3_mn$Proportion_Treated)

# Merge with the main data
df_final_mn %<>% left_join(HUCE_summary3_mn, by = "HUCEgDC") 
df_huc %<>% left_join(HUCE_summary3_mn, by = "HUCEgDC") 

# Save the list as an RData file
save(HUCE_summary3, file = "HUCE_ratio_wb.RData")
save(HUCE_summary3_mn, file = "HUCE_ratio_mn.RData")





state_summary_mn <- df_final_mn %>%
  group_by(State_full) %>%
  select(State_full, ID) %>% 
  distinct() %>% 
  summarise(
    Total_IDs = n_distinct(ID)
  ) # Count the number of stations within each watershed

Treated_summary_mn <- df_final_mn %>%
  group_by(State_full) %>%
  select(ID, Treated) %>% 
  distinct() %>% 
  summarise(
    Treated_IDs = sum(Treated==1),
    Total_IDs = n_distinct(ID),
    Ratio = Treated_IDs / Total_IDs
    
  ) 


summary(state_summary_mn$Total_IDs)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    4.00   10.00   24.76   27.00  451.00 

HUCE_summary2_mn <- df_final_mn %>%
  group_by(HUCEgDC) %>%
  select(ID, Treated, HUCEgDC) %>% 
  distinct() %>% 
  group_by(HUCEgDC) %>% 
  summarise(
    Treated_IDs = sum(Treated==1)
  ) # Count the number of treated stations

HUCE_summary3_mn <- HUCE_summary_mn %>% left_join(HUCE_summary2_mn, by = "HUCEgDC") %>% 
  group_by(HUCEgDC) %>% 
  summarize(
    Proportion_Treated = Treated_IDs / Total_IDs
  ) # Count the proportion

summary(HUCE_summary3_mn$Proportion_Treated)

