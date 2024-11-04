library(dplyr)
library(purrr)  # Ensure this library is loaded for the map functions

###############################################################################
# preliminary
###############################################################################

#1. Count the number of observations per year

df_final %>% select(State_full) %>% distinct()
# 1 Florida   
# 2 Oklahoma
# 3 Arizona   
# 4 California
# 5 Nevada    
# 6 Wisconsin # statewide
# 7 Montana   
# 8 New Jersey # statewide
# 9 Utah    

waterbody_obs <- df_final %>% 
                group_by(Year) %>% 
                summarize(obs = n(), .groups = "drop")
View(waterbody_obs)
ggplot(waterbody_obs, aes(x = Year, y = obs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Year", y = "obs", title = "Frequency of Years across 9 states") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

#1. Count the number of observations per year

df_final_mn %>% select(State_full) %>% distinct()
# 1 Florida   
# 2 Oklahoma
# 3 Arizona   
# 4 California
# 5 Nevada    
# 6 Wisconsin # statewide
# 7 Montana   
# 8 New Jersey # statewide
# 9 Utah    

station_obs <- df_final_mn %>% 
  group_by(Year) %>% 
  summarize(obs = n(), .groups = "drop")
View(station_obs)
ggplot(station_obs, aes(x = Year, y = obs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Year", y = "obs", title = "Frequency of Years across 9 states") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility


#######################################################################
# Count the number of stations/watersheds/waterbodies that have consecutive observations
#######################################################################


############################################################################
# Loop (station)
############################################################################

# Initialize list to store the data frames for each number of observations
list_of_dfs_mn <- list()
summary_list_mn <- list()  # List to store state-specific summaries
summary_list2_mn <- list() # List to store across-state summaries
year_combination_list <- list() 

# Starting and ending years for the full dataset
start_year <- 1981
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      # filter(State_full != "Wisconsin" & State_full != "New Jersey") %>% 
      group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_mn[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name
      summary_list_mn[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      
      summary_list2_mn[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}

# Confirm that the total number of unique waterbodies equals the sum of summary_df2
df_final_mn %>% summarise(ID_count = n_distinct(ID), .groups = 'drop') # 12229
do.call(rbind, summary_list2_mn) %>% 
  summarise(ID_count = n_distinct(ID), .groups = 'drop') # 12229


save(summary_list, summary_list2, summary_list_mn, summary_list2_mn, file = "summary_lists.RData")


#################################################### from 2000
# Initialize list to store the data frames for each number of observations
list_of_dfs_mn_2000 <- list()
summary_list_mn_2000 <- list()  # List to store state-specific summaries
summary_list2_mn_2000 <- list() # List to store across-state summaries
year_combination_list_2000 <- list() 

# Starting and ending years for the full dataset
start_year <- 2000
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_2000[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      filter(State_full != "Wisconsin" & State_full != "New Jersey") %>% 
      group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_mn_2000[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name
      summary_list_mn_2000[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      
      summary_list2_mn_2000[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}

#################################################### from 1990
# Initialize list to store the data frames for each number of observations
list_of_dfs_mn_1990 <- list()
summary_list_mn_1990 <- list()  # List to store state-specific summaries
summary_list2_mn_1990 <- list() # List to store across-state summaries
year_combination_list_1990 <- list() 

# Starting and ending years for the full dataset
start_year <- 1990
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_2000[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      filter(State_full != "Wisconsin" & State_full != "New Jersey") %>% 
      group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_mn_1990[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name
      summary_list_mn_1990[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      
      summary_list2_mn_1990[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}

#################################################### from 1995
# Initialize list to store the data frames for each number of observations
list_of_dfs_mn_1995 <- list()
summary_list_mn_1995 <- list()  # List to store state-specific summaries
summary_list2_mn_1995 <- list() # List to store across-state summaries
year_combination_list_1995 <- list() 

# Starting and ending years for the full dataset
start_year <- 1995
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_1995[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      filter(State_full != "Wisconsin" & State_full != "New Jersey") %>% 
            group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_mn_1995[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name
      summary_list_mn_1995[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      
      summary_list2_mn_1995[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}

#################################################### from 1996
# Initialize list to store the data frames for each number of observations
list_of_dfs_mn_1996 <- list()
summary_list_mn_1996 <- list()  # List to store state-specific summaries
summary_list2_mn_1996 <- list() # List to store across-state summaries
year_combination_list_1996 <- list() 

# Starting and ending years for the full dataset
start_year <- 1996
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_1996[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      filter(State_full != "Wisconsin" & State_full != "New Jersey") %>% 
      group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_mn_1996[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name
      summary_list_mn_1996[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      
      summary_list2_mn_1996[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}

#################################################### from 1996 to 2020
# Initialize list to store the data frames for each number of observations
list_of_dfs_mn_199620 <- list()
summary_list_mn_199620 <- list()  # List to store state-specific summaries
summary_list2_mn_199620 <- list() # List to store across-state summaries
year_combination_list_199620 <- list() 

# Starting and ending years for the full dataset
start_year <- 1996
end_year <- 2020

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_199620[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      filter(State_full != "Wisconsin" & State_full != "New Jersey") %>% 
      group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_mn_199620[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name
      summary_list_mn_199620[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      
      summary_list2_mn_199620[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}

#################################################### from 1997
# Initialize list to store the data frames for each number of observations
list_of_dfs_mn_1997 <- list()
summary_list_mn_1997 <- list()  # List to store state-specific summaries
summary_list2_mn_1997 <- list() # List to store across-state summaries
year_combination_list_1997 <- list() 

# Starting and ending years for the full dataset
start_year <- 1997
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_1997[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      filter(State_full != "Wisconsin" & State_full != "New Jersey") %>% 
      group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_mn_1997[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name
      summary_list_mn_1997[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      
      summary_list2_mn_1997[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}

#################################################### from 1998
# Initialize list to store the data frames for each number of observations
list_of_dfs_mn_1998 <- list()
summary_list_mn_1998 <- list()  # List to store state-specific summaries
summary_list2_mn_1998 <- list() # List to store across-state summaries
year_combination_list_1998 <- list() 

# Starting and ending years for the full dataset
start_year <- 1998
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_1998[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      filter(State_full != "Wisconsin" & State_full != "New Jersey") %>% 
      group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_mn_1998[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name
      summary_list_mn_1998[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      
      summary_list2_mn_1998[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}

#################################################### from 1999
# Initialize list to store the data frames for each number of observations
list_of_dfs_mn_1999 <- list()
summary_list_mn_1999 <- list()  # List to store state-specific summaries
summary_list2_mn_1999 <- list() # List to store across-state summaries
year_combination_list_1999 <- list() 

# Starting and ending years for the full dataset
start_year <- 1999
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_1999[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      filter(State_full != "Wisconsin" & State_full != "New Jersey") %>% 
      group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_mn_1999[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name
      summary_list_mn_1999[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      
      summary_list2_mn_1999[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}

################################################################################
# Loop (watershed level)
################################################################################
# Note: when aggregating the data to the huc level, I used station level data 
# rather than waterbody level data because some water segments cross multiple
# watersheds.

# Initialize list to store the data frames for each number of observations
list_of_dfs_huc <- list()
summary_list_huc <- list()  # List to store state-specific summaries
summary_list2_huc <- list() # List to store across-state summaries

# Starting and ending years for the full dataset
start_year <- 1981
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_huc %>%
      group_by(HUCEgDC) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs_huc[[paste("watershed", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(HUCE_count = n_distinct(HUCEgDC), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(HUCEgDC) %>%
        summarise(HUCE_count = n_distinct(HUCEgDC), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list_huc[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2_huc[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}

# Confirm that the total number of watersheds equals the sum of summary_df2
df_huc %>% select(HUCEgDC) %>% distinct(HUCEgDC) %>%  nrow() # 495

do.call(rbind, summary_list2_huc) %>% 
  summarise(HUC_count = n_distinct(HUCEgDC), .groups = 'drop') # 495


rbind_huc <- do.call(rbind, summary_list2_huc) %>% distinct() 

# that are in df_final$HUCEgDC but not in summary_list2_huc$HUCEgDC
setdiff(
  unique(df_final$HUCEgDC),
  unique(rbind_huc$HUCEgDC)
)
setdiff(
  unique(rbind_huc$HUCEgDC),
  unique(df_final$HUCEgDC)
)

huc_check <- df_final_mn %>% select(HUCEgDC) %>% distinct(HUCEgDC)

# Left join and filter for unmatched rows
unmatched_rows <- left_join(huc_check, rbind_huc, by = "HUCEgDC") %>%
  filter(is.na(HUCE_count)) # replace `column_name` with the name of a key column from summary_list2_huc that should have data if the match was successful.

# View the unmatched rows
print(unmatched_rows)
# HUCEgDC         HUCE_count
# 1 10070007         NA
# 2 10040101         NA
# 3 10070004         NA

################################################ from 2000
# Initialize list to store the data frames for each number of observations
list_of_dfs_huc_2000 <- list()
summary_list_huc_2000 <- list()  # List to store state-specific summaries
summary_list2_huc_2000 <- list() # List to store across-state summaries

# Starting and ending years for the full dataset
start_year <- 2000
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_huc %>%
      group_by(HUCEgDC) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs_huc_2000[[paste("watershed", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(HUCE_count = n_distinct(HUCEgDC), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(HUCEgDC) %>%
        summarise(HUCE_count = n_distinct(HUCEgDC), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list_huc_2000[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2_huc_2000[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}


################################################ from 1990
# Initialize list to store the data frames for each number of observations
list_of_dfs_huc_1990 <- list()
summary_list_huc_1990 <- list()  # List to store state-specific summaries
summary_list2_huc_1990 <- list() # List to store across-state summaries

# Starting and ending years for the full dataset
start_year <- 1990
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_huc %>%
      group_by(HUCEgDC) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs_huc_1990[[paste("watershed", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(HUCE_count = n_distinct(HUCEgDC), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(HUCEgDC) %>%
        summarise(HUCE_count = n_distinct(HUCEgDC), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list_huc_1990[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2_huc_1990[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}

################################################ from 2000
# Initialize list to store the data frames for each number of observations
list_of_dfs_huc_2000 <- list()
summary_list_huc_2000 <- list()  # List to store state-specific summaries
summary_list2_huc_2000 <- list() # List to store across-state summaries

# Starting and ending years for the full dataset
start_year <- 2000
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_huc %>%
      group_by(HUCEgDC) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs_huc_2000[[paste("watershed", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(HUCE_count = n_distinct(HUCEgDC), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(HUCEgDC) %>%
        summarise(HUCE_count = n_distinct(HUCEgDC), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list_huc_2000[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2_huc_2000[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}

################Waterbody########################

# Initialize list to store the data frames for each number of observations
list_of_dfs <- list()
summary_list <- list()  # List to store state-specific summaries
summary_list2 <- list() # List to store across-state summaries
year_combination_list <- list() 

# Starting and ending years for the full dataset
start_year <- 2000
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final %>%
      group_by(UniqueID) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs[[paste("wb", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(UniqueID_count = n_distinct(UniqueID), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(UniqueID) %>%
        summarise(UniqueID_count = n_distinct(UniqueID), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}

# Confirm that the total number of watersheds equals the sum of summary_df2
df_final %>% select(UniqueID) %>% distinct(UniqueID) %>%  nrow() # 3406

do.call(rbind, summary_list2) %>% 
  summarise(WB_count = n_distinct(UniqueID), .groups = 'drop') # 3406



################Control states Watersheds########################

# Initialize list to store the data frames for each number of observations
list_of_dfs_ctrlhuc <- list()
summary_list_ctrlhuc <- list()  # List to store state-specific summaries
summary_list2_ctrlhuc <- list() # List to store across-state summaries
year_combination_list <- list() 

# Starting and ending years for the full dataset
start_year <- 1981
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_control_huc %>%
      group_by(HUCEightDigitCode) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs_ctrlhuc[[paste("huc", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(huc_count = n_distinct(HUCEightDigitCode), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(HUCEightDigitCode) %>%
        summarise(huc_count = n_distinct(HUCEightDigitCode), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list_ctrlhuc[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2_ctrlhuc[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}

# Confirm that the total number of watersheds equals the sum of summary_df2
df_control_huc %>% select(HUCEightDigitCode) %>% distinct(HUCEightDigitCode) %>%  nrow() # 958

do.call(rbind, summary_list2_ctrlhuc) %>% 
  summarise(huc_count = n_distinct(HUCEightDigitCode), .groups = 'drop') # 958


################from 1990
# Initialize list to store the data frames for each number of observations
list_of_dfs_ctrlhuc_1990 <- list()
summary_list_ctrlhuc_1990 <- list()  # List to store state-specific summaries
summary_list2_ctrlhuc_1990 <- list() # List to store across-state summaries
year_combination_list_1990 <- list() 

# Starting and ending years for the full dataset
start_year <- 1990
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_control_huc %>%
      group_by(HUCEightDigitCode) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs_ctrlhuc_1990[[paste("huc", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(huc_count = n_distinct(HUCEightDigitCode), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(HUCEightDigitCode) %>%
        summarise(huc_count = n_distinct(HUCEightDigitCode), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list_ctrlhuc_1990[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2_ctrlhuc_1990[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}

# Confirm that the total number of watersheds equals the sum of summary_df2
df_control_huc %>% select(HUCEightDigitCode) %>% distinct(HUCEightDigitCode) %>%  nrow() # 958

do.call(rbind, summary_list2_ctrlhuc) %>% 
  summarise(huc_count = n_distinct(HUCEightDigitCode), .groups = 'drop') # 958

################Control states Stations########################

# Initialize list to store the data frames for each number of observations
list_of_dfs_ctrlmn <- list()
summary_list_ctrlmn <- list()  # List to store state-specific summaries
summary_list2_ctrlmn <- list() # List to store across-state summaries
year_combination_list <- list() 

# Starting and ending years for the full dataset
start_year <- 1981
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_control_pptmean %>%
      group_by(ID) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs_ctrlmn[[paste("mn", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list_ctrlmn[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2_ctrlmn[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}

# Confirm that the total number of watersheds equals the sum of summary_df2
df_control_pptmean %>% select(ID) %>% distinct(ID) %>%  nrow() # 13694

do.call(rbind, summary_list2_ctrlmn) %>% 
  summarise(ID_count = n_distinct(ID), .groups = 'drop') # 14150

# Merge state full name with summary_list_ctrlmn - code needs revision
# states <- States %>% select(Code, State_full) 
# for (df in summary_list_ctrlmn) {
#   df <- df %>% left_join(states, by = c("StateCode"="Code"))
#   summary_list_ctrlmn[df] <- df
# }

################################################## From 2000
# Initialize list to store the data frames for each number of observations
list_of_dfs_ctrlmn_2000 <- list()
summary_list_ctrlmn_2000 <- list()  # List to store state-specific summaries
summary_list2_ctrlmn_2000 <- list() # List to store across-state summaries
year_combination_list_2000 <- list() 

# Starting and ending years for the full dataset
start_year <- 2000
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_2000[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_control_pptmean %>%
      group_by(ID) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs_ctrlmn_2000[[paste("mn", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list_ctrlmn_2000[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2_ctrlmn_2000[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}

################################################## From 1990
# Initialize list to store the data frames for each number of observations
list_of_dfs_ctrlmn_1990 <- list()
summary_list_ctrlmn_1990 <- list()  # List to store state-specific summaries
summary_list2_ctrlmn_1990 <- list() # List to store across-state summaries
year_combination_list_1990 <- list() 

# Starting and ending years for the full dataset
start_year <- 1990
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_2000[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_control_pptmean %>%
      group_by(ID) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs_ctrlmn_1990[[paste("mn", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list_ctrlmn_1990[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2_ctrlmn_1990[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}

################################################## From 1996
# Initialize list to store the data frames for each number of observations
list_of_dfs_ctrlmn_1996 <- list()
summary_list_ctrlmn_1996 <- list()  # List to store state-specific summaries
summary_list2_ctrlmn_1996 <- list() # List to store across-state summaries
year_combination_list_1996 <- list() 

# Starting and ending years for the full dataset
start_year <- 1996
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list_2000[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_control_pptmean %>%
      group_by(ID) %>%
      # Check if any set of years entirely matches the Year column
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    # Proceed if there are rows in the current data frame
    if (nrow(current_df) > 0) {
      list_of_dfs_ctrlmn_1996[[paste("mn", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df <- current_df %>%
        group_by(State_full) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')
      
      summary_df2 <- current_df %>% 
        group_by(ID) %>%
        summarise(ID_count = n_distinct(ID), .groups = 'drop')
      
      # Append to the summary list with a unique name
      summary_list_ctrlmn_1996[[paste("summary", length(years), "years", sep = "_")]] <- summary_df
      summary_list2_ctrlmn_1996[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
    }
  }
}

#################################################################################
# Summary stats
consecutive42_control_mn <- summary_list2_ctrlmn[["summary_42_years"]] %>% pull(ID)

df_42yrs_control <- df_control_mn %>% 
                    filter(ID %in% consecutive42_control_mn$ID) %>% 
                    select(ID, HUCEightDigitCode) %>% 
                    distinct() # 1353
  
df_42yrs_summary <- df_42yrs_control %>%
  group_by(HUCEightDigitCode) %>%
  summarize(n_stations = n())

save(df_42yrs_control, df_42yrs_summary, df_control_pptmean, file = "control_42yrs_data.RData")

statename <- States %>% select(State, State_full, Code) %>% rename(StateCode = Code)
df_control_mn %<>% left_join(statename, by = "StateCode") 
table(df_control_mn$State_full)

# Confirm the number of unique HUCEgDC in the main dataset
total_hucs <- df_control_mn %>%
  distinct(HUCEightDigitCode) %>%
  nrow() # 958

summary_control <- df_control_mn %>% 
                    group_by(HUCEightDigitCode) %>% 
                    select(HUCEightDigitCode, ID) %>% 
                    distinct() %>% 
                    summarize(huc_size = n()) %>% 
                    ungroup()

plot_control <- summary_control %>% 
                group_by(huc_size) %>% 
                summarize(number_by_hucsize = n()) %>%
                ungroup()

plot(plot_control$huc_size, plot_control$number_by_hucsize, 
     type = "l", 
     xlab = "Number of watersheds", ylab = "Size of watersheds",
     main = "Watershed size distribution")

table(plot_control$huc_size)

# Dig further
consecutive23_controlhuc <- summary_list2_ctrlhuc[["summary_23_years"]] %>% 
                            select(HUCEightDigitCode) %>% 
                            left_join(summary_control, by ="HUCEightDigitCode") # 72

plot_control23 <- consecutive23_controlhuc %>% 
  group_by(huc_size) %>% 
  summarize(number_by_hucsize = n()) %>%
  ungroup()

plot(plot_control23$huc_size, plot_control23$number_by_hucsize, 
     type = "l", 
     xlab = "Size of watersheds", ylab = "Number of watersheds",
     main = "72 Watersheds' size distribution with 23 consecutive observations")
table(plot_control23$huc_size)


consecutive42_controlhuc <- summary_list2_ctrlhuc[["summary_42_years"]] %>% 
  select(HUCEightDigitCode) %>% 
  left_join(summary_control, by ="HUCEightDigitCode") # 72

plot_control42 <- consecutive42_controlhuc %>% 
  group_by(huc_size) %>% 
  summarize(number_by_hucsize = n()) %>%
  ungroup()

plot(plot_control42$huc_size, plot_control42$number_by_hucsize, 
     type = "l", 
     xlab = "Size of watersheds", ylab = "Number of watersheds",
     main = "32 Watersheds' size distribution with 42 consecutive observations")
table(plot_control42$huc_size)



##########################################################################
# Wisconsin Only
##########################################################################
# Initialize list to store the data frames for each number of observations
list_of_dfs_WI <- list()
summary_list2_WI <- list() # List to store across-state summaries
year_combination_list <- list() 

# Starting and ending years for the full dataset
start_year <- 1981
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      filter(State_full == "Wisconsin") %>%
      group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_WI[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name

      summary_list2_WI[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}

df_final_WI %>% summarise(ID_count = n_distinct(ID), .groups = 'drop') # 1372


# Initialize list to store the data frames for each number of observations
list_of_dfs_WI_2000 <- list()
summary_list2_WI_2000 <- list() # List to store across-state summaries
year_combination_list <- list() 

# Starting and ending years for the full dataset
start_year <- 2000
end_year <- 2022

# Maximum number of years to check
max_years <- end_year - start_year + 1

# Loop through the different year counts from 1 to max_years
for (i in 1:max_years) {
  consecutive_years <- i
  year_combinations <- lapply(0:(consecutive_years-1), function(x) seq(start_year + x, end_year - (consecutive_years - 1) + x))
  
  # Store each year combination in a list with a descriptive name
  year_combination_list[[paste("years_combination_", consecutive_years, "year spans", sep = "")]] <- year_combinations
  
  # Filter and process data for each year combination
  for (years in year_combinations) {
    current_df <- df_final_mn %>%
      filter(State_full == "Wisconsin") %>%
      filter(Year>1999) %>% 
      group_by(ID) %>%
      filter(any(map_lgl(year_combinations, ~all(.x %in% Year)))) %>%
      ungroup()
    
    if (nrow(current_df) > 0) {
      list_of_dfs_WI_2000[[paste("stations", length(years), "obs", sep = "_")]] <- current_df
      
      # Create summary and store it in the list
      summary_df2 <- current_df %>% 
        group_by(ID) %>% # You need to run this in order to see the corresponding IDs
        summarise(ID_count = n_distinct(ID), .groups = 'drop')  # Ensure the group by operation is fully handled
      
      
      # Append to the summary list with a unique name
      
      summary_list2_WI_2000[[paste("summary", length(years), "years", sep = "_")]] <- summary_df2
      
    }
  }
}
