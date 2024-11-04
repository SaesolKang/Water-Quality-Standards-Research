####################Set up############################################################
# as data frame
df_final <- as.data.frame(df_final) # 41420 

# Count the unique waterbodies
df_final %>%  distinct(UniqueID) %>%  nrow()# Define the lookup table for state codes and names

# Count the unique stations
df_final_mn %>%  distinct(ID) %>%  nrow()# Define the lookup table for state codes and names

state_lookup <- data.frame(
  StateCd = c("04", "06", "12", "30", "32", "34", "40", "49", "55"),  # example state codes
  StateName = c("Arizona", "California", "Florida", "Montana", "Nevada", "New Jersey", "Oklahoma", "Utah", "Wisconsin")  # corresponding state names
)

# Get unique state codes from the dataframe
state_codes <- unique(df_final_mn$StateCd)

##################Proportion of the treated per state#############################
# Loop over each state code
for (state_code in state_codes) {
  # Get the state name from the lookup table
  state_name <- filter(state_lookup, StateCd == state_code)$StateName
  # Ensure state_name is a single string, not a dataframe or vector
  state_name <- as.character(state_name)
  
  # Monitoring station treated ratio
  station_ratio <- df_final_mn %>% 
    filter(StateCd == state_code) %>% 
    distinct(ID, .keep_all = TRUE) %>% 
    summarise(
      total = n(),
      treated = sum(Treated == 1, na.rm = TRUE),
      proportion = treated / total
    ) 
  
    print(paste(state_name, "proportion for treated stations:", station_ratio$proportion, station_ratio$total))
  
  # Waterbody treated ratio
  wb_ratio <- df_final %>% # Use df_ppt for now as temperature is not available
    filter(StateCd == state_code) %>%
    distinct(UniqueID, .keep_all = TRUE) %>%
    summarise(
      total = n(),
      treated = sum(Treated == 1, na.rm = TRUE),
      proportion = treated / total
    )
    print(paste(state_name, "proportion for treated waterbodies:", wb_ratio$proportion, wb_ratio$total))
}

########### Compute the before and after p levels by treatment status ##########
adoption_dates <- States %>%
  filter(Code %in% Statecode_interest) %>%  
  select(Code, AdoptionDate, State_full) %>%  
  mutate(AdoptionYear = year(AdoptionDate)) %>% 
  select(Code, AdoptionYear, State_full)
as.data.frame()  
# 
# adoption_year <- adoption_dates %>% filter(Code =="04") %>% pull(Year)
# 
# AZ_Plevel <- df_filtered_spatialjoin %>%
#   filter(StateCd == "04") %>%
#   mutate(Period = if_else(Year < adoption_year, "Before", "After")) %>%
#   group_by(Treated, Period) %>%
#   summarize(Avg_P_level = mean(yrlyPlv, na.rm = TRUE), .groups = 'drop')  
#   
# print(AZ_Plevel)
###########Provider distribution###################
# Loop over each state code in adoption_dates
results_list <- list()  # Initialize a list to store results for each state

# station level
for (code in unique(adoption_dates$Code)) {
  
  # Get the state name from the lookup table
  state_name <- filter(adoption_dates, Code == code)$State_full
  
  # Process phosphorus levels data for the state
  provider_counts <- df_ppt %>%
    filter(StateCd == code) %>%
    pull(PrvdrNm) %>% 
    table()  
    
  total_non_na <- sum(provider_counts)
  
  prvdrnm_proportions <- provider_counts / total_non_na
  
  # Store results in a list with the state code as the key
  results_list[[code]] <- prvdrnm_proportions
  
  # Optionally print results for each state
  print(paste(state_name, "Station level provider distribution:"))
  print(prvdrnm_proportions)
}

# temporary data for wb level analysis
df_wb_summary <- df_final %>% 
  group_by(UniqueID, StateCd, Treated) %>%
  summarize(obs=n(), .groups = "drop") %>% 
    ungroup() 

# waterbody level
for (code in unique(adoption_dates$Code)) {
  
  # Get the state name from the lookup table
  state_name <- filter(adoption_dates, Code == code)$State_full
  state_name <- as.character(state_name)
  
  # Process phosphorus levels data for the state
  provider_counts <- df_wb_summary %>%
    filter(StateCd == code) %>%
    pull(PrvdrNm) %>% 
    table()  
  
  total_non_na <- sum(provider_counts)
  
  prvdrnm_proportions <- provider_counts / total_non_na
  
  # Store results in a list with the state code as the key
  results_list[[code]] <- prvdrnm_proportions
  
  # Optionally print results for each state
  print(paste(state_name, "WB level provider distribution:"))
  print(prvdrnm_proportions)
}

#################Station Level#####################################
# Loop over each state code in adoption_dates
results_list <- list()  # Initialize a list to store results for each state

# Regression dataset
for (code in unique(adoption_dates$Code)) {
  
  # Get the state name from the lookup table
  state_name <- filter(adoption_dates, Code == code)$State_full

  # Extract the adoption year for the current state
  adoption_year <- filter(adoption_dates, Code == code)$Year
  
  # Process phosphorus levels data for the state
  state_Plevel <- df_ppt %>%
    filter(StateCd == code) %>%
    group_by(Treated, Year) %>%
    summarize(yrly_P_level = mean(yrlyPlv, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(Period = if_else(Year < adoption_year, "Before", "After")) %>%
    group_by(Treated, Period) %>%
    summarize(Avg_P_level = mean(yrly_P_level, na.rm = TRUE), .groups = 'drop')
  
  # Store results in a list with the state code as the key
  results_list[[code]] <- state_Plevel
  
  # Optionally print results for each state
  print(paste(state_name, "Station level Results:"))
  print(state_Plevel)
}

# df_filtered_spatialjoin
for (code in unique(adoption_dates$Code)) {
  
  # Extract the adoption year for the current state
  adoption_year <- adoption_dates %>%
    filter(Code == code) %>%
    pull(Year)
  
  # Process phosphorus levels data for the state
  state_Plevel <- df_filtered_spatialjoin %>%
    filter(StateCd == code) %>%
    group_by(Treated, Year) %>%
    summarize(yrly_P_level = mean(yrlyPlv, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(Period = if_else(Year < adoption_year, "Before", "After")) %>%
    group_by(Treated, Period) %>%
    summarize(Avg_P_level = mean(yrly_P_level, na.rm = TRUE), .groups = 'drop')
  
  # Store results in a list with the state code as the key
  results_list[[code]] <- state_Plevel
  
  # Optionally print results for each state
  print(paste("Results for State Code:", code))
  print(state_Plevel)
}

##############Waterbody Level###################################
results_list <- list()  # Initialize a list to store results for each state

# Regression data 
for (code in unique(adoption_dates$Code)) {

  # Get the state name from the lookup table
  state_name <- filter(adoption_dates, Code == code) %>% 
    pull(State_full)  # Ensure state_name is a single string
  
  # Extract the adoption year for the current state
  adoption_year <- filter(adoption_dates, Code == code) %>% 
    pull(AdoptionYear)  # Ensure adoption_year is a single value
  
  # Process phosphorus levels data for the state
  state_Plevel <- df_final %>%
    filter(StateCd == code) %>%
    group_by(Treated, Year) %>%
    summarize(yrly_P_level = mean(yrlyPlv, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(Period = if_else(Year < adoption_year, "Before", "After")) %>%
    group_by(Treated, Period) %>%
    summarize(Avg_P_level = mean(yrly_P_level, na.rm = TRUE), .groups = 'drop')
  
  # Store results in a list with the state code as the key
  # results_list[[code]] <- state_Plevel
  
  # Optionally print results for each state
  print(paste(state_name, "waterbody level Results:"))
  print(state_Plevel)
}


# Check the number of yearly observation per station
station_obs <- df_final_mn %>% 
  filter(StateCd == "04") %>% 
  group_by(ID) %>% 
  summarize(obs = n(), .groups = "drop")


# Ohio number of obs for the control group   
df_ppt %>%
  filter(StateCd == "40", Treated== "0") %>%
  pull(Year) %>%  # Extract the Year column as a vector
  table() %>%     # Create a frequency table of years
  print()         # Print the table
# 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 
# 39   22   28   39   29   23   23   29   25   65  222  198   99   20   16   15   16 
# 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 
# 9   22   12   11   17   22   19   21   40   20   23   45   44   27   37   39  162 
# 2015 2016 2017 2018 2019 2020 2021 2022 
# 180   50  119  119  162  143   89  100 


# Generate the obs distribution plots per
# Filter the data, pull the Year, create a frequency table, and convert it to a dataframe for plotting
year_data <- df_final_mn %>%
  filter(StateCd == "04", Treated == "0") %>%
  pull(Year) %>%
  table() %>%
  as.data.frame()  

# Renaming columns for clarity
names(year_data) <- c("Year", "Frequency")

# Create a bar plot of year frequencies
ggplot(year_data, aes(x = Year, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Year", y = "Frequency", title = "Frequency of Years for Non-Treated in StateCd 40") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

# Optionally, you can print the year data as well
print(year_data)

# headstream
table(df_final_mn$Tstatus, df_final_mn$Headstream)
for (name in names(MN_final)) {
  df <- MN_final[[name]]
  print(paste("Processing data frame for:", name))
  print(table(df$Tstatus, df$Headstream))
}

# precipitation per state
ppt_summary <- df_final_mn %>% 
  group_by(State_full, Year) %>% 
  summarize(precipitation = mean(ppt, na.rm = TRUE))
# Create the plot
ggplot(ppt_summary, aes(x = Year, y = precipitation, color = as.factor(State_full), group = State_full)) +
  geom_line() +
  labs(title = "Average Precipitation Over Time by State",
       x = "Year",
       y = "Average Precipitation",
       color = "State") +
  theme_minimal() +
  scale_color_viridis_d(
    option = "turbo",  # Use the color option from viridis
  )

# temperature per state
tmean_summary <- df_final_mn %>% 
  group_by(State_full, Year) %>% 
  summarize(temperature = mean(tmean, na.rm = TRUE))
# Create the plot
ggplot(tmean_summary, aes(x = Year, y = temperature, color = as.factor(State_full), group = State_full)) +
  geom_line() +
  labs(title = "Average Temperature Over Time by State",
       x = "Year",
       y = "Average Temperature",
       color = "State") +
  theme_minimal() +
  scale_color_viridis_d(
    option = "turbo",  # Use the color option from viridis
  )
