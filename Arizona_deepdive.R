# Select monitoring station vs. waterbody level
df_final_AZ <- df_final %>% filter(StateCd=="04")
df_final_AZ <- df_final_mn %>% filter(StateCd=="04")

# Plot the average pollution trend
df_final_AZ %>%
  group_by(Year, Treated, yearlyPlevel) %>% 
  ggplot(aes(x = Year, y = yearlyPlevel)) +  # Ensure variables are correctly mapped in aes()
  geom_line() +  # Add a line plot
  labs(title = "Yearly Phosphorus Levels",
       x = "Year",
       y = "Phosphorus Level") +
  theme_minimal()  # Apply a minimalistic theme

# Plot each treated rivers and streams

# Verde River
df_final_AZ %>%
  filter(Name == "Verde River", Treated == 1) %>%
  ggplot(aes(x = Year, y = yearlyPlevel)) +  # Ensure variables are correctly mapped in aes()
  geom_line() +  # Add a line plot
  labs(title = "Yearly Phosphorus Levels in the Verde River",
       x = "Year",
       y = "Phosphorus Level") +
  theme_minimal()  # Apply a minimalistic theme

      # Filter the data, pull the Year, create a frequency table, and convert it to a dataframe for plotting
      year_data <- df_final_AZ %>%
        filter(Name == "Verde River", Treated == 1) %>%
        pull(Year) %>%
        table() %>%
        as.data.frame()  
      # Renaming columns for clarity
      names(year_data) <- c("Year", "Frequency")
      # Create a bar plot of year frequencies
      ggplot(year_data, aes(x = Year, y = Frequency)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_minimal() +
        labs(x = "Year", y = "Frequency", title = "Frequency of Years for Verde River") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility


# Black River
df_final_AZ %>%
  filter(Name == "Black River", Treated == 1) %>%
  ggplot(aes(x = Year, y = yearlyPlevel)) +  # Ensure variables are correctly mapped in aes()
  geom_line() +  # Add a line plot
  labs(title = "Yearly Phosphorus Levels in the Black River",
       x = "Year",
       y = "Phosphorus Level") +
  theme_minimal()  # Apply a minimalistic theme

      # Obs histogram
      year_data <- df_final_AZ %>%
        filter(Name == "Black River", Treated == 1) %>%
        pull(Year) %>%
        table() %>%
        as.data.frame()  
      # Renaming columns for clarity
      names(year_data) <- c("Year", "Frequency")
      # Create a bar plot of year frequencies
      ggplot(year_data, aes(x = Year, y = Frequency)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_minimal() +
        labs(x = "Year", y = "Frequency", title = "Frequency of Years for Black River") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

# Tonto Creek
df_final_AZ %>%
  filter(Name == "Tonto Creek", Treated == 1) %>%
  ggplot(aes(x = Year, y = yrlyPlv)) +  # Ensure variables are correctly mapped in aes()
  geom_line() +  # Add a line plot
  labs(title = "Yearly Phosphorus Levels in the Tonto Creek",
       x = "Year",
       y = "Phosphorus Level") +
  theme_minimal()  # Apply a minimalistic theme
        
        # Obs histogram
        year_data <- df_final_AZ %>%
          filter(Name == "Tonto Creek", Treated == 1) %>%
          pull(Year) %>%
          table() %>%
          as.data.frame()  
        # Renaming columns for clarity
        names(year_data) <- c("Year", "Frequency")
        # Create a bar plot of year frequencies
        ggplot(year_data, aes(x = Year, y = Frequency)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          theme_minimal() +
          labs(x = "Year", y = "Frequency", title = "Frequency of Years for Tonto Creek") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

# Salt River
df_final_AZ %>%
  filter(Name == "Salt River", Treated == 1) %>%
  ggplot(aes(x = Year, y = yearlyPlevel)) +  # Ensure variables are correctly mapped in aes()
  geom_line() +  # Add a line plot
  labs(title = "Yearly Phosphorus Levels in the Salt River",
       x = "Year",
       y = "Phosphorus Level") +
  theme_minimal()  # Apply a minimalistic theme

      # Obs histogram
      year_data <- df_final_AZ %>%
        filter(Name == "Salt River", Treated == 1) %>%
        pull(Year) %>%
        table() %>%
        as.data.frame()  
      # Renaming columns for clarity
      names(year_data) <- c("Year", "Frequency")
      # Create a bar plot of year frequencies
      ggplot(year_data, aes(x = Year, y = Frequency)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_minimal() +
        labs(x = "Year", y = "Frequency", title = "Frequency of Years for Salt River") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

# Little Colorado River
df_final_AZ %>%
  filter(Name == "Little Colorado River", Treated == 1) %>%
  ggplot(aes(x = Year, y = yearlyPlevel)) +  # Ensure variables are correctly mapped in aes()
  geom_line() +  # Add a line plot
  labs(title = "Yearly Phosphorus Levels in the Little Colorado River",
       x = "Year",
       y = "Phosphorus Level") +
  theme_minimal()  # Apply a minimalistic theme

      # Obs histogram
      year_data <- df_final_AZ %>%
        filter(Name == "Little Colorado River", Treated == 1) %>%
        pull(Year) %>%
        table() %>%
        as.data.frame()  
      # Renaming columns for clarity
      names(year_data) <- c("Year", "Frequency")
      # Create a bar plot of year frequencies
      ggplot(year_data, aes(x = Year, y = Frequency)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_minimal() +
        labs(x = "Year", y = "Frequency", title = "Frequency of Years for Little Colorado River") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

# Colorado River
df_final_AZ %>%
  filter(Name == "Colorado River", Treated == 1) %>%
  ggplot(aes(x = Year, y = yrlyPlv)) +  # Ensure variables are correctly mapped in aes()
  geom_line() +  # Add a line plot
  labs(title = "Yearly Phosphorus Levels in the Colorado River",
       x = "Year",
       y = "Phosphorus Level") +
  theme_minimal()  # Apply a minimalistic theme

# Oak Creek
df_final_AZ %>%
  filter(Name == "Oak Creek", Treated == 1) %>%
  ggplot(aes(x = Year, y = yearlyPlevel)) +  # Ensure variables are correctly mapped in aes()
  geom_line() +  # Add a line plot
  labs(title = "Yearly Phosphorus Levels in the Oak Creek",
       x = "Year",
       y = "Phosphorus Level") +
  theme_minimal()  # Apply a minimalistic theme

# Observation by treated and controlled status

