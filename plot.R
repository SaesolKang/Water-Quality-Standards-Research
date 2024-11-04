library(pacman)
p_load(ggplot2, viridis, magrittr, dplyr, usmap, grid)

######Plots for number of stations before&after adoption by states####
monitoringsites_plots <- list()
state_names <- df_final_mn %>%
  filter(State_full != "New Jersey" & State_full != "Wisconsin") %>%
  distinct(State_full) %>%
  pull(State_full)  # Extract the state names as a character vector

# Loop through each state name
for (state_name in state_names) {
  
  # Get the adoption year from the 'States' dataset
  adoption_year <- States %>%
    filter(State_full == state_name) %>%
    pull(year)  # Extract the year directly as a numeric value
  
  # Compute the number of unique stations for the treated group
  Treated_sites <- df_final_mn %>% 
    filter(State_full == state_name, Treated == 1) %>%
    group_by(Year) %>% 
    summarize(station_number = n_distinct(ID, na.rm = TRUE), .groups = 'drop')
  
  # Compute the number of unique stations for the control group
  Control_sites <- df_final_mn %>% 
    filter(State_full == state_name, Treated == 0) %>%
    group_by(Year) %>% 
    summarize(station_number = n_distinct(ID, na.rm = TRUE), .groups = 'drop')
  
  # Merge treated and control data frames for comparison
  station_n <- merge(Treated_sites, Control_sites, by = "Year", suffixes = c("_treated", "_control"))
  
  # Plot the results using ggplot2 for better visualization
  plot <- ggplot(station_n, aes(x = Year)) +
    geom_line(aes(y = station_number_treated, colour = "Treated")) +
    geom_line(aes(y = station_number_control, colour = "Control")) +
    geom_vline(xintercept = as.numeric(adoption_year), linetype = "dashed", color = "red") +
    labs(x = "Year", y = "Number of Monitoring Sites",
         title = paste("Monitoring Station Number Trends in", state_name),
         colour = "Group") +
    theme_minimal()
  
  # Store plot in list
  monitoringsites_plots[[state_name]] <- plot
}

# Wisconsin (All treated)
# Loop through each state name
  # Get the adoption year from the 'States' dataset
  adoption_year <- States %>%
    filter(State_full == "Wisconsin") %>%
    pull(year)  # Extract the year directly as a numeric value
  
  # Compute the number of unique stations for the treated group
  Treated_sites <- df_final_mn %>% 
    filter(State_full == "Wisconsin", Treated == 1) %>%
    group_by(Year) %>% 
    summarize(station_number = n_distinct(ID, na.rm = TRUE), .groups = 'drop')
  
  # Plot the results using ggplot2 for better visualization
  plot <- ggplot(Treated_sites, aes(x = Year)) +
    geom_line(aes(y = station_number, colour = "Treated")) +
    geom_vline(xintercept = as.numeric(adoption_year), linetype = "dashed", color = "red") +
    labs(x = "Year", y = "Number of Monitoring Sites",
         title = paste("Monitoring Station Number Trends in", "Wisconsin"),
         colour = "Group") +
    theme_minimal()
  
  # Store plot in list
  monitoringsites_plots[["Wisconsin"]] <- plot

# Optionally, print or save plots
for (state_name in names(monitoringsites_plots)) {
  print(monitoringsites_plots[[state_name]])
  # Uncomment to save plots
  # ggsave(filename = paste0("Phosphorus_Level_", state_code, ".png"), plot = plots_list[[state_code]], width = 10, height = 6)
}

                         
########### Plot on the wb level yearly p levels###############
# Import the data
df_plot <- as.data.frame(df_final) # 21319
table(df_plot$StateCd)
df_plot %<>% filter(StateCd!=15) 

                # Plot on the station level
                df_plot2 <- as.data.frame(df_final_mn) # 41121
                table(df_plot2$State_full)
                avg_plevel2 <- df_plot2 %>%
                  group_by(State_full, Year) %>% # each state's yearly p level
                  summarize(State_Plevel = mean(yrlyPlv, na.rm = TRUE), .groups = 'drop') %>% 
                  group_by(Year) %>% # yearly p level across 9 states
                  summarize(Plevel = mean(State_Plevel, na.rm = TRUE), .groups = 'drop') 
                plot(avg_plevel2$Year, avg_plevel2$Plevel, 
                     type = "l", 
                     xlab = "Year", ylab = "Average Plevel",
                     main = "Average Plevel At the Station Level")

                # 1. Compute the p level for each state and each year
                avg_plevel_state <- df_plot2 %>%
                  filter(State_full != "New Jersey" & State_full != "Wisconsin") %>% 
                  group_by(State_full, Year) %>%
                  summarize(State_Plevel = mean(yrlyPlv, na.rm = TRUE), .groups = 'drop') 
                
                # 2. Plot the average Plevel for each state
                unique_states <- unique(avg_plevel_state$State_full)
                
                # 3. Define custom labels for StateCd
                state_labels <- c("04" = "Arizona", "06" = "California", "12" = "Florida", "15" = "Hawaii", "30" = "Montana", "32" = "Nevada", "34" = "New Jersey", "40" = "Oklahoma", "49" = "Utah", "55" = "Wisconsin")  # Add more labels as needed
                state_labels <- c("04" = "Arizona", "06" = "California", "12" = "Florida", "30" = "Montana", "32" = "Nevada", "40" = "Oklahoma", "49" = "Utah")  # Add more labels as needed
                
                # 4. Define colors for each state if needed, or use a single color for simplicity
                state_colors <- c("Arizona" = "red", "California" = "blue", "Florida" = "darkgreen", "Hawaii" = "purple", 
                                  "Montana" = "darkorange", "Nevada" = "brown", "New Jersey" = "magenta", "Oklahoma" = "darkgray", 
                                  "Utah" = "black", "Wisconsin" = "darkblue")
                state_colors <- c("Arizona" = "red", "California" = "blue", "Florida" = "darkgreen",  
                                  "Montana" = "darkorange", "Nevada" = "brown", "Oklahoma" = "darkgray", 
                                  "Utah" = "black")
                
                # 5. Create plots for each state
                for (state in unique_states) {
                  # Filter data for the current state
                  state_data <- subset(avg_plevel_state, State_full == state)
                  
                  # Create plot
                  p <- ggplot(state_data, aes(x = Year, y = State_Plevel, color = State_full)) +
                    geom_line() +
                    scale_color_manual(values = state_colors[state], labels = State_full) +
                    labs(x = "Year", y = "Average Plevel", title = paste("Average Plevel for", state_labels[state]),
                         color = "State_full") +
                    theme_minimal()
                  
                  # Print plot
                  print(p)
                }
                # Overlay 7 states in one plot
                state_labels <- c("04" = "Arizona", "06" = "California", "12" = "Florida", "30" = "Montana", "32" = "Nevada", "40" = "Oklahoma", "49" = "Utah")  # Add more labels as needed
                
                state_colors <- c("04" = "red", "06" = "blue", "12" = "darkgreen", "15" = "purple", 
                                  "30" = "darkorange", "32" = "brown", "34" = "magenta", "40" = "darkgray", 
                                  "49" = "black", "55" = "darkblue")
                
                ggplot(avg_plevel_state, aes(x = Year, y = State_Plevel, color = State_full)) +
                  geom_line() +
                  labs(
                    x = "Year", 
                    y = "Average Total Phosphorus level", 
                    color = "States",
                    title = "Trends in Average Total Phosphorus Levels by State (1981-2022)"
                  ) +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
                  scale_color_viridis_d(
                    option = "turbo",  # Use the color option from viridis
                    labels = state_labels  # Custom labels for each state code
                  )
                # Compute the mean, standard error, and 95% confidence interval
                avg_plevel_state <- df_plot2 %>%
                  filter(State_full != "New Jersey" & State_full != "Wisconsin") %>%
                  group_by(State_full, Year) %>%
                  summarize(
                    State_Plevel = mean(yrlyPlv, na.rm = TRUE),      # Mean phosphorus level
                    se = sd(yrlyPlv, na.rm = TRUE) / sqrt(n()),      # Standard error of the mean
                    # lower_ci = State_Plevel - 1.96 * se,             # Lower bound of the 95% CI
                    # upper_ci = State_Plevel + 1.96 * se,             # Upper bound of the 95% CI
                    lower_ci = State_Plevel - 1.64 * se,             # Lower bound of the 95% CI
                    upper_ci = State_Plevel + 1.64 * se,             # Upper bound of the 95% CI
                    
                       .groups = 'drop'
                  )
                
                # State labels and colors
                state_labels <- c("04" = "Arizona", "06" = "California", "12" = "Florida", "30" = "Montana", 
                                  "32" = "Nevada", "40" = "Oklahoma", "49" = "Utah")  # Add more labels as needed
                
                state_colors <- c("04" = "red", "06" = "blue", "12" = "darkgreen", "15" = "purple", 
                                  "30" = "darkorange", "32" = "brown", "34" = "magenta", 
                                  "40" = "darkgray", "49" = "black", "55" = "darkblue")
                
                # Plot with confidence intervals
                ggplot(avg_plevel_state, aes(x = Year, y = State_Plevel, color = State_full)) +
                  geom_line() +                               # Plot the mean phosphorus levels
                  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = State_full), alpha = 0.2) +  # Add CI ribbons
                  labs(
                    x = "Year", 
                    y = "Average Total Phosphorus Level", 
                    color = "States",
                    title = "Trends in Average Total Phosphorus Levels by State (1981-2022)"
                  ) +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
                  scale_color_viridis_d(
                    option = "turbo",  # Use the color option from viridis
                    labels = state_labels  # Custom labels for each state code
                  ) +
                  scale_fill_viridis_d(
                    option = "turbo", 
                    labels = state_labels, 
                    guide = FALSE  # Hide the legend for the CI fill
                  )
                
# 1. Compute the wb level yearly p levels for the 9 states
avg_plevel <- df_plot %>%
  group_by(StateCd, Year) %>% # each state's yearly p level
  summarize(State_Plevel = mean(yearlyPlevel, na.rm = TRUE), .groups = 'drop') %>% 
  group_by(Year) %>% # yearly p level across 9 states
  summarize(Plevel = mean(State_Plevel, na.rm = TRUE), .groups = 'drop') 
  
# 2. Plot the average Plevel across the states
plot(avg_plevel$Year, avg_plevel$Plevel, 
     type = "l", 
     xlab = "Year", ylab = "Average Plevel",
     main = "Average Plevel Waterbody level")

# 1. Compute the p level for each state and each year
avg_plevel_state <- df_plot %>%
  group_by(StateCd, Year) %>%
  summarize(State_Plevel = mean(yearlyPlevel, na.rm = TRUE), .groups = 'drop') 

# 2. Plot the average Plevel for each state
unique_states <- unique(avg_plevel_state$StateCd)

# 3. Define custom labels for StateCd
state_labels <- c("04" = "Arizona", "06" = "California", "12" = "Florida", "15" = "Hawaii", "30" = "Montana", "32" = "Nevada", "34" = "New Jersey", "40" = "Oklahoma", "49" = "Utah", "55" = "Wisconsin")  # Add more labels as needed

# 4. Define colors for each state if needed, or use a single color for simplicity
state_colors <- c("04" = "red", "06" = "blue", "12" = "darkgreen", "15" = "purple", 
                  "30" = "darkorange", "32" = "brown", "34" = "magenta", "40" = "darkgray", 
                  "49" = "black", "55" = "darkblue")

# 5. Create plots for each state
for (state in unique_states) {
  # Filter data for the current state
  state_data <- subset(avg_plevel_state, StateCd == state)
  
  # Create plot
  p <- ggplot(state_data, aes(x = Year, y = State_Plevel, color = StateCd)) +
    geom_line() +
    scale_color_manual(values = state_colors[state], labels = state_labels) +
    labs(x = "Year", y = "Average Plevel", title = paste("Average Plevel for", state_labels[state]),
         color = "State") +
    theme_minimal()
  
  # Print plot
  print(p)
}

# Print nine statewide plots altogether
p <- ggplot(avg_plevel_state, aes(x = Year, y = State_Plevel)) +
  geom_line() +  # Add lines
  scale_color_manual(values = state_labels, labels = names(state_labels)) +
  facet_wrap(~StateCd, scales = "free_y", labeller = labeller(StateCd = state_labels)) +
  labs(x = "Year", y = "Average Plevel", title = "Average Plevel by State and Year") +
  theme_minimal()
print(p)

# Overlay nine states in one plot
# 1. Define custom labels for StateCd
state_labels <- c("04" = "Arizona", "06" = "California", "12" = "Florida", "30" = "Montana", "32" = "Nevada", "34" = "New Jersey", "40" = "Oklahoma", "49" = "Utah", "55" = "Wisconsin")  

# Default color scheme
ggplot(avg_plevel_state, aes(x = Year, y = State_Plevel, color = StateCd)) +
  geom_line() +
  labs(x = "Year", y = "Average Total Phosphorus level") +
  theme_minimal() +
  scale_color_manual(values = rainbow(length(unique(avg_plevel_state$StateCd))), 
                     labels = state_labels, 
                     name = "States")  
# viridis turbo color scheme

ggplot(avg_plevel_state, aes(x = Year, y = State_Plevel, color = StateCd)) +
  geom_line() +
  labs(
    x = "Year", 
    y = "Average Total Phosphorus level", 
    color = "States",
    title = "Trends in Average Total Phosphorus Levels by State (1981-2022)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  scale_color_viridis_d(
    option = "turbo",  # Use the color option from viridis
    labels = state_labels  # Custom labels for each state code
  )

################BELOW IS FOR PRISM DATA CUTOFF SETUP###########################
# Filter the data frame for the specific UniqueID and Year
half_data <- df_ppt %>% filter(Miles > median(Miles)) 

# Filter the data for UniqueID=27564 and Year=2017
filtered_data <- half_data %>%
  filter(UniqueID == 27564, Year == 2019)

# Create the scatter plot
ggplot(filtered_data, aes(x = ID, y = ppt)) +
  geom_point() +
  labs(x = "Monitoring Station", y = "PPT", title = "PPT for UniqueID 27564 in Year 2017") +
  theme_minimal()

# How many obs in 2017 of 27564?
# Count the number of observations where ID=27564 and Year=2017
df_ppt %>%
  filter(UniqueID == "27564", Year == 2017) %>%
  nrow() # 10

df_ppt %>%
  filter(UniqueID == "27564", Year == 2017) %>%
  print(ID)

ggplot(df_final, aes(x = y_ppt)) +
  geom_density(fill = "skyblue", color = "blue") +
  labs(title = "Density Plot of tmean",
       x = "Annual precipitation",
       y = "Density")

mile_distribution <- df_filtered_wb %>%
  distinct(UniqueID, Miles) %>%
  count(Miles)

ggplot(mile_distribution, aes(x = Miles, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Miles", y = "Frequency", title = "Distribution of Miles") +
  theme_minimal()
summary_stats <- c(
  mean = mean(mile_distribution$Miles),
  median = median(mile_distribution$Miles),
  min = min(mile_distribution$Miles),
  max = max(mile_distribution$Miles)
)
print(summary_stats)
##########################DATA EXPLORATION ENDS HERE############################


# Create a US map showing the criteria adoption status

# # Create a data frame with states and their assigned colors
# state_colors <- data.frame(
#   state = state.abb,  # State abbreviations
#   color = sample(colors(), 50)  # Assign random colors to each state
# )
# 
# # Plot the map with the state colors
# plot_usmap(data = state_colors, values = "color", regions = "states") + 
#   labs(title = "States with Total Nitrogen or Total Phosphorus Criteria") + 
#   theme_void() +  # This sets a completely blank theme
#   theme(
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_fill_identity()  # Use the colors provided in the data


# Create a data frame with states and their assigned colors
state_colors <- data.frame(
  state = state.abb,  # State abbreviations
  color = "white"  # Default color for all states
)

# Set specific colors for Florida and other states
state_colors$color[state_colors$state == "FL"] <- "darkblue"
state_colors$color[state_colors$state == "MN"] <- "darkblue"
state_colors$color[state_colors$state == "WI"] <- "darkblue"
state_colors$color[state_colors$state == "NJ"] <- "darkblue"
state_colors$color[state_colors$state == "HI"] <- "darkblue"

state_colors$color[state_colors$state == "WV"] <- "#5755C0"
state_colors$color[state_colors$state == "VT"] <- "#5755C0"
state_colors$color[state_colors$state == "RI"] <- "#5755C0"

state_colors$color[state_colors$state == "MT"] <- "#B0AFF9"
state_colors$color[state_colors$state == "OR"] <- "#B0AFF9"
state_colors$color[state_colors$state == "CA"] <- "#B0AFF9"
state_colors$color[state_colors$state == "NV"] <- "#B0AFF9"
state_colors$color[state_colors$state == "UT"] <- "#B0AFF9"
state_colors$color[state_colors$state == "CO"] <- "#B0AFF9"
state_colors$color[state_colors$state == "AZ"] <- "#B0AFF9"
state_colors$color[state_colors$state == "NM"] <- "#B0AFF9"
state_colors$color[state_colors$state == "MO"] <- "#B0AFF9"
state_colors$color[state_colors$state == "IL"] <- "#B0AFF9"
state_colors$color[state_colors$state == "MA"] <- "#B0AFF9"
state_colors$color[state_colors$state == "VA"] <- "#B0AFF9"
state_colors$color[state_colors$state == "SC"] <- "#B0AFF9"
state_colors$color[state_colors$state == "GA"] <- "#B0AFF9"

# Plot the map with the updated state colors
plot_usmap(data = state_colors, values = "color", regions = "states") + 
  labs(title = "States with Total Nitrogen or Total Phosphorus Criteria") + 
  theme_void() +  # This sets a completely blank theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = "white"),  # Reduce title text size
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#5C5C5D"),  # Set the background color to grey
    panel.border = element_blank(),  # Remove the default border
    panel.grid = element_blank()
  ) +
  scale_fill_identity() +  # Use the colors provided in the data
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#5C5C5D"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  borders("state", colour = "black", size = 4)  # Thicken the borderlines



states_with_NP <- plot_usmap(data = state_colors, values = "color", regions = "states") + 
  # labs(title = "States with Total Nitrogen or Total Phosphorus Criteria") + 
  labs(title = "") + 
  
  theme_void() +  # This sets a completely blank theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, color = "white"),  # Reduce title text size
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#5C5C5D"),  # Set the background color to grey
    panel.border = element_blank(),  # Remove the default border
    panel.grid = element_blank()
  ) +
  scale_fill_identity() +  # Use the colors provided in the data
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#5C5C5D"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  borders("state", colour = "black", size = 4) +  # Thicken the borderlines
  guides(fill = guide_legend(title = "States"))  # Add legend with title "States"

png("states_with_NP.png")  # Open a PNG device
plot(states_with_NP)       # Create the plot (or use your plotting function)
dev.off()                 # Close the PNG device
# state_colors <- data.frame(
#   state = state.name,
#   color = sample(c("#7E0CA5", "darkblue", "#9e9ac8", "#bcbddc", "#dadaeb"), 50, replace = TRUE)
# )

# Create the main map plot
map_plot <- plot_usmap(data = state_colors, values = "color", regions = "states") + 
  labs(title = "States with Total Nitrogen or Total Phosphorus Criteria") + 
  theme_void() +  # This sets a completely blank theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, color = "white"),  # Reduce title text size
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#5C5C5D"),  # Set the background color to grey
    panel.border = element_blank(),  # Remove the default border
    panel.grid = element_blank()
  ) +
  scale_fill_identity() +  # Use the colors provided in the data
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#5C5C5D"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  borders("state", colour = "black", size = 4) 
print(map_plot)





######Plots for P levels before&after adoption by treatment and control groups####
plots_list <- list()
state_codes <- unique(df_final$StateCd)

for (state_code in state_codes) {
  # Retrieve the corresponding state name from the lookup table
  state_name <- filter(state_lookup, StateCd == state_code)$StateName
  
  # Get the adoption year from the 'States' dataset
  adoption_year <- States %>%
    filter(Code == state_code) %>%
    pull(year)  # Extract the year directly as a numeric value
  
  # Compute average phosphorus levels for the treated group
  Plevel_treated <- df_final %>% 
    filter(StateCd == state_code, Treated == 1) %>%
    group_by(UniqueID, Year) %>% 
    summarize(avg_Plevel = mean(yrlyPlv, na.rm = TRUE), .groups = 'drop') %>%
    group_by(Year) %>%
    summarize(P_treated = mean(avg_Plevel, na.rm = TRUE), .groups = 'drop')
  
  # Compute average phosphorus levels for the control group
  Plevel_control <- df_final %>% 
    filter(StateCd == state_code, Treated == 0) %>%
    group_by(UniqueID, Year) %>% 
    summarize(avg_Plevel = mean(yrlyPlv, na.rm = TRUE), .groups = 'drop') %>%
    group_by(Year) %>%
    summarize(P_control = mean(avg_Plevel, na.rm = TRUE), .groups = 'drop')
  
  # Merge treated and control data frames for comparison
  avg_plevel <- merge(Plevel_treated, Plevel_control, by = "Year")
  
  # Plot the results using ggplot2 for better visualization
  plot <- ggplot(avg_plevel, aes(x = Year)) +
    geom_line(aes(y = P_treated, colour = "Treated")) +
    geom_line(aes(y = P_control, colour = "Control")) +
    geom_vline(xintercept = as.numeric(adoption_year), linetype = "dashed", color = "red") +
    labs(x = "Year", y = "Average Phosphorus Level (Plevel)",
         title = paste("Yearly Phosphorus Levels in", state_name),
         colour = "Group") +
    theme_minimal()
  
  # Store plot in list
  plots_list[[state_code]] <- plot
}

# Optionally, print or save plots
for (state_code in names(plots_list)) {
  print(plots_list[[state_code]])
  # Uncomment to save plots
  # ggsave(filename = paste0("Phosphorus_Level_", state_code, ".png"), plot = plots_list[[state_code]], width = 10, height = 6)
}

############## New Jersey lacks data from 1980 to 2010. Investigate.
# Retrieve the corresponding state name from the lookup table
state_name <- filter(state_lookup, StateCd == "34")$StateName

# Get the adoption year from the 'States' dataset
adoption_year <- States %>%
  filter(Code == "34") %>%
  pull(year)  

# Compute average phosphorus levels for the treated group
Plevel_treated <- df_ppt %>% 
  filter(StateCd == "34", Treated == 1) %>%
  group_by(UniqueID, Year) %>% 
  summarize(avg_Plevel = mean(yrlyPlv, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Year) %>%
  summarize(P_treated = mean(avg_Plevel, na.rm = TRUE), .groups = 'drop')

# Plot the results using ggplot2 for better visualization
plot_NJ <- ggplot(Plevel_treated, aes(x = Year)) +
  geom_line(aes(y = P_treated, colour = "Treated")) +
  geom_vline(xintercept = as.numeric(adoption_year), linetype = "dashed", color = "red") +
  labs(x = "Year", y = "Average Phosphorus Level (Plevel)",
       title = paste("Average Phosphorus Level Over the Years in", state_name),
       colour = "Group") +
  theme_minimal()

print(plot_NJ)



##### Waterbody level Plots of obs numbers by t&c groups in each state
plots_list <- list()
state_codes <- unique(df_ppt$StateCd)

for (state_code in state_codes) {
  state_name <- filter(state_lookup, StateCd == state_code)$StateName %>% as.character()
  adoption_year <- States %>% filter(Code == state_code) %>% pull(year)
  
  Treated_year <- df_wb_summary %>%
    filter(StateCd == state_code, Treated == 1) %>%
    pull(Year) %>%
    table() %>%
    as.data.frame()
  names(Treated_year) <- c("Year", "Frequency_Treated")
  
  Control_year <- df_wb_summary %>%
    filter(StateCd == state_code, Treated == 0) %>%
    pull(Year) %>%
    table() %>%
    as.data.frame()
  names(Control_year) <- c("Year", "Frequency_Control")
  
  # Merge and fill NA values with zero
  Year_obs <- merge(Treated_year, Control_year, by = "Year", all = TRUE)
  Year_obs[is.na(Year_obs)] <- 0
  
  # Plotting with checks for single observation years
  plot <- ggplot(Year_obs, aes(x = Year)) +
    geom_line(aes(y = Frequency_Treated, colour = "Treated")) +
    geom_line(aes(y = Frequency_Control, colour = "Control")) +
    geom_point(aes(y = Frequency_Treated, colour = "Treated")) +
    geom_point(aes(y = Frequency_Control, colour = "Control")) +
    geom_vline(xintercept = adoption_year, linetype = "dashed", color = "red") +
    labs(x = "Year", y = "Frequency of Observations",
         title = paste("Yearly Observations for", state_name),
         colour = "Group") +
    theme_minimal()
  
  plots_list[[state_name]] <- plot
}

# Optionally, print or save plots
for (state_name in names(plots_list)) {
  print(plots_list[[state_name]])
}
# Optionally, print or save plots
for (state_name in names(plots_list)) {
  print(plots_list[[state_name]])
}

#####station level
plots_list <- list()
state_codes <- unique(df_ppt$StateCd)

for (state_code in state_codes) {
  state_name <- filter(state_lookup, StateCd == state_code)$StateName %>% as.character()
  adoption_year <- States %>% filter(Code == state_code) %>% pull(year)
  
  Treated_year <- df_ppt %>%
    filter(StateCd == state_code, Treated == 1) %>%
    pull(Year) %>%
    table() %>%
    as.data.frame()
  names(Treated_year) <- c("Year", "Frequency_Treated")
  
  Control_year <- df_ppt %>%
    filter(StateCd == state_code, Treated == 0) %>%
    pull(Year) %>%
    table() %>%
    as.data.frame()
  names(Control_year) <- c("Year", "Frequency_Control")
  
  # Merge and fill NA values with zero
  Year_obs <- merge(Treated_year, Control_year, by = "Year", all = TRUE)
  Year_obs[is.na(Year_obs)] <- 0
  
  # Plotting with checks for single observation years
  plot <- ggplot(Year_obs, aes(x = Year)) +
    geom_line(aes(y = Frequency_Treated, colour = "Treated")) +
    geom_line(aes(y = Frequency_Control, colour = "Control")) +
    geom_point(aes(y = Frequency_Treated, colour = "Treated")) +
    geom_point(aes(y = Frequency_Control, colour = "Control")) +
    geom_vline(xintercept = adoption_year, linetype = "dashed", color = "red") +
    labs(x = "Year", y = "Frequency of Observations",
         title = paste("Yearly Observations for", state_name),
         colour = "Group") +
    theme_minimal()
  
  plots_list[[state_name]] <- plot
}

# Optionally, print or save plots
for (state_name in names(plots_list)) {
  print(plots_list[[state_name]])
}



# Compute annual temperature for plots
station_shp <- df_filtered_mn %>% st_as_sf()
station_shp <- df_pptmean %>% st_as_sf()

prism_set_dl_dir(paste0(maindir, "/tmean_annual"))
get_prism_annual(type="tmean", year = 1981 : 2022, keepZip=FALSE)
RStmean <- prism_stack(prism_archive_ls())
station_shp <- st_transform(station_shp, st_crs(RStmean))

par.tmean <- raster::extract(RStmean, station_shp, fun = mean, weights = TRUE)
par.tmean <- as.data.frame(par.tmean)

# replace column names with dates
colnames <- colnames(par.tmean)
rep_cnames <- data.frame(name = str_extract(colnames, "(stable_?).+")) %>%
  mutate(Year = substr(name, 14, 17))

# row is the parcel, column is the year
colnames(par.tmean) <- rep_cnames$Year
par.tmean$id <- 1:nrow(par.tmean)

tmeanLong <- gather(par.tmean, key = "Year", value = "tmean", -id, na.rm = FALSE,
                    convert = FALSE, factor_key = FALSE)

StationInfo <- data.frame(ID = station_shp$ID,
                          id = 1:nrow(station_shp))

tmeanLong %<>%  left_join(StationInfo, by = "id") %>%  select(-id)
tmeanLong$Year <- as.numeric(tmeanLong$Year)
df_final_mn %<>% left_join(tmeanLong, by = c("ID", "Year"))
