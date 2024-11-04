# July 8 2024
# StateCode (new data on control states) vs. StateCd (origianl data)
##################### Preamble -----------
# First, load in packages
pacman::p_load(magrittr, prism, raster, rgdal, rgeos, sf, tidyverse)
pacman::p_load(readxl, dataRetrieval, dplyr, lubridate, sf, tidyr, zoo, ggplot2, DescTools, fixest, viridis, usmap, grid, stringr)

library("Matrix")

# Read in strings as non-factors
options(stringsAsFactors = F)

# Setting Paths 
maindir <- "/mnt/ufs18/home-261/kangsaes/"
setwd(maindir)

#Read in monitoring station data
station_shp <- st_read("df_filtered_mn.shp")
load("df_control.RData")
station_shp <- st_as_sf(df_control_mn, coords=c("LongitudeMeasure", "LatitudeMeasure"), crs=4326) 


st_crs(station_shp)

#################################################### Temperature #######
prism_set_dl_dir(paste0(maindir, "/tmean"))
# Initialize an empty list to store results for each year
result_list <- list()
# Define intervals
value_ranges <- c(-Inf, 0, 5, 10, 15, 20, 25, Inf)  

# Loop through years from 1981 to 2022
for (year in 1981:2022) {
  # Define date range for the current year
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  
  # Load and process data for the current year
  # prism_data <- get_prism_dailys(type = "tmean", minDate = start_date, maxDate = end_date)
  # RStmean <- prism_stack(prism_data)  # Using the updated function
  RStmean <- prism_stack(prism_archive_subset(type = "tmean", temp_period = "daily", minDate = start_date, maxDate = end_date))
  
  # Extract mean temperature
  par.tmean <- raster::extract(RStmean, station_shp, fun = mean, weights = TRUE)
  par.tmean <- as.data.frame(par.tmean)
  par.tmean$id <- 1:nrow(par.tmean)
  
  # Transform data to long format and categorize
  tmeanLong <- tidyr::pivot_longer(par.tmean, cols = -id, names_to = "date", values_to = "tmean")
  tmeanLong$interval <- cut(tmeanLong$tmean, breaks = value_ranges, right = FALSE)
  
  # Count observations in each interval by id
  df_long <- tmeanLong %>%
    group_by(id, interval) %>%
    summarize(count = n(), .groups = "drop")
  
  # Convert the data from long to wide format for the current year
  df_wide <- df_long %>%
    pivot_wider(names_from = interval, values_from = count, values_fill = list(count = 0)) %>%
    rename_with(~ paste("t_", year, "_", .x, sep = ""), -id)
  
  # Store the wide-format data frame in the result list
  result_list[[as.character(year)]] <- df_wide
}

# 
for (i in seq_along(result_list)) {
  # Extract the year from the second column of each data frame
  year <- str_extract(colnames(result_list[[i]])[2], "\\d{4}")  # Extract year from column name
  
  # Add a constant 'Year' column to the data frame
  result_list[[i]] <- result_list[[i]] %>%
    mutate(Year = as.integer(year))  # Add 'Year' column with constant value
}  

# Define a function to process each data frame in result_list
process_dataframe <- function(df) {
  # Extract variable names starting with "t_"
  t_vars <- grep("^t_", names(df), value = TRUE)
  
  # Modify variable names by removing substrings (e.g., substr(2, 6))
  new_names <- sapply(t_vars, function(var) {
    if (nchar(var) >= 6) {
      modified_name <- paste0(substr(var, 1, 1), substr(var, 7, nchar(var)))
    } else {
      modified_name <- var  # Keep the original name if it doesn't require modification
    }
    return(modified_name)
  })
  
  # Rename the selected variables in the data frame
  df <- df %>%
    rename_at(vars(t_vars), ~ new_names)
  
  return(df)
}

# Iterate over each data frame in 'result_list' and apply the processing function
processed_result_list <- lapply(result_list, process_dataframe)


StationInfo <- data.frame(ID = station_shp$ID, id = 1:nrow(station_shp))

process_join_select_rename <- function(df) {
  # Perform left join with StationInfo, remove 'id', and rename 'Station' to 'UniqueID'
  processed_df <- df %>%
    left_join(StationInfo, by = "id") %>%
    select(-id) 
  # rename(UniqueID = Uniqu)
  
  return(processed_df)
}

# Apply the processing function to each data frame in 'processed_result_list'
processed_result_list <- lapply(processed_result_list, process_join_select_rename)


# Append the data frames in the list
temp_df <- bind_rows(processed_result_list)
save(temp_df, file = "temp_df.RData")


#########Annual Average Temperature#######################

# Make a path storing annual precipitation
prism_set_dl_dir(paste0(maindir, "/tmean_annual"))

get_prism_annual(type = "tmean", years = 1981:2022, keepZip = FALSE)

RStmean <- prism_stack(prism_archive_ls())

station_shp <- st_transform(station_shp, st_crs(RStmean))
par.tmean_annual <- raster::extract(RStmean, station_shp, fun = mean, weights = TRUE)
par.tmean_annaul <- as.data.frame(par.tmean_annual)

# Prepare the data frame for final results
colnames <- names(par.tmean_annaul)
rep_cnames <- data.frame(name = stringr::str_extract(colnames, "(stable_?).+")) %>%
  mutate(Year = substr(name, 14, 17))
names(par.tmean_annaul) <- rep_cnames$Year
par.tmean_annaul$id <- 1:nrow(par.tmean_annaul)
tmeanAnnualLong <- tidyr::gather(par.tmean_annaul, key = "Year", value = "tmean", -id, na.rm = FALSE,
                         convert = FALSE, factor_key = FALSE)
StationInfo <- data.frame(ID = station_shp$ID, id = 1:nrow(station_shp))
tmeanAnnualLong %<>% left_join(StationInfo, by = "id") %>% select(-id) %>% distinct()

save(tmeanAnnualLong, file = "df_tmean_annual.RData")
save(tmeanAnnualLong, file = "df_control_tmean_annual.RData")







########################Precipitation###############


# Make a list storing each state's annual precipitation
      prism_set_dl_dir(paste0(maindir, "/ppt"))
      # get_prism_annual(type = "ppt", years = 1981:2022, keepZip = FALSE)
      # RSppt <- prism_stack(prism_archive_ls())
      RSppt <- prism_stack(prism_archive_subset(type = "ppt", temp_period = "annual"))

      station_shp <- st_transform(station_shp, st_crs(RSppt))
      par.ppt <- raster::extract(RSppt, station_shp, fun = mean, weights = TRUE)
      par.ppt <- as.data.frame(par.ppt)
      
      # Prepare the data frame for final results
      colnames <- names(par.ppt)
      rep_cnames <- data.frame(name = stringr::str_extract(colnames, "(stable_?).+")) %>%
        mutate(Year = substr(name, 14, 17))
      names(par.ppt) <- rep_cnames$Year
      par.ppt$id <- 1:nrow(par.ppt)
      pptLong <- tidyr::gather(par.ppt, key = "Year", value = "ppt", -id, na.rm = FALSE,
                               convert = FALSE, factor_key = FALSE)
      StationInfo <- data.frame(ID = station_shp$ID, id = 1:nrow(station_shp), StateCd = station_shp$StateCd)
      pptLong %<>% left_join(StationInfo, by = "id") %>% select(-id)
      ppt_annual <- pptLong  %>%  distinct() # Leave the unique stations
      
      # Save ppt_annual separately
      save(ppt_annual, file = "df_ppt_annual.RData")
      
      
      # Divide ppt by 365 for average
      ppt_annual %<>% mutate(ppt_365 = ppt/365) 
      
      # Group by the state
      state_ppt <- ppt_annual %>% 
        group_by(StateCd) %>% 
        summarize(avg_ppt = mean(ppt_365, na.rm = TRUE))

# loop (create below and above average rainfalls)
      #Set up location for prism data to download--note that I did this twice, separately, to get tmean and ppt
      prism_set_dl_dir(paste0(maindir, "/ppt_daily"))

      # Initialize an empty data frame to store results
      df_ppt_results <- data.frame(
        Year = integer(),
        ID = integer(),
        below_avg_days = integer(),
        above_avg_days = integer()
      )
      
      for (year in 1981:2022) {
        # Format dates for the start and end of the year
        start_date <- paste(year, "-01-01", sep="")
        end_date <- paste(year, "-12-31", sep="")
        
        # Get PRISM data
        # get_prism_dailys(type="ppt", minDate = start_date, maxDate = end_date, keepZip=FALSE)
        RSppt <- prism_stack(prism_archive_subset(type = "ppt", temp_period = "daily", minDate = start_date, maxDate = end_date))
        # RSppt <- prism_stack(prism_archive_ls())
        
        # Transform coordinate reference system
        station_shp <- st_transform(station_shp, st_crs(RSppt))
      
        # Extract mean precipitation using raster package
        par.ppt <- raster::extract(RSppt, station_shp, fun = mean, weights = TRUE)
        par.ppt <- as.data.frame(par.ppt)
      
        # Replace column names with dates
        colnames <- names(par.ppt)
        rep_cnames <- data.frame(name = stringr::str_extract(colnames, "(stable_?).+")) %>%
          mutate(Date = substr(name, 14, 21))
        names(par.ppt) <- rep_cnames$Date
      
        par.ppt$id <- 1:nrow(par.ppt)
        pptLong <- tidyr::gather(par.ppt, key = "Date", value = "ppt", -id, na.rm = FALSE,
                                   convert = FALSE, factor_key = FALSE)
      
        StationInfo <- data.frame(ID = station_shp$ID, id = 1:nrow(station_shp), StateCd = station_shp$StateCd)
        pptLong %<>% left_join(StationInfo, by = "id") %>% select(-id) %>% distinct()
      
        # Inside the loop, after processing precipitation data
        ppt_vars <- pptLong %>% left_join(state_ppt, by = "StateCd") %>%
          group_by(ID, StateCd) %>%
          summarise(below_avg_days = sum(ppt < avg_ppt, na.rm = TRUE),
                    above_avg_days = sum(ppt >= avg_ppt, na.rm = TRUE),
                    .groups = 'drop')  # Ensure the group by operation is fully handled
        
        # Append to the results dataframe
        df_ppt_results <- bind_rows(df_ppt_results, ppt_vars %>% mutate(Year = year))        
      }
  
  
# Prepare the data frame for the days with precipitation
df_ppt_results2 <- data.frame(Year = integer(), ID = integer(), rainydays = integer())
      
      
      # loop (create rainydays)
      for (year in 1981:2022) {
        # Format dates for the start and end of the year
        start_date <- paste(year, "-01-01", sep="")
        end_date <- paste(year, "-12-31", sep="")
        
        # Get PRISM data
        RSppt <- prism_stack(prism_archive_subset(type = "ppt", temp_period = "daily", minDate = start_date, maxDate = end_date))

        # Transform coordinate reference system
        station_shp <- st_transform(station_shp, st_crs(RSppt))
        
        # Extract mean precipitation using raster package
        par.ppt <- raster::extract(RSppt, station_shp, fun = mean, weights = TRUE)
        par.ppt <- as.data.frame(par.ppt)
        
        # Replace column names with dates
        colnames <- names(par.ppt)
        rep_cnames <- data.frame(name = stringr::str_extract(colnames, "(stable_?).+")) %>%
          mutate(Date = substr(name, 14, 21))
        names(par.ppt) <- rep_cnames$Date
        
        par.ppt$id <- 1:nrow(par.ppt)
        pptLong <- tidyr::gather(par.ppt, key = "Date", value = "ppt", -id, na.rm = FALSE,
                                 convert = FALSE, factor_key = FALSE)
        
        StationInfo <- data.frame(ID = station_shp$ID, id = 1:nrow(station_shp))
        pptLong %<>% left_join(StationInfo, by = "id") %>% select(-id)
        
        # Count the number of rainy days in a year
        pptLong <- pptLong %>%
          group_by(ID) %>%
          summarise(rainydays = sum(ppt > 0, na.rm = TRUE))
        
        # Store results
        df_ppt_results2 <- bind_rows(df_ppt_results2, data.frame(Year = year, ID = pptLong$ID, rainydays = pptLong$rainydays))
        
      }

save(df_ppt_results, file = "df_ppt_results.RData")
save(df_ppt_results2, file = "df_ppt_results2.RData")


