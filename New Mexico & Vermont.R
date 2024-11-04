library(pacman)
p_load(readxl, dataRetrieval, dplyr, lubridate, sf, tidyr, magrittr, zoo, ggplot2, DescTools, stringr, knitr)

#desktop
maindir <- "C:\\Users\\Sol\\OneDrive - Michigan State University\\Saesol and Molly Shared Work\\Water Quality\\Data Compilation"
setwd(maindir)

# Download the longitude and latitude data per monitoring stations; We need numeric statecodes in the data ##

#1. Create an empty data frame 
extracted_df <- data.frame(StateCode = character(),
                           MonitoringLocationIdentifier = character(),
                           MonitoringLocationName = character(),
                           HUCEightDigitCode = character(),
                           Latitude=character(),
                           Longitude=character(),
                           stringsAsFactors = FALSE)

#2. Load the data containing adoption dates
States <- read_excel("States.xlsx", sheet = "RImport") # check the sheet name before importing

#3. Download the data
VTNM_data <- list()
VTNM_interest <- c("35", "50")
for (state_code in VTNM_interest) {
  # Retrieve data for the current state code and characteristic name
  current_sites <- whatWQPsites(statecode = state_code, characteristicName = "Phosphorus")
  VTNM_data[[state_code]] <- current_sites
}

#4. Select the variables of use
for (state_code in VTNM_interest) {
  VTNM_data[[state_code]] <- VTNM_data[[state_code]] %>%
    select(StateCode, CountyCode, MonitoringLocationIdentifier, MonitoringLocationName,
           HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, ProviderName,
           OrganizationIdentifier, OrganizationFormalName, MonitoringLocationTypeName)
}
VTNM_site <- bind_rows(VTNM_data) # 85403


VT <- readWQPdata(statecode = "VT", characteristicName = "Phosphorus")
NM <- readWQPdata(statecode = "NM", characteristicName = "Phosphorus")


df_concise <- df %>% 
  filter(ResultSampleFractionText == "Total" &
           ActivityMediaSubdivisionName == "Surface Water" &
           !is.na(ResultMeasureValue)) %>%
  filter(ActivityTypeCode == "Sample-Routine") %>%
  # filter(ActivityTypeCode == "Sample-Routine" | ActivityTypeCode == "Sample") %>%
  mutate(Year = year(ActivityStartDate), # add year and month columns 
         Month = month(ActivityStartDate)) %>% 
  rename(Unit = ResultMeasure.MeasureUnitCode) %>%  # Select variables of use
  select(ActivityMediaSubdivisionName,
         ActivityStartDate,
         ActivityIdentifier,
         ActivityTypeCode,
         MonitoringLocationIdentifier,
         SampleCollectionMethod.MethodIdentifier,
         ResultMeasureValue,
         ResultValueTypeName,
         Unit,
         Year,
         Month
  ) # Select variables of use 892388 (579264 for Sample-Routine)

# Leftjoin df_concise with extracted_df; now we have numeric statecode
df_concise %<>% left_join(df_site, by = "MonitoringLocationIdentifier") #892388 (579264) obs

# Add obs of which 'ActivityMediaSubdivisionName' have Water/NA 

#1. filter for ActivityMediaSubdivisionName = Water or NA  
# table(df$ActivityMediaSubdivisionName) # Water 10628
# table(is.na(df$ActivityMediaSubdivisionName)) # NA: 225286
df_wt <- df %>% filter(ActivityMediaSubdivisionName == "Water") # 10628
df_na <- df %>% filter(is.na(ActivityMediaSubdivisionName)) # 296974
df_updated <- rbind(df_wt, df_na) # additional obs 307602

#2. filter for other variables in use
df_updated_concise <- df_updated %>% 
  filter(ResultSampleFractionText == "Total" &
           !is.na(ResultMeasureValue)) %>%
  filter(ActivityTypeCode == "Sample-Routine") %>%
  # filter(ActivityTypeCode == "Sample-Routine" | ActivityTypeCode == "Sample") %>%
  mutate(Year = year(ActivityStartDate), # add year and month columns 
         Month = month(ActivityStartDate)) %>% 
  rename(Unit = ResultMeasure.MeasureUnitCode) %>%  # Select variables of use
  select(ActivityMediaSubdivisionName,
         ActivityStartDate,
         ActivityIdentifier,
         ActivityTypeCode,
         MonitoringLocationIdentifier,
         SampleCollectionMethod.MethodIdentifier,
         ResultMeasureValue,
         ResultValueTypeName,
         Unit,
         Year,
         Month
  ) # 240754 obs (194246)

#3. Leftjoin df_concise with extracted_df; now we have numeric statecode
df_updated_concise %<>% left_join(df_site, by = "MonitoringLocationIdentifier")

#4. subfilter for river/stream-like obs
df_updated_concise %<>% 
  dplyr::filter(grepl("STREAM|CREEK|Cr\\.|RIVER|RIV.|SURFACE|spring", MonitoringLocationName, ignore.case = TRUE)) 
# 107633 more observations obtained  (91304)

# combine the two datasets
df_concise <- rbind(df_concise, df_updated_concise) #1000021 obs (670568)

# Remove NAs in the latitude and longitude
df_concise %<>% filter(!is.na(LongitudeMeasure) & !is.na(LatitudeMeasure)) # Remove none

# Create id variable: MonitoringLocationIdentifier is not valid! Pure combination of (lat, long) does not work
df_concise <- df_concise %>%
  mutate(LatLong = paste(LatitudeMeasure, LongitudeMeasure))

df_concise_ID <- df_concise %>%
  mutate(LatLong = paste(LatitudeMeasure, LongitudeMeasure) ) %>% 
  distinct(LatLong) %>%
  mutate(ID = row_number()) %>%
  ungroup()  # 43354 monitoring stations (36302)

df_concise %<>% left_join(df_concise_ID, by="LatLong") %>% select(-LatLong) # 670568

# Check the NAs 
for (col in names(df_concise)) {
  var_na <- sum(is.na(df_concise[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}
