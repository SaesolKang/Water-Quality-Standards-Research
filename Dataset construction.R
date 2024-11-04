library(pacman)
p_load(readxl, dataRetrieval, dplyr, lubridate, sf, tidyr, magrittr, zoo, ggplot2, DescTools, stringr, knitr)

#laptop
maindir <- "C:\\Users\\wilin\\OneDrive - Michigan State University\\Saesol and Molly Shared Work\\Water Quality\\Data Compilation"
#desktop
maindir <- "C:\\Users\\Sol\\OneDrive - Michigan State University\\Saesol and Molly Shared Work\\Water Quality\\Data Compilation"
#Molly
maindir <- "C:/Users/vando/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation"

setwd(maindir)

# Download the longitude and latitude data per monitoring stations; We need numeric statecodes in the data ##

    # #1. Create an empty data frame - not really needed. may delete
    # extracted_df <- data.frame(StateCode = character(),
    #                            MonitoringLocationIdentifier = character(),
    #                            MonitoringLocationName = character(),
    #                            HUCEightDigitCode = character(),
    #                            Latitude=character(),
    #                            Longitude=character(),
    #                            stringsAsFactors = FALSE)
    
    #2. Load the data containing adoption dates
    States <- read_excel("States.xlsx", sheet = "RImport") # check the sheet name before importing
    
    #3. Download the data
    site_data <- list()
    Statecode_interest <- c("04", "06", "12", "15", "30", "32", "34", "40", "49", "55")
    for (state_code in Statecode_interest) {
      # Retrieve data for the current state code and characteristic name
      current_sites <- whatWQPsites(statecode = state_code, characteristicName = "Phosphorus")
      site_data[[state_code]] <- current_sites
    }
    
    #4. Select the variables of use
    for (state_code in Statecode_interest) {
      site_data[[state_code]] <- site_data[[state_code]] %>%
        select(StateCode, CountyCode, MonitoringLocationIdentifier, MonitoringLocationName,
               HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, ProviderName,
               OrganizationIdentifier, OrganizationFormalName, MonitoringLocationTypeName)
    }
    df_site <- bind_rows(site_data) # 85403 86883

c## Data extraction from WQP ##
   AZ <- readWQPdata(statecode = "AZ", characteristicName = "Phosphorus")
   CA <- readWQPdata(statecode = "CA", characteristicName = "Phosphorus")
   FL <- readWQPdata(statecode = "FL", characteristicName = "Phosphorus")
   HI <- readWQPdata(statecode = "HI", characteristicName = "Phosphorus")
   NV <- readWQPdata(statecode = "NV", characteristicName = "Phosphorus")
   MN <- readWQPdata(statecode = "MT", characteristicName = "Phosphorus")
   NJ <- readWQPdata(statecode = "NJ", characteristicName = "Phosphorus")
   OK <- readWQPdata(statecode = "OK", characteristicName = "Phosphorus")
   UT <- readWQPdata(statecode = "UT", characteristicName = "Phosphorus")
   WI <- readWQPdata(statecode = "WI", characteristicName = "Phosphorus")
   
   VT <- readWQPdata(statecode = "VT", characteristicName = "Phosphorus")
   NM <- readWQPdata(statecode = "NM", characteristicName = "Phosphorus")
   
   
   PR <- readWQPdata(statecode = "PR", characteristicName = "Phosphorus")
   
   AL <- readWQPdata(statecode = "AL", characteristicName = "Phosphorus")
   AK <- readWQPdata(statecode = "AK", characteristicName = "Phosphorus")
   AR <- readWQPdata(statecode = "AR", characteristicName = "Phosphorus")
   CT <- readWQPdata(statecode = "CT", characteristicName = "Phosphorus")
   DE <- readWQPdata(statecode = "DE", characteristicName = "Phosphorus")
   DC <- readWQPdata(statecode = "DC", characteristicName = "Phosphorus")
   ID <- readWQPdata(statecode = "ID", characteristicName = "Phosphorus")
   IN <- readWQPdata(statecode = "IN", characteristicName = "Phosphorus")
   IA <- readWQPdata(statecode = "IA", characteristicName = "Phosphorus")
   KS <- readWQPdata(statecode = "KS", characteristicName = "Phosphorus")
   KY <- readWQPdata(statecode = "KY", characteristicName = "Phosphorus")
   LA <- readWQPdata(statecode = "LA", characteristicName = "Phosphorus")
   ME <- readWQPdata(statecode = "ME", characteristicName = "Phosphorus")
   MD <- readWQPdata(statecode = "MD", characteristicName = "Phosphorus")
   MI <- readWQPdata(statecode = "MI", characteristicName = "Phosphorus")
   MS <- readWQPdata(statecode = "MS", characteristicName = "Phosphorus")
   NY <- readWQPdata(statecode = "NY", characteristicName = "Phosphorus")
   NC <- readWQPdata(statecode = "NC", characteristicName = "Phosphorus")
   ND <- readWQPdata(statecode = "ND", characteristicName = "Phosphorus")
   OH <- readWQPdata(statecode = "OH", characteristicName = "Phosphorus")
   PA <- readWQPdata(statecode = "PA", characteristicName = "Phosphorus")
   TN <- readWQPdata(statecode = "TN", characteristicName = "Phosphorus")
   TX <- readWQPdata(statecode = "TX", characteristicName = "Phosphorus")
   WY <- readWQPdata(statecode = "WY", characteristicName = "Phosphorus")
   save(AL, AK, AR, CT, DE, DC, ID, IN, IA, KS, KY, LA, #
        ME, MD, MI, MS, NY, NC, ND, OH, PA, TN, TX, WY, #
        file = "Control_States.RData")
   df_control <- rbind(AL, AK, AR, CT, DE, DC, ID, IN, IA, KS, KY, LA, #
   ME, MD, MI, MS, NY, NC, ND, OH, PA, TN, TX, WY)
   df <- rbind(AZ, CA, FL, HI, NV, MN, NJ, OK, UT, WI) #1532927

# Additional Filtering for variables in use

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
          ) # 194246
        
        #3. Leftjoin df_concise with extracted_df; now we have numeric statecode
        df_updated_concise %<>% left_join(df_site, by = "MonitoringLocationIdentifier")
        
        #4. subfilter for river/stream-like obs
        df_updated_concise2 <- df_updated_concise  %>%  
          dplyr::filter(grepl("STREAM|CREEK|Cr\\.|RIVER|RIV.|SURFACE|spring", MonitoringLocationName, ignore.case = TRUE)) 
        # (91304) 91250
        
# combine the two datasets
df_concise2 <- rbind(df_concise, df_updated_concise2) # 670568 670514
        
########################################The monitoring station level nutrients data are constructed

# Remove NAs in the latitude and longitude
df_concise2 %<>% filter(!is.na(LongitudeMeasure) & !is.na(LatitudeMeasure)) # Remove none 670306

# Create id variable: MonitoringLocationIdentifier is not valid! Pure combination of (lat, long) does not work
df_concise <- df_concise %>%
  mutate(LatLong = paste(LatitudeMeasure, LongitudeMeasure))

df_concise_ID <- df_concise %>%
  mutate(LatLong = paste(LatitudeMeasure, LongitudeMeasure) ) %>% 
  distinct(LatLong) %>%
  mutate(ID = substr(LatLong, 1, 6)) %>%
  ungroup()  # 43354 monitoring stations (36302) 36219

df_concise %<>% left_join(df_concise_ID, by="LatLong") %>% select(-LatLong) # 670568

# Check the NAs 
for (col in names(df_concise)) {
  var_na <- sum(is.na(df_concise[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}
# Column ActivityMediaSubdivisionName has 91304 missing values.
# Column ActivityStartDate has 0 missing values.
# Column ActivityIdentifier has 0 missing values.
# Column ActivityTypeCode has 0 missing values.
# Column MonitoringLocationIdentifier has 0 missing values.
# Column SampleCollectionMethod.MethodIdentifier has 345 missing values.
# Column ResultMeasureValue has 0 missing values.
# Column ResultValueTypeName has 169 missing values.
# Column Unit has 1971 missing values.
# Column Year has 0 missing values.
# Column Month has 0 missing values.
# Column StateCode has 0 missing values.
# Column CountyCode has 2 missing values.
# Column MonitoringLocationName has 0 missing values.
# Column HUCEightDigitCode has 20853 missing values.
# Column LatitudeMeasure has 0 missing values.
# Column LongitudeMeasure has 0 missing values.
# Column ProviderName has 0 missing values.
# Column OrganizationIdentifier has 0 missing values.
# Column OrganizationFormalName has 0 missing values.
# Column ID has 0 missing values.

# Filter rows before 1981 and after 2022; df_filtered is filtered for 1981-2022 & mg/L & annual p level
rows_to_remove <- df_concise$Year < 1980 | df_concise$Year > 2022 # Make sure to remove 1980 later
df_filtered <- df_concise[!rows_to_remove, ] # 599501

# Convert the measurement unit
table(df_filtered$Unit)
df_filtered %<>% mutate(Unit=ifelse(Unit %in% c("mg/l", "mg/l as P"), "mg/L", 
                                      ifelse(Unit=="ug/l","ug/L", Unit))) %>% 
                                      filter(Unit=="mg/L" | Unit=="ug/L") # 833738  (577371) obs

# Remove problematic nutrient values before conversion
df_filtered %<>% mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
  filter(!is.na(ResultMeasureValue), ResultMeasureValue >= 0) # 885204 obs (576519)

# multiply 0.001 if the unit is ug/L and replace the unit as mg/L
df_filtered %<>% mutate(ResultMeasureValue = ifelse(Unit == "ug/L", 0.001 * ResultMeasureValue, ResultMeasureValue)) %>% 
                mutate(Unit=ifelse(Unit=="ug/L", "mg/L", Unit))
table(df_filtered$Unit) # 576519 obs

# Remove duplicates. This treatment should take place before computing yearly p level
df_filtered %<>% distinct(ID, ActivityStartDate, ResultMeasureValue, .keep_all = TRUE) # 553298

# Winsorize at 99% 
df_filtered %<>% group_by(StateCode) %>%
  mutate(Plv_winsorized = Winsorize(ResultMeasureValue, probs = c(0.005, 0.995)))

# There are calculated and estimated values; dropping this would affect the winsorized values
table(df_filtered$ResultValueTypeName)
# Actual Calculated  Estimated 
# 548349        439       4491 
df_filtered <- df_filtered %>% filter(ResultValueTypeName=="Actual") # 40588
df_filtered %<>% mutate(Day=day(ActivityStartDate)) 

    # Test the water pollution trend here
    df_daily2 <- df_filtered %>% 
      group_by(ID, Year, Month, Day) %>%
      summarize(NumObs = n(), 
                dailyPlevel = sum(ResultMeasureValue) / n()  ) %>% 
      ungroup() 
      df_filtered_trend <- df_filtered %>%  left_join(df_daily2, by = c("ID", "Year", "Month", "Day")) %>% 
      select(ID, ActivityStartDate, Year, Month, Day, StateCode,CountyCode, HUCEightDigitCode, dailyPlevel ) %>% 
      group_by(ID, ActivityStartDate) %>%
      distinct()
      sapply(df_filtered_trend, function(x) sum(is.na(x)))  
      summary(df_filtered_trend)
      table(df_filtered_trend$StateCode)
      df_filtered_summary <- df_filtered_trend %>%  group_by(ID, ActivityStartDate) %>% summarize(NumObs = n())
      df_filtered_AZ <- df_filtered_trend %>% filter(StateCode=="04")
      feols(dailyPlevel ~ Year | Month + Day + ID + HUCEightDigitCode, data = df_filtered_AZ)
      df_filtered_CA <- df_filtered_trend %>% filter(StateCode=="06")
      feols(dailyPlevel ~ Year | Month + Day + ID + HUCEightDigitCode, data = df_filtered_CA)
      df_filtered_FL <- df_filtered_trend %>% filter(StateCode=="12")
      feols(dailyPlevel ~ Year | Month + Day + ID + HUCEightDigitCode, data = df_filtered_FL)
      df_filtered_HI <- df_filtered_trend %>% filter(StateCode=="15")
      feols(dailyPlevel ~ Year | Month + Day + ID + HUCEightDigitCode, data = df_filtered_HI)
      df_filtered_MT <- df_filtered_trend %>% filter(StateCode=="30")
      feols(dailyPlevel ~ Year | Month + Day + ID + HUCEightDigitCode, data = df_filtered_MT)
      df_filtered_NV <- df_filtered_trend %>% filter(StateCode=="32")
      feols(dailyPlevel ~ Year | Month + Day + ID + HUCEightDigitCode, data = df_filtered_NV)
      df_filtered_NJ <- df_filtered_trend %>% filter(StateCode=="34")
      feols(dailyPlevel ~ Year | Month + Day + ID + HUCEightDigitCode, data = df_filtered_NJ)
      df_filtered_OK <- df_filtered_trend %>% filter(StateCode=="40")
      feols(dailyPlevel ~ Year | Month + Day + ID + HUCEightDigitCode, data = df_filtered_OK)
      df_filtered_UT <- df_filtered_trend %>% filter(StateCode=="49")
      feols(dailyPlevel ~ Year | Month + Day + ID + HUCEightDigitCode, data = df_filtered_UT)
      df_filtered_WI <- df_filtered_trend %>% filter(StateCode=="55")
      feols(dailyPlevel ~ Year | Month + Day + ID + HUCEightDigitCode, data = df_filtered_WI)
      
      feols(dailyPlevel ~ Year + Month + Day | ID + StateCode, data = df_filtered_trend, cluster="HUCEightDigitCode" )

# Construct the yearly P levels
  # 1. Average the phosphorus levels on a daily/monthly basis
    df_daily <- df_filtered %>% mutate(Day=day(ActivityStartDate)) %>% 
                    group_by(ID, Year, Month, Day) %>%
                    summarize(NumObs = n(), 
                              dailyPlevel = sum(Plv_winsorized) / n()  ) %>% 
                    ungroup()
    
    df_monthly <- df_daily %>% group_by(ID, Year, Month) %>%
                summarize(NumObs = n(), 
                monthlyPlevel = sum(dailyPlevel) / n()  ) %>% 
                ungroup()
    
    df_yearly <- df_monthly %>% group_by(ID, Year) %>%
                summarize(NumObs = n(), 
                          yearlyPlevel = sum(monthlyPlevel) / n()  ) %>% 
                ungroup()

  #2. Merge yearly p level with the main dataset
    df_filtered %<>% left_join(df_yearly, by = c("ID", "Year")) %>% 
      group_by(ID, Year) %>%
      distinct() %>%
      ungroup() # 879735 (553298)

# Remove unnecessary variables before leaving one annual observation per ID
df_filtered %<>%  select(-c(Month, NumObs, ActivityMediaSubdivisionName, ActivityIdentifier, 
                            ActivityTypeCode, MonitoringLocationIdentifier, 
                            SampleCollectionMethod.MethodIdentifier, OrganizationIdentifier, 
                            ActivityStartDate, ResultMeasureValue, Plv_winsorized)) %>%  
                  distinct() # 117454 (94817)

# Check whether duplicates are removed. Not all due to changing monitoring location name
df_filtered_summary <- df_filtered %>%  group_by(ID, Year) %>% summarize(NumObs = n())

# Remove the duplicates by ID, Year.
df_filtered %<>% distinct(ID, Year, .keep_all=TRUE) # 114828 (92661)
    
table(df_filtered$ResultValueTypeName)
# Actual Calculated  Estimated 
# 91857        118        668 

# Create a lagged variable
    # # Ensure your data is sorted by ID and Year
    # df_filtered %<>% arrange(ID, Year)
    # 
    # # Create the lagged variable
    # df_filtered %<>%  group_by(ID) %>%
    #                   mutate(Plevel_lag = lag(yearlyPlevel, 1))
    # summary(df_filtered$Plevel_lag) # NA 32102
    
    
# Create a lagged Plevel variable

    # Ensure the dataframe is sorted by ID and Year
    df_filtered_lag <- df_filtered %>% arrange(ID, Year)
    
    # Initialize the lagged variables with NA
    df_filtered_lag$Plevel_lag <- NA
    df_filtered_lag$lag <- NA
    
    # Loop over each row starting from the second one because the first row can't have a previous year
    for (i in 2:nrow(df_filtered_lag)) {
      # Get the current ID and year
      current_id <- df_filtered_lag$ID[i]
      current_year <- df_filtered_lag$Year[i]
      
      # Filter to find previous years' data for the same ID
      previous_entries <- df_filtered_lag[df_filtered_lag$ID == current_id & df_filtered_lag$Year < current_year, ]
      
      if (nrow(previous_entries) > 0) {
        # Get only those entries where 'yearlyPlevel' is not NA
        available_entries <- previous_entries[!is.na(previous_entries$yearlyPlevel), ]
        
        if (nrow(available_entries) > 0) {
          # Compute the number of years back to the available data
          years_back <- current_year - available_entries$Year
          min_index <- which.min(years_back)  # Find the index of the minimum value in years_back
          
          # Assign the lagged values
          df_filtered_lag$Plevel_lag[i] <- available_entries$yearlyPlevel[min_index]
          df_filtered_lag$lag[i] <- years_back[min_index]
          
          # Debugging output
          # print(paste("ID:", current_id, "Year:", current_year, "Years back:", years_back, "Lag selected:", years_back[min_index]))
        }
      }
    }
    
    # Check a summary to ensure it looks right
    summary(df_filtered_lag$lag)
    
#################################################################################################################
# Conduct geo-spatial data generation
# Export the data frame as a shape file (to match with the nearest waterbody)
df_filtered_sf <- st_as_sf(df_filtered_lag, coords=c("LongitudeMeasure", "LatitudeMeasure"), crs=4326) 
address <- "df_filtered_sf_sampleroutine.shp"
st_write(df_filtered_sf, dsn = address, layer = "df_filtered_sf") # 91681

# Import other shape files
USA_Rivers_and_Streams_withID <- st_read("USA_Rivers_and_Streams_withID_ExportFeatures.shp")
USA_Rivers_and_Streams_withID <- USA_Rivers_and_Streams_withID[!duplicated(USA_Rivers_and_Streams_withID$UniqueID), ] # remove 139 duplicates 112968 obs
Treated_Rivers_and_Streams_withID <- st_read("Treated_Merge_tenstates.shp") %>% as.data.frame # 3552
Treated_Rivers_and_Streams_withID %<>% select(Name, State, UniqueID)

# Not necessarily need to run these as we will include them in the intensity variable R script
Treated_MT_l3 <- st_read("Montana_wbl3_SpatialJoin.shp") %>%
  as.data.frame() %>% 
  select(Name, State, UniqueID, US_L3NAME) %>% 
  rename(LName = US_L3NAME) %>% 
  filter(LName %in% c("Canadian Rockies", "Idaho Batholith", "Nothern Rockies", "Northwestern Glaciated Plains", "Northwestern Great Plains", "Wyoming Basin") )  %>% 
  select(Name, State, UniqueID)

Treated_MT_l4 <- st_read("Montana_wbl4_SpatialJoin.shp") %>%
  as.data.frame() %>% 
  select(Name, State, UniqueID, US_L4NAME) %>% 
  rename(LName = US_L4NAME) %>% 
  filter(LName %in% c("Absaroka-Gallatin Volcanic Mountains", 
                      "Sweetgrass Uplands",
                      "Milk River Pothole Upland" ,
                      "Rocky Mountain Front Foothill Potholes" ,
                      "Foothill Grassland",
                      "Non-calcareous Foothill Grassland",
                      "Shield-Smith Valleys",
                      "Limy Foothill Grassland",
                      "Pryor-Bighorn Foothills",
                      "Unglaciated Montana High Plains")) %>% 
  select(Name, State, UniqueID)

Treated_Rivers_and_Streams_withID <- rbind(Treated_Rivers_and_Streams_withID,
                                             Treated_MT_l3,
                                             Treated_MT_l4) # 6588


# Remove Saint Johns River as the lower treated part is not included (filtering by name does not work so use unique ID)
Treated_Rivers_and_Streams_withID %<>% filter(!(UniqueID == "39612" |
                                               UniqueID == "39627" | 
                                               UniqueID == "39640" |
                                               UniqueID == "39644" |
                                               UniqueID == "39650" |
                                               UniqueID == "39656" |
                                               UniqueID == "39671" |
                                               UniqueID == "39686" |
                                               UniqueID == "39691" |
                                               UniqueID == "39715" |
                                               UniqueID == "39718" |
                                               UniqueID == "39720" |
                                               UniqueID == "39742" |
                                               UniqueID == "39751" |
                                               UniqueID == "39752" |
                                               UniqueID == "39774" |
                                               UniqueID == "39783" |
                                               UniqueID == "39794" |
                                               UniqueID == "39800" |
                                               UniqueID == "39807" |
                                               UniqueID == "39813" |
                                               UniqueID == "39815" |
                                               UniqueID == "39824" |
                                               UniqueID == "39828" |
                                               UniqueID == "39838" |
                                               UniqueID == "39880" |
                                               UniqueID == "39884" |
                                               UniqueID == "39892" 
                                               )) # 6090

# Remove Little Owyhee River and Spring Canyon Creek
Treated_Rivers_and_Streams_withID %<>% filter(!(UniqueID == "44879" |
                                                  UniqueID == "78690" | 
                                                  UniqueID == "78830" |
                                                  UniqueID == "79277" |
                                                  UniqueID == "79395" |
                                                  UniqueID == "79403" |
                                                  UniqueID == "35748" |# Spring Canyon Creek
                                                  UniqueID == "80278" # Wall Canyon Creek
                                              )) # 6082   

###################################################################################################
#Create Intensity Variable
##################################################################################################

Treated_Rivers_and_Streams_withID %<>% distinct(UniqueID, .keep_all = TRUE) # 5732


# Add stations located at the lower Saint Johns River and create a Unique ID (This is station level so merge it later)
# Treated_FL_lowerStJohnsRiver <- st_read("FL_lowerStJohnsRiver.shp") # 165
# Treated_FL_lowerStJohnsRiver <- as.data.frame(Treated_FL_lowerStJohnsRiver) %>% distinct(ID, .keep_all = TRUE) # 94
# Treated_FL_lowerStJohnsRiver %<>% mutate(UniqueID = "112969") 
# Treated_FL_lowerStJohnsRiver %<>% mutate(Name = "Lower Saint Johns River") 
# Treated_FL_lowerStJohnsRiver %<>% mutate(Treated = 1) 

# Create 'Treated'
USA_Rivers_and_Streams_withID %<>%
  mutate(Treated = ifelse(UniqueID %in% Treated_Rivers_and_Streams_withID$UniqueID, 1, 0))

# Update all waterbodies in Wisconsin as treated (Those in treated rivers and streams are for the intensity variable)
USA_Rivers_and_Streams_withID %<>% 
  mutate(Treated = ifelse(str_detect(State, "WI"), 1, Treated))

# Merge Intensity
USA_Rivers_and_Streams_withID %<>% 
  left_join(Treated_Rivers_and_Streams_withID[, c("UniqueID", "Intensity")], by = "UniqueID")

# Rename some waterbodies
USA_Rivers_and_Streams_withID$Name[USA_Rivers_and_Streams_withID$UniqueID == 107128] <- "South Fork Flambeau River"
USA_Rivers_and_Streams_withID$Name[USA_Rivers_and_Streams_withID$UniqueID == 108641] <- "South Fork Flambeau River"
 
# Export the shps to find the nearest waterbody from each monitoring station
address <- "USA_Rivers_and_Streams_withID.shp"
st_write(USA_Rivers_and_Streams_withID, dsn = address, layer = "USA_Rivers_and_Streams_withID")

#################################################################################################################
#Find the nearest waterbody on ArcGIS using Near & Spatial Join based on Geodesic
################################################################################################################
# Import the processed shp (use Mode for p level and treated when exporting)
# df_filtered_spatialjoin <- st_read("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/df_filtered_sf_SpatialJoin.shp") # 117,454
df_filtered_spatialjoin <- st_read("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/df_filtered_sr_SpatialJoin.shp") # 92,661
df_filtered_spatialjoin <- as.data.frame(df_filtered_spatialjoin) 

  # Separate 1980 from df_filtered
  df_filtered_1980 <- df_filtered_spatialjoin[df_filtered_spatialjoin$Year == 1980, ] # 980

  # Revmoe 1980 from the main dataset
  df_filtered_spatialjoin <- df_filtered_spatialjoin[df_filtered_spatialjoin$Year != 1980, ] # 91681
  
  
# Remove the mis-treated stations located in a lake
df_filtered_spatialjoin  %<>%  filter(!ID=="36287") # Rock Lake near Bad River in WI
df_filtered_spatialjoin  %<>%  filter(!ID=="36129") # Bad River Slough in WI
df_filtered_spatialjoin  %<>%  filter(!ID=="28717") # Milwaukee River in WI 
# 91672

# Mutate 0 as NAs (ArcGIS coverts NA into 0.000)
df_filtered_spatialjoin %<>% mutate(Plvl_lg = ifelse(lag==0, NA, Plvl_lg))
summary(df_filtered_spatialjoin$Plvl_lg)
  
# # Update the Lower Saint Johns River
# df_filtered_spatialjoin %<>%
#   mutate(
#     Treated = if_else(ID %in% Treated_FL_lowerStJohnsRiver$ID, 1, Treated),
#     UniqueID = if_else(ID %in% Treated_FL_lowerStJohnsRiver$ID, 112969, UniqueID),
#     Name = if_else(ID %in% Treated_FL_lowerStJohnsRiver$ID, "Lower Saint Johns River", Name),
#     Join_Count = if_else(ID %in% Treated_FL_lowerStJohnsRiver$ID, 1, Join_Count)
#     )

# Summary stat
state_lookup <- data.frame(
  StateCd = c("04", "06", "12", "15", "30", "32", "34", "40", "49", "55"), # example state codes
  StateName = c("Arizona", "California", "Florida", "Hawaii", "Montana", "Nevada", "New Jersey", "Oklahoma", "Utah", "Wisconsin") # corresponding state names
)

# Get unique state codes from the dataframe
state_codes <- unique(df_filtered_spatialjoin$StateCd)

# Loop over each state code
for (state_code in state_codes) {
  # Get the state name from the lookup table
  state_name <- filter(state_lookup, StateCd == state_code)$StateName
  
  # First operation
  first_operation <- df_filtered_spatialjoin %>%
    filter(StateCd == state_code) %>%
    summarise(
      total = n(),
      unmatched = sum(Join_Count == 0, na.rm = TRUE),
      proportion = unmatched / total
    ) 
    print(paste(state_name, "proportion for losing observations:", first_operation))

  # Second operation
  second_operation <- df_filtered_spatialjoin %>%
    filter(StateCd == state_code) %>%
    distinct(ID, .keep_all = TRUE) %>%
    summarise(
      total = n(),
      unmatched = sum(Join_Count == 0, na.rm = TRUE),
      proportion = unmatched / total
    ) 
    print(paste(state_name, "proportion for losing stations:", second_operation))
  
}

# [1] "Arizona proportion for losing observations: 5186"             
# [2] "Arizona proportion for losing observations: 1208"             
# [3] "Arizona proportion for losing observations: 0.232934824527574"
# [1] "Arizona proportion for losing stations: 1464"             
# [2] "Arizona proportion for losing stations: 457"              
# [3] "Arizona proportion for losing stations: 0.312158469945355"
# [1] "California proportion for losing observations: 9031"             
# [2] "California proportion for losing observations: 3450"             
# [3] "California proportion for losing observations: 0.382017495293987"
# [1] "California proportion for losing stations: 3987"             
# [2] "California proportion for losing stations: 1751"             
# [3] "California proportion for losing stations: 0.439177326310509"
# [1] "Florida proportion for losing observations: 52184"            
# [2] "Florida proportion for losing observations: 36488"            
# [3] "Florida proportion for losing observations: 0.699218151157443"
# [1] "Florida proportion for losing stations: 20332"            
# [2] "Florida proportion for losing stations: 15209"            
# [3] "Florida proportion for losing stations: 0.748032657879205"
# [1] "Hawaii proportion for losing observations: 447"              
# [2] "Hawaii proportion for losing observations: 234"              
# [3] "Hawaii proportion for losing observations: 0.523489932885906"
# [1] "Hawaii proportion for losing stations: 74"               
# [2] "Hawaii proportion for losing stations: 35"               
# [3] "Hawaii proportion for losing stations: 0.472972972972973"
# [1] "Nevada proportion for losing observations: 3721"             
# [2] "Nevada proportion for losing observations: 1501"             
# [3] "Nevada proportion for losing observations: 0.403386186509003"
# [1] "Nevada proportion for losing stations: 897"              
# [2] "Nevada proportion for losing stations: 425"              
# [3] "Nevada proportion for losing stations: 0.473801560758082"
# [1] "Montana proportion for losing observations: 2366"            
# [2] "Montana proportion for losing observations: 276"             
# [3] "Montana proportion for losing observations: 0.11665257819104"
# [1] "Montana proportion for losing stations: 876"              
# [2] "Montana proportion for losing stations: 132"              
# [3] "Montana proportion for losing stations: 0.150684931506849"
# [1] "New Jersey proportion for losing observations: 5643"             
# [2] "New Jersey proportion for losing observations: 2003"             
# [3] "New Jersey proportion for losing observations: 0.354953039163565"
# [1] "New Jersey proportion for losing stations: 1113"             
# [2] "New Jersey proportion for losing stations: 520"              
# [3] "New Jersey proportion for losing stations: 0.467205750224618"
# [1] "Oklahoma proportion for losing observations: 3309"             
# [2] "Oklahoma proportion for losing observations: 551"              
# [3] "Oklahoma proportion for losing observations: 0.166515563614385"
# [1] "Oklahoma proportion for losing stations: 925"              
# [2] "Oklahoma proportion for losing stations: 237"              
# [3] "Oklahoma proportion for losing stations: 0.256216216216216"
# [1] "Utah proportion for losing observations: 848"             
# [2] "Utah proportion for losing observations: 300"             
# [3] "Utah proportion for losing observations: 0.35377358490566"
# [1] "Utah proportion for losing stations: 252"              
# [2] "Utah proportion for losing stations: 76"               
# [3] "Utah proportion for losing stations: 0.301587301587302"
# [1] "Wisconsin proportion for losing observations: 8937"            
# [2] "Wisconsin proportion for losing observations: 4250"            
# [3] "Wisconsin proportion for losing observations: 0.475551079780687"
# [1] "Wisconsin proportion for losing stations: 2814"             
# [2] "Wisconsin proportion for losing stations: 1433"             
# [3] "Wisconsin proportion for losing stations: 0.509239516702203"


# Remove the monitoring stations that are out of 500m boundary
df_filtered_spatialjoin %<>% filter(Join_Count != 0) # 114,818 -> 51,138 More than half observations are removed
# Sample-routine 91,672 -> 41,411


# # Remove outliers before computing the yearly phosphorus levels
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 34037))
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 85359 & Year==2004))
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 70371 & Year==2013))
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 84857 & Year==2004))
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 69480 & Year==1994))
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 107286 & Year==1999))
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 84464 & Year==2005))
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 85588 & Year==2005))
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 31118 & Year==2011))
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 31217 & Year==2011))
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 31042 & Year==2011)) 
# df_filtered_spatialjoin %<>% filter(!(UniqueID== 103423 & Year==2022)) #51121
# # 41556

# Append the unmatched monitoring stations (tentatively holding)
df_filtered_spatialjoin %<>% rbind(AZ_unmatched) # 51354

# # Aggregate the yearly phosphorus levels by the waterbody (UniqueID = Waterbody ID)
#   df_yearly_wb <- df_filtered_spatialjoin %>%
#                   as.data.frame() %>% 
#                   group_by(UniqueID, Year, StateCd) %>%
#                   summarize(yearlyPlevel = mean(yrlyPlv, na.rm = TRUE), .groups = 'drop') %>%
#                   ungroup() # This is for exploring; not for analysis
  
# Prepare data on the monitoring station level
df_filtered_mn <- df_filtered_spatialjoin # can skip the below if prism is ready
df_filtered_mn <- st_as_sf(df_filtered_mn, crs=4326) # Convert data frame into a shape file 52669 (41420)
st_crs(df_filtered_mn)
address <- "df_filtered_mn.shp"
st_write(df_filtered_mn, dsn = address, layer = "df_filtered_mn") # 41420

save(df_filtered_mn, file = "df_filtered_mn.RData")

#################################################################################################################
# Extract PRISM Data on hpcc
################################################################################################################
# load precipitation data 
load("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/df_ppt_results.RData")
load("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/df_ppt_results2.RData")

# Check the NAs 
for (col in names(df_ppt_results)) {
  var_na <- sum(is.na(df_ppt_results[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}
for (col in names(df_ppt_results2)) {
  var_na <- sum(is.na(df_ppt_results2[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}

# Merge precipitation data on the monitoring station level (directly imported from R)
ppt$Year <- as.numeric(ppt$Year) # 521724
df_filtered_mn <- as.data.frame(df_filtered_mn) # 41411
df_ppt <- df_filtered_mn %>% left_join(ppt, by = c("ID", "Year")) # 41411
df_ppt <- df_ppt %>% filter(!is.na(ppt)) # 41192 Hawaii removed

# Leftjoin new ppt vars as stations with no data are removed now
df_ppt <- df_ppt %>% left_join(df_ppt_results, by = c("ID", "Year", "StateCd")) # 41411
df_ppt <- df_ppt %>% left_join(df_ppt_results2, by = c("ID", "Year")) # 41411

df_ppt %<>% filter(!is.na(HUCEgDC)) # 41112

  # If you do some treatment to the data frame outside of R, check whether ID 33974 contains NA in the lag p variable!!

# Remove unnecessary variable
df_ppt %<>% select(-c(Join_Count, TARGET_FID, OBJECTID)) 

# Merge tmean data on the monitoring station level
load("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/df_tmean_annual.RData")
df_tmean_annual <- tmeanAnnualLong
df_tmean_annual$Year <- as.numeric(df_tmean_annual$Year) # 521724

load("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/temp_df_1004.RData") 
temp_df %<>% ungroup() %>% distinct() # 523404

      NA_ID <- temp_df %>% filter(t_NA==366) %>% select(ID) %>% distinct()  # 40 missing stations
      temp_na <- df_ppt %>% 
        filter(ID %in% NA_ID$ID) %>% 
        select(ID, geometry) %>% distinct()
      save(temp_na, file = "temp_na.RData")


df_pptmean <-  df_ppt %>% 
              left_join(temp_df, by = c("ID", "Year")) %>% 
              left_join(df_tmean_annual, by = c("ID", "Year"))# 41112
df_pptmean <- df_pptmean %>% 
              filter(t_NA!=366) %>% 
              filter(!is.na(tmean)) # 41112

summary <- df_pptmean %>% 
  group_by(ID, Year) %>% 
  summarize(obs = n(), .groups = 'drop')

# Rename the tmean columns for easy modeling
  # Get column names starting with "t_"
  tmean_vars <- grep("^t_", names(df_pptmean), value = TRUE)
  # Loop through matching column names and remove special characters
  for (col_name in tmean_vars) {
    new_col_name <- gsub("\\[|\\)|,", "", col_name)
    new_col_name <- gsub(" ", "", new_col_name)  # Replace blanks with underscore
    names(df_pptmean)[names(df_pptmean) == col_name] <- new_col_name  # Update column name
  }

# Add variables 'Tstatus' and HUCE6&4
  Statecode_interest <- c("04", "06", "12", "15", "30", "32", "34", "40", "49", "55")
  
  adoption_dates <- States %>%
    filter(Code %in% Statecode_interest) %>%  
    select(Code, AdoptionDate, State_full) %>%  
    mutate(AdoptionYear = year(AdoptionDate)) %>% 
    select(Code, AdoptionYear, State_full)
    as.data.frame()  

      # Update Montana Adoption year from 2008 to 2014(2015)
      adoption_dates %<>% mutate(AdoptionYear = ifelse(Code == "30", 2014, AdoptionYear))
  
df_pptmean %<>% left_join(adoption_dates, by = c("StateCd" = "Code")) 
# %>% mutate(adop_ym = ifelse(!is.na(AdoptionDate), format(AdoptionDate, "%Y-%m"), NA_character_))

df_pptmean <- df_pptmean %>%
  mutate(Tstatus = case_when(
    Treated == 1 & AdoptionYear <= Year ~ 1,  # Assign 1 if treated and AdoptionDate is on or before Year
    Treated == 1 & AdoptionYear > Year | Treated == 0 ~ 0  # Assign 0 if treated but AdoptionDate is after Year or if not treated
  ))


# Create different HUCE and upper stream dummy variable
df_pptmean$HUCE6 <- substr(df_pptmean$HUCEgDC, 1, 6)
df_pptmean$HUCE4 <- substr(df_pptmean$HUCEgDC, 1, 4)
df_pptmean$Headstream <- as.integer(substr(df_pptmean$HUCEgDC, nchar(as.character(df_pptmean$HUCEgDC)) - 1, nchar(as.character(df$HUCEgDC))) == "01")


# Check the NAs 
for (col in names(df_pptmean)) {
  var_na <- sum(is.na(df_pptmean[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}
summary(df_pptmean)


############################################################################
# Final steps of the data construction
############################################################################
# Remove stations with less than 10 observations between 1981-2022
  
  # Trim the data
  df_mn <- df_pptmean %>% select(-geometry) %>% as.data.frame() # 41112
  df_mn %<>% mutate(ppt2=ppt*ppt) 
  
    
  # Finish the Intensity variable construction
  
# # Add average temperature for test
#   df_test <- tmeanLong %>% select(c("ID", "Year", "tmean", "test_0", "test_5", "test_10", "test_15", "test_20", "test_25", "test_inf"))
#   df_test$Year <- as.numeric(df_test$Year)
#   df_mn <- left_join(df_mn, df_test, by = c("ID", "Year"))
#   
# #### Path 1: Use the station level data ####
#   # Step 1: Calculate the number of observations per ID
#   summary <- df_mn %>% group_by(ID) %>% summarize(obs = n()) #14290
#   
#   # Step 2: Count the frequency of IDs for each number of observations
#   obs_counts <- summary %>% group_by(obs) %>% summarize(count = n())
#   
#   # Step 3: Plot the histogram
#   ggplot(obs_counts, aes(x = obs, y = count)) +
#     geom_bar(stat = "identity") +
#     labs(x = "Number of Observations", y = "Number of IDs") +
#     theme_minimal() +
#     ylim(0, 3500) +  # Set y-axis limits
#     xlim(0, 15)    # Set x-axis limits
#   
#   # Remove IDs with less than 10 observations
#   summary_filtered <- summary %>% filter(obs>9) %>% select(ID) # 948
#   df_final_mn <- df_mn %>%  filter(ID %in% summary_filtered$ID) # 17515
  
  table(df_mn$RsltVTN)
  # Actual Calculated  Estimated 
  # 40588         79        429 
  
  # df_mn %>% filter(RsltVTN == "Estimated") %>% select(yrlyPlv) %>% summary()
  # df_mn %>% filter(RsltVTN == "Calculated") %>% select(yrlyPlv) %>% summary()
  # df_mn %>% filter(RsltVTN == "Actual") %>% select(yrlyPlv) %>% summary()
  
  df_final_mn <- df_mn %>% filter(RsltVTN=="Actual") # 40588
  df_final_mn %<>% filter(lag == 1 | is.na(lag))
  
  
  # Check the NAs 
  for (col in names(df_final_mn)) {
    var_na <- sum(is.na(df_final_mn[[col]]))
    cat("Column", col, "has", var_na, "missing values.\n")
  }
  summary(df_final_mn$Plvl_lg)
  summary(df_final_mn)
  table(df_final_mn$Tstatus)
  # 0      1 
  # 28837  11751 
  table(df_final_mn$Treated)
  # 0      1 
  # 22620 17968 

  
  
#### Path 2: Use the waterbody level data ####

  df_wb <- as.data.frame(df_mn) # 41112
  
  # Confirm that while waterbodies have multiple HUCE code
  HUC_summary <- df_wb %>%
    distinct(ID, UniqueID, HUCEgDC, .keep_all = FALSE) %>%  
    group_by(UniqueID, HUCEgDC) %>%
    summarize(nHUCE = n(), .groups = "drop")  # UniqueID 38947 156 miles  
  
  df_wb <- df_wb %>%
    mutate(Last_Two_Digits = as.integer(substr(HUCEgDC, nchar(HUCEgDC) - 1, nchar(HUCEgDC)))) %>%
    group_by(UniqueID) %>%
    mutate(HUCE_cluster = HUCEgDC[which.min(Last_Two_Digits)]) %>%
    ungroup() %>% 
    select(UniqueID, Year, ID, HUCEgDC, HUCE_cluster, Last_Two_Digits, everything())
  
# Average over the waterbody
wb_summary <- df_wb %>% 
  group_by(UniqueID, Year) %>% 
  summarize(y_ppt = sum(ppt) / n(),
    `t_-Inf0` = sum(`t_-Inf0`) / n(),
    `t_05` = sum(`t_05`) / n(),
    `t_510` = sum(`t_510`) / n(),
    `t_1015` = sum(`t_1015`) / n(),
    `t_1520` = sum(`t_1520`) / n(),
    `t_2025` = sum(`t_2025`) / n(),
    `t_25Inf` = sum(`t_25Inf`) / n(),
    `yearlyPlevel` = sum(`yrlyPlv`) / n(),
    `below_avg_days` = sum(`below_avg_days`) / n(),
    `above_avg_days` = sum(`above_avg_days`) / n(),
    `rainydays` = sum(`rainydays`) / n(),
    `tmean` = sum(`tmean`) / n()
  ) # 23995 (21680)
# Using grep to find columns starting with 't_' or 'test_'
tmean_vars <- grep("^(t_|test_)", names(df_wb), value = TRUE)

# Remove same name variables + leftjoin
df_wb %<>% select(-all_of(tmean_vars), -below_avg_days, -above_avg_days, -rainydays, -tmean) %>% left_join(wb_summary, by=c("Year", "UniqueID")) # 41112

df_wb %<>% mutate(y_ppt2=y_ppt*y_ppt) 

# Remove unnecessary variable
df_wb %<>% select(-c(yrlyPlv, Plvl_lg, lag, ID, ppt, ppt2, HUCE6, HUCE4))

# Leave the right HUCe code for clustering
df_wb %<>% select(UniqueID, Year, yearlyPlevel, HUCEgDC, HUCE_cluster, everything()) %>% 
            arrange(UniqueID, Year, HUCEgDC, HUCE_cluster)

df_wb %<>% distinct(Year, UniqueID, .keep_all = TRUE) # 21680

# Each segment has one cluster HUCE code now
HUC_summary2 <- df_wb %>%
  distinct(UniqueID, HUCE_cluster, .keep_all = FALSE) %>%  
  group_by(UniqueID) %>%
  summarize(HUC_n = n(), .groups = "drop")  # UniqueID 38947 156 miles  

# Check how many UniqueID has more than one HUCEcode
HUC_summary3 <- df_wb %>%
  distinct(UniqueID, HUCEgDC, .keep_all = FALSE) %>%  
  group_by(UniqueID) %>%
  summarize(HUC_n = n(), .groups = "drop")  # 47 / 3428 have more than one watershed code  

# Create upper level HUCE codes for clustering
df_wb$HUCE6 <- substr(df_wb$HUCE_cluster, 1, 6)
df_wb$HUCE4 <- substr(df_wb$HUCE_cluster, 1, 4)

summary(df_wb$Miles)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.03    6.24   13.83   25.56   27.41  504.08 


# # Create the test temperature variable and precipitation squared
# df_final %<>%
#   mutate(
#     test_0 = ifelse(y_tmean < 0, 1, 0),
#     test_5 = ifelse(y_tmean >= 0 & y_tmean < 5, 1, 0),
#     test_10 = ifelse(y_tmean >= 5 & y_tmean < 10, 1, 0),
#     test_15 = ifelse(y_tmean >= 10 & y_tmean < 15, 1, 0),
#     test_20 = ifelse(y_tmean >= 15 & y_tmean < 20, 1, 0),
#     test_25 = ifelse(y_tmean >= 20 & y_tmean < 25, 1, 0),
#     test_inf = ifelse(y_tmean >= 25, 1, 0)
#   )

# Create a lag variable for each waterbody
  # Ensure the dataframe is sorted by ID and Year
  df_wb %<>% arrange(UniqueID, Year) # 21680

  # Initialize the lagged variables with NA
  df_wb$Plevel_lag <- NA
  df_wb$lag <- NA

  # Loop over each row starting from the second one because the first row can't have a previous year
  for (i in 2:nrow(df_wb)) {
    # Get the current ID and year
    current_id <- df_wb$UniqueID[i]
    current_year <- df_wb$Year[i]
    
    # Filter to find previous years' data for the same ID
    previous_entries <- df_wb[df_wb$UniqueID == current_id & df_wb$Year < current_year, ]
    
    if (nrow(previous_entries) > 0) {
      # Get only those entries where 'yearlyPlevel' is not NA
      available_entries <- previous_entries[!is.na(previous_entries$yearlyPlevel), ]
      
      if (nrow(available_entries) > 0) {
        # Compute the number of years back to the available data
        years_back <- current_year - available_entries$Year
        min_index <- which.min(years_back)  # Find the index of the minimum value in years_back
        
        # Assign the lagged values
        df_wb$Plevel_lag[i] <- available_entries$yearlyPlevel[min_index]
        df_wb$lag[i] <- years_back[min_index]
        
        # Debugging output
        # print(paste("ID:", current_id, "Year:", current_year, "Years back:", years_back, "Lag selected:", years_back[min_index]))
      }
    }
  }
  
# Check a summary to ensure it looks right
summary(df_wb$lag)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   1.000   1.000   1.956   1.000  41.000    3428 

for (col in names(df_wb)) {
  var_na <- sum(is.na(df_wb[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}

# Remove calculated and estimated 
table(df_wb$RsltVTN, useNA = "ifany")
# Actual Calculated  Estimated       <NA> 
#   21317         62        286         15 

# df_wb %>% filter(RsltVTN == "Estimated") %>% select(yearlyPlevel) %>% summary()
# df_wb %>% filter(RsltVTN == "Calculated") %>% select(yearlyPlevel) %>% summary()
# df_wb %>% filter(RsltVTN == "Actual") %>% select(yearlyPlevel) %>% summary()


df_final <- df_wb %>% filter(RsltVTN=="Actual") # 21317
table(df_final$RsltVTN, useNA = "ifany")

# df_final %>% filter(StateCd == "40") %>% select(Headstream) %>% table()

# 
# Select lag levels
summary(df_final$lag)
#   Min.   1st Qu.  Median  Mean    3rd Qu.    Max.    NA's 
#   1.000   1.000   1.000   1.962   1.000     41.000  3359 
table(df_final$lag)
# 1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17 
# 15190   649   403   308   257   190   132   106    82    73    66    54    40    27    37    38    41 
# 18    19    20    21    22    23    24    25    26    27    28    29    30    31    32    33    34 
# 28    26    21    23    33    20    13    12    11     8     7     3    11     4     6    19     8 
# 35    36    37    38    39    41 
# 1     6     2     1     2     1 


# Initialize an empty data frame to store results
all_states_lags <- data.frame(State = character(), Lag = character(), Count = integer())

# Loop through each unique state
for(state in unique_states) {
  # Filter data for the current state
  df_state <- df_final %>% filter(StateCd == state)
  
  # Create a table of 'lag' values
  lag_table <- table(df_state$lag)
  
  # Convert the table to a data frame and ensure all transformations are applied immediately
  df_lag_table <- as.data.frame(lag_table, stringsAsFactors = FALSE)
  df_lag_table$Var1 <- as.character(df_lag_table$Var1)  # Convert Lag to character immediately
  
  # Rename columns to match the intended output
  names(df_lag_table) <- c("Lag", "Count")
  df_lag_table$State <- unique(df_state$State_full)  # Add state name
  
  # Combine with the overall results data frame
  all_states_lags <- bind_rows(all_states_lags, df_lag_table)
}

all_states_lags <- all_states_lags %>% 
  arrange(State)


# Prepare a data frame with all possible lag values
all_lags <- data.frame(Lag = as.character(1:max(as.numeric(all_states_lags$Lag), na.rm = TRUE)))

# Initialize an empty list to store each state's data frame
list_of_dfs <- list()

# Loop through each unique state
for (state in unique(all_states_lags$State)) {
  # Filter data for the current state
  df_state <- all_states_lags %>%
    filter(State == state) %>%
    select(Lag, Count) %>%
    rename(!!state := Count)  # Dynamically rename 'Count' to the state name
  
  # Ensure all lag values are present, filling missing lags with zero count
  df_state <- full_join(all_lags, df_state, by = "Lag", suffix = c("", "_y")) %>%
    mutate(!!state := coalesce(!!sym(state), 0)) %>%
    select(-matches("_y$"))
  
  # Store the modified data frame in a list using the state name as the key
  list_of_dfs[[state]] <- df_state
}

# Combine all data frames into one wide-format data frame
wide_df <- reduce(list_of_dfs, full_join, by = "Lag")

# Print the wide-format data frame
print(wide_df)


# Combine all data frames into one wide-format data frame
# Using reduce from the purrr package to merge all data frames in the list by 'Lag'
library(purrr)
library(dplyr)
wide_df <- reduce(list_of_dfs, full_join, by = "Lag")

# Print the wide-format data frame
print(wide_df)

#################################################################################
# Construct HUCE dataset
df_huc <- as.data.frame(df_mn) # 41112

# Average over the waterbody
huc_summary <- df_huc %>% 
  group_by(HUCEgDC, Year) %>% 
  summarize(y_ppt = sum(ppt) / n(),
            `t_-Inf0` = sum(`t_-Inf0`) / n(),
            `t_05` = sum(`t_05`) / n(),
            `t_510` = sum(`t_510`) / n(),
            `t_1015` = sum(`t_1015`) / n(),
            `t_1520` = sum(`t_1520`) / n(),
            `t_2025` = sum(`t_2025`) / n(),
            `t_25Inf` = sum(`t_25Inf`) / n(),
            `yearlyPlevel` = sum(`yrlyPlv`) / n(),
            `below_avg_days` = sum(`below_avg_days`) / n(),
            `above_avg_days` = sum(`above_avg_days`) / n(),
            `rainydays` = sum(`rainydays`) / n(),
            `tmean` = sum(`tmean`) / n()
  ) # 23995 (21680)
# Using grep to find columns starting with 't_' or 'test_'
tmean_vars <- grep("^(t_|test_)", names(df_huc), value = TRUE)

# Remove same name variables + leftjoin
df_huc %<>% select(-all_of(tmean_vars), -tmean, -below_avg_days, -above_avg_days, -rainydays) %>% left_join(huc_summary, by=c("Year", "HUCEgDC")) # 

df_huc %<>% mutate(y_ppt2=y_ppt*y_ppt) 

# Remove unnecessary variable
df_huc %<>% select(-c(yrlyPlv, Plvl_lg, lag, ID, UniqueID, ppt, ppt2))

# Leave the right HUCE code for clustering
df_huc %<>% select(HUCEgDC, Year, yearlyPlevel, everything()) %>% 
  arrange(HUCEgDC, Year) # 41112

df_huc %<>% distinct(Year, HUCEgDC, .keep_all = TRUE) # 8067














# Import State Finance data
finance <- readxl::read_excel("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/StateFinanceData/SewerageExpenditure.xlsx", sheet = "Data") # check the sheet name before importing

# Merge with States to obtain StateCodes
finance$State <- substr(finance$Name, 1, 2)
finance <- finance %>% left_join(select(States, State, Code), by = "State") %>% rename(StateCd=Code)

# Merge finance data
df_final_mn <- df_final_mn %>% 
  left_join(select(finance, Year, StateCd, SW_propor), by = c("Year", "StateCd")) # 18818

df_final <- df_final %>% 
  left_join(select(finance, Year, StateCd, SW_propor), by = c("Year", "StateCd")) # 24002
  
check <- df_final_mn %>% filter(is.na(SW_propor)) %>% group_by(StateCd, Year) %>%  summarize()

# Drop the NAs in finance
df_final_sw2 <- df_final_mn %>% filter(!is.na(SW_propor)) # 17295
df_final_sw1 <- df_final %>% filter(!is.na(SW_propor)) # 21681
  

# Save the data
save(df_final, file = "df_final.RData")
save(df_final_mn, file = "df_final_mn.RData")
save(df_final_did, file = "df_final_did.RData")
