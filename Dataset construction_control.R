library(pacman)
p_load(readxl, dataRetrieval, dplyr, lubridate, sf, tidyr, magrittr, zoo, ggplot2, DescTools, stringr, knitr)
#desktop
maindir <- "C:\\Users\\Sol\\OneDrive - Michigan State University\\Saesol and Molly Shared Work\\Water Quality\\Data Compilation"
setwd(maindir)

# Data prep
#1. Create an empty data frame 
extracted_df <- data.frame(StateCode = character(),
                           MonitoringLocationIdentifier = character(),
                           MonitoringLocationName = character(),
                           HUCEightDigitCode = character(),
                           Latitude=character(),
                           Longitude=character(),
                           stringsAsFactors = FALSE)

#2. Load the data containing adoption dates
#desktop
maindir <- "C:\\Users\\Sol\\OneDrive - Michigan State University\\Saesol and Molly Shared Work\\Water Quality\\Data Compilation"
setwd(maindir)

States <- read_excel("States.xlsx", sheet = "RImport") # check the sheet name before importing

#3. Download the data

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
                    ME, MD, MI, MS, NY, NC, ND, OH, PA, TN, TX, WY) # 2287518
# Check the number of columns in each data frame
sapply(list(AL, AK, AR, CT, DE, DC, ID, IN, IA, KS, KY, LA, ME, MD, MI, MS, NY, NC, ND, OH, PA, TN, TX, WY), ncol)

df_control_concise <- df_control %>% 
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
  ) # Select variables of use 532272


# # site_data_control <- list()
# # Exclude specific state codes from the States dataframe
# Statecode_interest <- c("01", "02", "05", "09", "10", "11", "16", "18", "19", "20", #
#                         "21", "22", "23", "24", "26", "28", "36", "37", "38", "39",  #
#                         "42", "47", "48", "56" )
# 
# for (state_code in Statecode_interest) {
#   # Retrieve data for the current state code and characteristic name
#   current_sites <- whatWQPsites(statecode = state_code, characteristicName = "Phosphorus")
#   site_data_control[[state_code]] <- current_sites
# }
# 
# for (state_code in Statecode_interest) {
#   site_data_control[[state_code]] <- site_data_control[[state_code]] %>%
#     select(StateCode, CountyCode, MonitoringLocationIdentifier, MonitoringLocationName,
#            HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, ProviderName,
#            OrganizationIdentifier, OrganizationFormalName)
# }
# df_controlsite <- bind_rows(site_data_control) # 532272

df_control_concise %<>% left_join(df_controlsite, by = "MonitoringLocationIdentifier") #532272
df_control_concise %<>% filter(!is.na(LongitudeMeasure) & !is.na(LatitudeMeasure)) # Remove none

# add 12345 to ID to avoid overlaps
df_control_concise %<>%
  mutate(LatLong = paste(LatitudeMeasure, LongitudeMeasure))

df_control_ID <- df_control_concise %>%
  distinct(LatLong) %>%
  mutate(ID = row_number() + 123456) %>%
  ungroup()  # 43354 monitoring stations (36302)

df_control_concise %<>% left_join(df_control_ID, by="LatLong") %>% select(-LatLong) # 670568

# Check the NAs 
for (col in names(df_control_concise)) {
  var_na <- sum(is.na(df_concise[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}
# table(df_control_concise$StateCode)
# Filter rows before 1981 and after 2022; df_filtered is filtered for 1981-2022 & mg/L & annual p level
rows_to_remove <- df_control_concise$Year < 1981 | df_control_concise$Year > 2022 # Make sure to remove 1980 later
df_control_filtered <- df_control_concise[!rows_to_remove, ] # 386498

# Convert the measurement unit
table(df_control_filtered$Unit)
# mg/l      mg/L mg/l as P  mg/l PO4       ppm      ug/L 
# 188     38092    308876     24174       435     14733

df_control_filtered %<>% mutate(Unit=ifelse(Unit %in% c("mg/l", "mg/l as P"), "mg/L", 
                                            ifelse(Unit=="ug/l","ug/L", Unit))) %>% 
  filter(Unit=="mg/L" | Unit=="ug/L") # 361889

# Remove problematic nutrient values before conversion
df_control_filtered %<>% mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
  filter(!is.na(ResultMeasureValue), ResultMeasureValue >= 0) # 374961

# multiply 0.001 if the unit is ug/L and replace the unit as mg/L
df_control_filtered %<>% mutate(ResultMeasureValue = ifelse(Unit == "ug/L", 0.001 * ResultMeasureValue, ResultMeasureValue)) %>% 
  mutate(Unit=ifelse(Unit=="ug/L", "mg/L", Unit))
table(df_control_filtered$Unit) # 361853

# There are calculated and estimated values; dropping this would affect the winsorized values
table(df_control_filtered$ResultValueTypeName)
# Actual Calculated  Estimated 
# 356485         55       5313 
df_control_filtered <- df_control_filtered %>% filter(ResultValueTypeName=="Actual") # 40588
df_control_filtered %<>% mutate(Day=day(ActivityStartDate)) 


                # Test the water pollution trend here
                df_daily2 <- df_control_filtered %>% 
                  group_by(ID, Year, Month, Day) %>%
                  summarize(NumObs = n(), 
                            dailyPlevel = sum(ResultMeasureValue) / n()  ) %>% 
                  ungroup() 
                df_filtered_ctrl_trend <- df_control_filtered %>%  left_join(df_daily2, by = c("ID", "Year", "Month", "Day")) %>% 
                  select(ID, ActivityStartDate, Year, Month, Day, StateCode,CountyCode, HUCEightDigitCode, dailyPlevel ) %>% 
                  group_by(ID, ActivityStartDate) %>%
                  distinct()
                sapply(df_filtered_ctrl_trend, function(x) sum(is.na(x)))
                summary(df_filtered_ctrl_trend)
                
                df_filtered_summary <- df_filtered_ctrl_trend %>%  group_by(ID, ActivityStartDate) %>% summarize(NumObs = n())
                
                feols(dailyPlevel ~ Year + Month + Day | ID + StateCode, data = df_filtered_ctrl_trend, cluster="HUCEightDigitCode" )

# Remove duplicates. This treatment should take place before computing yearly p level
df_control_filtered %<>% distinct(ID, ActivityStartDate, ResultMeasureValue, .keep_all = TRUE) # 349678

# Winsorize at 99% 
df_control_filtered %<>% group_by(StateCode) %>%
  mutate(Plv_winsorized = Winsorize(ResultMeasureValue, probs = c(0.005, 0.995)))

                
# Construct the yearly P levels
# 1. Average the phosphorus levels on a daily/monthly basis
df_daily <- df_control_filtered %>% mutate(Day=day(ActivityStartDate)) %>% 
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
df_control_filtered %<>% left_join(df_yearly, by = c("ID", "Year")) %>% 
  group_by(ID, Year) %>%
  distinct() %>%
  ungroup() # 349678

# Remove unnecessary variables before leaving one annual observation per ID
df_control_filtered %<>%  select(-c(Month, NumObs, ActivityMediaSubdivisionName, ActivityIdentifier, 
                                    ActivityTypeCode, MonitoringLocationIdentifier, 
                                    SampleCollectionMethod.MethodIdentifier, OrganizationIdentifier, 
                                    ActivityStartDate, ResultMeasureValue, Plv_winsorized)) %>%  
  distinct() # 53997

# Check whether duplicates are removed. Not all due to changing monitoring location name
df_filtered_summary <- df_control_filtered %>%  group_by(ID, Year) %>% summarize(NumObs = n())

# Remove the duplicates by ID, Year.
df_control_filtered %<>% distinct(ID, Year, .keep_all=TRUE) # 50264

table(df_control_filtered$ResultValueTypeName)
# Actual Calculated  Estimated 
# 49328         25        911 

# Create a lagged variable
# Ensure your data is sorted by ID and Year
df_control_filtered_lag <- df_control_filtered %>%  arrange(ID, Year)

# Initialize the lagged variables with NA
df_control_filtered_lag$Plevel_lag <- NA
df_control_filtered_lag$lag <- NA

# Loop over each row starting from the second one because the first row can't have a previous year
for (i in 2:nrow(df_control_filtered_lag)) {
  # Get the current ID and year
  current_id <- df_control_filtered_lag$ID[i]
  current_year <- df_control_filtered_lag$Year[i]
  
  # Filter to find previous years' data for the same ID
  previous_entries <- df_control_filtered_lag[df_control_filtered_lag$ID == current_id & df_control_filtered_lag$Year < current_year, ]
  
  if (nrow(previous_entries) > 0) {
    # Get only those entries where 'yearlyPlevel' is not NA
    available_entries <- previous_entries[!is.na(previous_entries$yearlyPlevel), ]
    
    if (nrow(available_entries) > 0) {
      # Compute the number of years back to the available data
      years_back <- current_year - available_entries$Year
      min_index <- which.min(years_back)  # Find the index of the minimum value in years_back
      
      # Assign the lagged values
      df_control_filtered_lag$Plevel_lag[i] <- available_entries$yearlyPlevel[min_index]
      df_control_filtered_lag$lag[i] <- years_back[min_index]
      
      # Debugging output
      # print(paste("ID:", current_id, "Year:", current_year, "Years back:", years_back, "Lag selected:", years_back[min_index]))
    }
  }
}

# Check a summary to ensure it looks right
summary(df_control_filtered_lag$lag)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   1.000   1.000   1.472   1.000  40.000   14345 
df_control_mn <- df_control_filtered_lag %>% filter(ResultValueTypeName=="Actual") # 49328

table(df_control_mn$Year)
# 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 
# 1945 1476 1286 1117 1104 1166 1171 1252 1196 1299 1196 1167 1098 1269  949  769  711  746  869 1151 1158 1114 
# 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 
# 1156 1195 1043 1108 1170 1102 1079 1088 1068 1275 1613 1412 1432 1313 1215 1424 1289 1005 1106 1026 

table(df_control_mn$StateCode)
# 01   02   05   09   10   11   16   18   19   20   21   22   23   24   26   28   36   37   38   39   42   47 
# 1109  658 4591 1774  106   98 2498  634  967 1467  590 1642 1915 1062 3366 1506 4068 5581 2653 1317 4600  476 
# 48   56 
# 5123 1527 

# Construct HUCE dataset
df_control_huc <- as.data.frame(df_control_mn) # 41112

# Average over the waterbody
huc_summary <- df_control_huc %>% 
  group_by(HUCEightDigitCode, Year) %>% 
  summarize(`yearlyPlevel` = sum(`yearlyPlevel`) / n(),
  ) 

# Remove same name variables + leftjoin
df_control_huc %<>% select(-yearlyPlevel, -Plevel_lag, -lag, -ID, -ResultValueTypeName) %>% left_join(huc_summary, by=c("Year", "HUCEightDigitCode")) # 

# Leave the right HUCE code for clustering
df_control_huc %<>% select(HUCEightDigitCode, Year, yearlyPlevel, everything()) %>% 
  arrange(HUCEightDigitCode, Year) # 41112

df_control_huc %<>% distinct(Year, HUCEightDigitCode, .keep_all = TRUE) # 14467

table(df_control_huc$Year)
# 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 
# 541  476  456  418  425  421  377  415  404  414  407  397  371  342  260  229  217  224  237  274  279  299 
# 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 
# 312  349  300  312  306  296  311  313  299  324  414  371  352  368  338  348  332  312  321  306 

table(df_control_huc$StateCode)
# 01   02   05   09   10   11   16   18   19   20   21   22   23   24   26   28   36   37   38   39   42   47 
# 416  352 1132  340   32   28  866  317  558  349  289  674  300  284 1075  379  984  893  945  437 1090  278 
# 48   56 
# 1720  729 

save(df_control_mn, df_control_huc, file = "df_control.RData")


################################################################################
# Merge PRISM
################################################################################
# load precipitation data 
load("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/df_ppt_control_results.RData")
load("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/df_ppt_control_results2.RData")
load("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/df_ppt_control_annual.RData")

# rename
df_ppt_control_results <- df_ppt_results
df_ppt_control_results2 <- df_ppt_results2
df_ppt_control_annual <- ppt_annual %>% select(-StateCd) 

summary(df_ppt_control_results)
summary(df_ppt_control_results2)
summary(df_ppt_control_annual) # Year is character; convert it
summary(df_control_tmean_annual) # Year is character; convert it

df_ppt_control_annual$Year <- as.numeric(df_ppt_control_annual$Year) # 594300

df_ppt_control <- df_ppt_control_results %>% 
  left_join(df_ppt_control_results2, by = c("ID", "Year")) %>% 
  left_join(df_ppt_control_annual, by = c("ID", "Year")) # 594300

# Check the NAs 
for (col in names(df_ppt_control)) {
  var_na <- sum(is.na(df_ppt_control[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}
# Column Year has 0 missing values.
# Column ID has 0 missing values.
# Column below_avg_days has 0 missing values.
# Column above_avg_days has 0 missing values.
# Column StateCd has 0 missing values.
# Column rainydays has 0 missing values.
# Column ppt has 18816 missing values.

# Draw IDs with ppt NAs
ppt_control_na <- df_ppt_control %>% filter(is.na(ppt)) %>% select(ID) %>% distinct() # 448 stations

# merge with the main data and remove NAs
df_control_ppt <- df_control_mn %>% left_join(df_ppt_control, by = c("ID", "Year")) # 49328
df_control_ppt %<>% filter(!is.na(ppt)) # 47517

# merge with the main data
df_control_ppt %<>% filter(!is.na(HUCEightDigitCode)) # 47505

# Remove unnecessary variable
df_control_ppt %<>% select(-c(ResultValueTypeName, StateCd, State)) 

# Merge tmean data on the monitoring station level
load("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/df_control_tmean_annual.RData")
load("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/Data Compilation/df_temp_control.RData") #605052
df_temp_control <- temp_df #2071776

df_temp_control %<>% ungroup() %>% distinct() # 594300
df_control_tmean_annual <- tmeanAnnualLong
df_control_tmean_annual$Year <- as.numeric(df_control_tmean_annual$Year) # 

NA_ID <- df_temp_control %>% filter(t_NA==366) %>% select(ID) %>% distinct()  # same 448 stations

# Extract NA ID and coordinate info
NA_df <- NA_ID %>% left_join(df_control_mn, by = "ID") %>% 
  select(ID, LatitudeMeasure, LongitudeMeasure) %>% 
  distinct() %>% 
  st_as_sf(coords=c("LongitudeMeasure", "LatitudeMeasure"), crs=4326)   
save(NA_df, file = "PRISM_NA_ControlStates.RData" )

df_control_pptmean <-  df_control_ppt %>% 
  left_join(df_temp_control, by = c("ID", "Year"))  %>% 
  left_join(df_control_tmean_annual, by = c("ID", "Year")) # 47505
df_control_pptmean %<>% filter(t_NA!=366) # 47505

# Rename the tmean columns for easy modeling
# Get column names starting with "t_"
tmean_vars <- grep("^t_", names(df_control_pptmean), value = TRUE)
# Loop through matching column names and remove special characters
for (col_name in tmean_vars) {
  new_col_name <- gsub("\\[|\\)|,", "", col_name)
  new_col_name <- gsub(" ", "", new_col_name)  # Replace blanks with underscore
  names(df_control_pptmean)[names(df_control_pptmean) == col_name] <- new_col_name  # Update column name
}

# Create different HUCE and upper stream dummy variable
df_control_pptmean$HUCE6 <- substr(df_control_pptmean$HUCEightDigitCode, 1, 6)
df_control_pptmean$HUCE4 <- substr(df_control_pptmean$HUCE6, 1, 4)
df_control_pptmean$Headstream <- as.integer(substr(
  df_control_pptmean$HUCEightDigitCode, 
  nchar(as.character(df_control_pptmean$HUCEightDigitCode)) - 1, 
  nchar(as.character(df_control_pptmean$HUCEightDigitCode))
) == "01")

df_control_pptmean %<>% mutate(ppt2=ppt*ppt) 

# Check the NAs 
for (col in names(df_control_pptmean)) {
  var_na <- sum(is.na(df_control_pptmean[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}
# Column Unit has 0 missing values.
# Column Year has 0 missing values.
# Column StateCode has 0 missing values.
# Column CountyCode has 0 missing values.
# Column MonitoringLocationName has 0 missing values.
# Column HUCEightDigitCode has 0 missing values.
# Column LatitudeMeasure has 0 missing values.
# Column LongitudeMeasure has 0 missing values.
# Column ProviderName has 0 missing values.
# Column OrganizationFormalName has 0 missing values.
# Column ID has 0 missing values.
# Column yearlyPlevel has 0 missing values.
# Column Plevel_lag has 13604 missing values.
# Column lag has 13604 missing values.
# Column State_full has 0 missing values.
# Column below_avg_days has 0 missing values.
# Column above_avg_days has 0 missing values.
# Column rainydays has 0 missing values.
# Column ppt has 0 missing values.
# Column t_05 has 0 missing values.
# Column t_510 has 0 missing values.
# Column t_1015 has 0 missing values.
# Column t_1520 has 0 missing values.
# Column t_2025 has 0 missing values.
# Column t_25Inf has 0 missing values.
# Column t_-Inf0 has 0 missing values.
# Column t_NA has 0 missing values.
# Column HUCE6 has 0 missing values.
# Column HUCE4 has 0 missing values.
summary(df_control_pptmean)

# Construct HUCE dataset
df_control_huc <- df_control_pptmean # 47505

# Average over the waterbody
huc_summary <- df_control_huc %>% 
  group_by(HUCEightDigitCode, Year) %>% 
  summarize(ppt = sum(ppt) / n(),
            `t_-Inf0` = sum(`t_-Inf0`) / n(),
            `t_05` = sum(`t_05`) / n(),
            `t_510` = sum(`t_510`) / n(),
            `t_1015` = sum(`t_1015`) / n(),
            `t_1520` = sum(`t_1520`) / n(),
            `t_2025` = sum(`t_2025`) / n(),
            `t_25Inf` = sum(`t_25Inf`) / n(),
            tmean = sum(tmean / n()),
            `yearlyPlevel` = sum(`yearlyPlevel`) / n(),
            `below_avg_days` = sum(`below_avg_days`) / n(),
            `above_avg_days` = sum(`above_avg_days`) / n(),
            `rainydays` = sum(`rainydays`) / n(),
  ) # 23995 (21680)
# Using grep to find columns starting with 't_' or 'test_'
tmean_vars <- grep("^t", names(df_control_huc), value = TRUE)

# Remove same name variables + leftjoin
df_control_huc %<>% select(-ID, -all_of(tmean_vars), -yearlyPlevel, -ppt, -below_avg_days, -above_avg_days, -rainydays) %>% left_join(huc_summary, by=c("Year", "HUCEightDigitCode")) # 

df_control_huc %<>% mutate(ppt2=ppt*ppt) 

# Leave the right HUCE code for clustering
df_control_huc %<>% select(HUCEightDigitCode, Year, yearlyPlevel, everything()) %>% 
  arrange(HUCEightDigitCode, Year) # 47505

df_control_huc %<>% distinct(Year, HUCEightDigitCode, .keep_all = TRUE) # 13952



# Summary stat
df_control_pptmean %>% select(ID) %>% distinct() %>% count()
df_control_pptmean %>% count()
