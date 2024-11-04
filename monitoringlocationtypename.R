mnlocationtype <- df_site %>% 
  select(LatitudeMeasure, LongitudeMeasure, MonitoringLocationTypeName, MonitoringLocationIdentifier, MonitoringLocationName) %>% 
  st_as_sf(coords=c("LongitudeMeasure", "LatitudeMeasure"), crs=4326) 

df_pptmean2 <- df_pptmean %>% st_as_sf(crs=4326)

# sf_joined2 <- left_join(df_pptmean, mnlocationtype, by="geometry")

# same location stations have two different types
sf_joined <- st_join(df_pptmean2, mnlocationtype, join = st_equals, left = TRUE) %>% 
              as.data.frame() %>% 
              # select(ID, MonitoringLocationTypeName, Year, MntrnLN) %>%
               # select(ID, MonitoringLocationTypeName) %>%
              select(ID, MonitoringLocationTypeName, MonitoringLocationName, MntrnLN, everything())
              
sf_joined_count <- sf_joined %>% 
  group_by(ID) %>% 
  summarize(count=n()) %>% 
  ungroup() 

sf_uniqueID <- sf_joined_count %>% filter(count==1) %>% select(ID) 

sf_joined_simple <- sf_joined %>% select(ID, MonitoringLocationTypeName) %>% distinct()
  
sf_uniqueID_joined <-  left_join(sf_uniqueID, sf_joined_simple, by="ID")

df_final_mn2 <-  left_join(df_final_mn, sf_uniqueID_joined, by="ID")
                        
                        