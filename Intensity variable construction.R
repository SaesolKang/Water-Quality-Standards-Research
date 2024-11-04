# Largest Unique ID 112968
library(pacman)
p_load(dplyr, magrittr, stringr, sf)

maindir <- "C:\\Users\\Sol\\OneDrive - Michigan State University\\Saesol and Molly Shared Work\\Water Quality\\Data Compilation"
setwd(maindir)

# Load the data after processing in Dataset construction
Treated_Rivers_and_Streams_withID <- as.data.frame(Treated_Rivers_and_Streams_withID) 

# Sort alphabetically
Treated_Rivers_and_Streams_withID <- Treated_Rivers_and_Streams_withID[order(Treated_Rivers_and_Streams_withID$State), ]

# 1/8. Arizona
Treated_AZ <- Treated_Rivers_and_Streams_withID %>%
  filter(str_detect(State, "AZ")) %>%  
  mutate(Intensity = case_when(
    Name == "Black River"           ~ 0.10,
    Name == "Colorado River"        ~ 0.20,
    Name == "Little Colorado River" ~ 0.08,
    Name == "Oak Creek"             ~ 0.10,  
    Name == "Salt River"            ~ 0.05,   
    Name == "Tonto Creek"           ~ 0.10,   
    Name == "Verde River"           ~ 0.10,   
    TRUE                            ~ 0     # Default value if none of the above conditions are met
  ))
# Remove the untreated ones at the border
Treated_AZ %<>% filter(Intensity!=0) 

# 2/8. California  
Treated_CA <- Treated_Rivers_and_Streams_withID %>%
  filter(State == "CA") %>%  
  mutate(Intensity = case_when(
    Name == "Baxter Creek "           ~ 0.12,
    Name == "Bryant Creek"            ~ 0.02,
    Name == "Eagle Creek"             ~ 0.01,  
    Name == "East Fork Carson River"  ~ 0.02,  
    Name == "East Walker River"  ~ 0.06,  
    Name == "Gold Run Creek"  ~ 0.02,  
    Name == "Lassen Creek"  ~ 0.02,  
    UniqueID == "34974" ~ 0.05, # Little Truckee River below Boca Reservoir
    UniqueID == "34497" ~ 0.04, # Little Truckee River above Independence Creek 
    # ID == "29645" ~ 0.03, # Independence Creek UniqueID needed 
    Name == "Martis Creek"  ~ 0.25,  
    Name == "McKinney Creek"  ~ 0.015,  
    Name == "Meeks Creek"  ~ 0.01,  
    Name == "Pine Creek"  ~ 0.06,  
    Name == "Robinson Creek"  ~ 0.02,  
    UniqueID == "33094" ~ 0.25, # Susan River near Litchfield at  Hwy. 395 
    Name == "Upper Truckee River"  ~ 0.015,  
    Name == "West Walker River"  ~ 0.01,  
    Name == "Willard Creek"  ~ 0.03,  
    # ID == "29468" ~ 0.03, # Truckee River at Lake Tahoe Outlet (13) 
    # ID == "1533" ~ 0.03, # Truckee River at Lake Tahoe Outlet  (13)
    # ID == "30734" ~ 0.10, # Bear Creek at Mouth (11)
    # ID == "1850" ~ 0.10, # Truckee River above Bear Creek (12)   
    # ID == "30941" ~ 0.10, # Truckee River above Bear Creek (12)   
    # ID == "2285" ~ 0.13, # Truckee River below Bear Creek (10)
    # ID == "31158" ~ 0.13, # Squaw Creek at Mouth (8)
    # ID == "29371" ~ 0.29, # Truckee River below Donner Creek (5)
    # ID == "29362" ~ 0.29, # Truckee River below Martis Creek (4)
    # ID == "29403" ~ 0.29, # Truckee River below Martis Creek (4)
    # ID == "30836" ~ 0.29, # Truckee River below Martis Creek (4)
    # ID == "30167" ~ 0.30, # Truckee River at stateline (1)   
    # ID == "1640" ~ 0.02, # West Fork Carson River at Woodfords (1)  
    # ID == "29565" ~ 0.02, # West Fork Carson River at Woodfords  
    # ID == "30702" ~ 0.02, # West Fork Carson River at Woodfords  
    # ID == "31903" ~ 0.02, # West Fork Carson River at Woodfords  
    UniqueID == "32525" ~ 0.10, # Willow Creek at Merrilville Rd
    UniqueID == "33029" ~ 0.05, # Willow Creek at Co. Road 216 
    
    TRUE                            ~ 0     # Default value if none of the above conditions are met
  ))


# 3/8. Florida part 1. treatment by region (the order should not be swapped as name specific rivers have different criteria)
Treated_FL_regions <- st_read("Florida_SpatialJoin_intensity.shp") # 805
Treated_FL_regions <- as.data.frame(Treated_FL_regions) %>%
                      mutate(Intensity = 0)
Treated_FL_regions %<>% 
  mutate(
    Intensity = case_when(
      NUTRIENT_W == "North Central"   ~ 0.30,
      NUTRIENT_W == "Panhandle East"  ~ 0.18,
      NUTRIENT_W == "Panhandle West"  ~ 0.06,
      NUTRIENT_W == "Peninsular"      ~ 0.12,
      NUTRIENT_W == "West Central"    ~ 0.49,
      TRUE                            ~ Intensity  # Preserve existing values by default
    )
  )
Treated_FL_regions %<>% select(c("UniqueID", "Intensity"))

# 3/8. Florida part 2. left join
Treated_FL <- Treated_Rivers_and_Streams_withID %>%
  filter(str_detect(State, "FL")) # 779 why does it have less?
Treated_FL %<>% left_join(Treated_FL_regions, by=c("UniqueID"))
  
# 3/8. Florida part 3. Update the criteria by name
Treated_FL %<>% mutate(Intensity = case_when(
  UniqueID == "39914" ~ 0.093, # Upper Saint Mary's River
  UniqueID == "39912" ~ 0.113, # Middle Saint Mary's River
  UniqueID == "112969" ~ 0.107
  TRUE                            ~ 0     # 779
))

# 4/8. Nevada

Treated_NV <- Treated_Rivers_and_Streams_withID %>%
  filter(str_detect(State, "NV")) %>%  
  mutate(Intensity = case_when(
    Name == "Barley Creek"           ~ 0.10,
    Name == "Barley Creek"           ~  0.10,
    Name == "Big Creek"           ~  0.10,
    Name == "Bilk Creek"           ~  0.10,
    Name == "Birch Creek"           ~  0.10,
    Name == "Bronco Creek"           ~  0.10,
    Name == "Bruneau River"           ~  0.10,
    Name == "Bryant Creek"           ~  0.05,
    Name == "Camp Creek"           ~  0.10,
    Name == "Canyon Creek"           ~  0.10,
    Name == "Carson River"           ~  0.10,
    Name == "Chiatovich Creek"           ~  0.06,
    Name == "Clear Creek"           ~  0.10,
    Name == "Clover Creek"           ~  0.10,
    UniqueID == "32182"                ~ 0.03, # Colorado River below Davis Dam
    UniqueID == "32241"                ~ 0.03, # Colorado River below Davis Dam
    UniqueID == "32242"                ~ 0.03, # Colorado River below Davis Dam
    Name == "Cottonwood Creek"           ~  0.10,
    Name == "Currant Creek"           ~  0.10,
    Name == "Deep Creek"           ~  0.10,
    Name == "Denay Creek"           ~  0.10,
    Name == "Desert Creek"           ~  0.13,
    Name == "Duck Creek"           ~  0.10,
    Name == "Dutch John Creek"           ~  0.10,
    Name == "Eagle Rock Creek"           ~  0.05,
    
    # ID == "23996"           ~ 0.10, # East Fork of the Carson River from the California-Nevada state line to the Riverview Mobile Home Park at U.S. Highway 395 south of Gardnerville
    # ID == "33985"           ~ 0.10, # East Fork of the Carson River from the California-Nevada state line to the Riverview Mobile Home Park at U.S. Highway 395 south of Gardnerville
    # 
    # ID == "23955"           ~ 0.10, # East Fork of the Carson River from the Riverview Mobile Home Park at U.S. Highway 395 to Muller Lane
    # ID == "23956"           ~ 0.10, # East Fork of the Carson River from the Riverview Mobile Home Park at U.S. Highway 395 to Muller Lane
    # ID == "23984"           ~ 0.10, # East Fork of the Carson River from the Riverview Mobile Home Park at U.S. Highway 395 to Muller Lane
    # ID == "33943"           ~ 0.10, # East Fork of the Carson River from the Riverview Mobile Home Park at U.S. Highway 395 to Muller Lane
    # 
    # ID == "23786"           ~ 0.10, # East Fork Carson River from Muller Lane to the West Fork
    # ID == "23864"           ~ 0.10, # East Fork Carson River from Muller Lane to the West Fork
    # ID == "23997"           ~ 0.10, # East Fork Carson River from Muller Lane to the West Fork
    # ID == "33983"           ~ 0.10, # East Fork Carson River from Muller Lane to the West Fork
    # ID == "33949"           ~ 0.10, # East Fork Carson River from Muller Lane to the West Fork
    # ID == "34298"           ~ 0.065, # East Fork Carson River from Muller Lane to the West Fork
    # ID == "34242"           ~ 0.10, # East Fork of the Carson River from Muller Lane to the West Fork
    # 
    # ID == "33939"           ~ 0.10, # the main stem of the Carson River from the confluence of the East and West Forks to Genoa Lane
    # 
    # ID == "33981"           ~ 0.10, # West Fork of the Carson River from the California-Nevada state line to the East Fork
    # ID == "34206"           ~ 0.10, # West Fork of the Carson River from the California-Nevada state line to the East Fork
    
    Name == "East Fork Jarbidge River"           ~  0.10,
    Name == "East Fork Quinn River"           ~  0.10,
    Name == "East Walker River"           ~  0.10,
    # Name == "First Creek"           ~  , not existing
    Name == "Goose Creek"           ~  0.10,
    Name == "Gray Creek"           ~  0.10,
    Name == "Harrington Creek"           ~  0.10,
    Name == "Hendricks Creek"           ~  0.10,
    Name == "Humboldt River"           ~ 0.1,
    
    # ID == "34199"               ~ 0.33, # Humboldt River from Woolsey to Rodgers Dam
    
    Name == "Huntington Creek"           ~  0.10,
    Name == "Jack Creek"           ~  0.10,
    Name == "Jarbidge River"           ~  0.05,
    Name == "Lamoille Creek"           ~  0.10,
    Name == "Leonard Creek"           ~  0.10,
    Name == "Lewis Creek"           ~  0.10,
    Name == "Little Humboldt River"           ~  0.33,
    
    Name == "Maggie Creek"           ~ 0.10,
    
    # ID == "34039"                   ~ 0.33, # Maggie Creek from its confluence with Soap Creek to its confluence with the Humboldt River
    # ID == "34324"                   ~ 0.33, # Maggie Creek from its confluence with Soap Creek to its confluence with the Humboldt River
    # ID == "34348"                   ~ 0.33, # Maggie Creek from its confluence with Jack Creek to its confluence with Soap Creek
    
    Name == "Martin Creek"           ~  0.10,
    Name == "Marys River"           ~  0.10,
    Name == "Mill Creek"           ~  0.10,
    
    Name == "Muddy River"           ~ 0.3,
    
    # ID == "23950"                  ~ 0.1, # Muddy River from the river source to the Glendale Bridge
    # ID == "33940"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
    # ID == "33941"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
    # ID == "33942"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
    # ID == "33944"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
    # ID == "33995"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
    # ID == "34141"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
    # ID == "34529"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
    
    Name == "Negro Creek"           ~  0.10,
    Name == "North Antelope Creek"           ~  0.10,
    Name == "North Fork Humbodlt River"           ~  0.10,
    Name == "North Fork Little Humboldt River"           ~  0.10,
    Name == "North Fork Salmon Falls Creek"           ~  0.10,
    Name == "Owyhee River"           ~  0.10,
    Name == "Peavine Creek"           ~  0.10,
    Name == "Penrod Creek"           ~  0.10,
    Name == "Pine Creek"           ~  0.10,
    Name == "Pole Creek"           ~  0.10,
    Name == "Quinn River"           ~  0.10,
    Name == "Reese River"           ~ 0.10,
    UniqueID == "80045"             ~ 0.33, # Reese River north of State Route 722 (old U.S. Highway 50)
    UniqueID == "80208"             ~ 0.33, # Reese River north of State Route 722 (old U.S. Highway 50)
    UniqueID == "80244"             ~ 0.33, # Reese River north of State Route 722 (old U.S. Highway 50)
    UniqueID == "80319"             ~ 0.33, # Reese River north of State Route 722 (old U.S. Highway 50)
    UniqueID == "80361"             ~ 0.33, # Reese River north of State Route 722 (old U.S. Highway 50)
    UniqueID == "80432"             ~ 0.33, # Reese River north of State Route 722 (old U.S. Highway 50)
    Name == "Roberts Creek"           ~  0.10,
    Name == "Rock Creek"           ~ 0.33,
    UniqueID == "80097"         ~   0.1, # Rock Creek from its origin to Squaw Valley Ranch
    UniqueID == "80553"         ~   0.1, # Rock Creek from its origin to Squaw Valley Ranch
    Name == "Salmon Falls Creek"           ~  0.10,
    
    # ID == "34249"                 ~ 0.10, # Secret Creek from the national forest boundary to its confluence with the Humboldt River
    # ID == "34256"                 ~ 0.33, # Secret Creek from its origin to the national forest boundary
    
    Name == "Shoshone Creek"           ~  0.10,
    Name == "Silver Creek"           ~  0.10,
    Name == "Skull Creek"           ~  0.10,
    Name == "Smoke Creek"           ~  0.10,
    Name == "South Fork Canyon Creek"           ~  0.10,
    Name == "South Fork Humboldt River"           ~  0.10,
    Name == "South Fork Little Humboldt River"           ~  0.10,
    Name == "South Fork Owyhee River"           ~  0.10,
    Name == "South Fork Quinn River"           ~  0.10,
    Name == "South Fork Salmon Falls Creek"           ~  0.10,
    
    Name == "Star Creek"           ~  0.10,
    Name == "Steamboat Creek"           ~  0.33,
    Name == "Stoneberger Creek"           ~  0.10,
    Name == "Sunnyside Creek"           ~  0.10,
    Name == "Tabor Creek"           ~  0.10,
    Name == "Timber Creek"           ~  0.10,
    Name == "Trout Creek"           ~  0.10,
    Name == "Truckee River"           ~ 0.05, # check california criteria there's one at the border
    
    # ID == "34312"                  ~ 0.035, # Truckee River at the California-Nevada state line

    Name == "Virgin River"           ~  0.10,
    Name == "Walker River"           ~  0.40,
    UniqueID == "80373"              ~ 0.23, # Walker River from the exterior border of the Walker River Indian Reservation to Walker Lake
    Name == "West Fork Carson River"           ~  0.033,
    Name == "West Walker River"           ~  0.10,
    Name == "White River"           ~  0.10,
    Name == "Willow Creek"           ~  0.10,
    TRUE                            ~ 0     # Default value if none of the above conditions are met
  ))


# 5/8. Montana keep the order # check why this is not in Treated Rivers and Streams
Treated_MT_l3 <- st_read("Montana_wbl3_SpatialJoin.shp") %>%
  as.data.frame() %>% 
  select(Name, State, UniqueID, US_L3NAME) %>% 
  rename(LName = US_L3NAME) %>% 
  filter(!is.na(LName)) %>% 
  mutate(Intensity = case_when(
    LName == "Canadian Rockies"          ~ 25,
    LName == "Idaho Batholith"           ~  25,
    LName == "Nothern Rockies"           ~  25,
    LName == "Northwestern Glaciated Plains"           ~  110,
    LName == "Northwestern Great Plains"           ~  150,
    LName == "Wyoming Basin"           ~  150,
        TRUE                            ~ 0     # 3045
  ))

Treated_MT_l4 <- st_read("Montana_wbl4_SpatialJoin.shp") %>%
  as.data.frame() %>% 
  select(Name, State, UniqueID, US_L4NAME) %>% 
  rename(LName = US_L4NAME) %>% 
  filter(!is.na(LName)) %>% 
  mutate(Intensity = case_when(
    LName == "Absaroka-Gallatin Volcanic Mountains"          ~ 105,
    LName == "Sweetgrass Uplands"           ~  80,
    LName == "Milk River Pothole Upland"           ~  80,
    LName == "Rocky Mountain Front Foothill Potholes"           ~  80,
    LName == "Foothill Grassland"           ~  80,
    LName == "Non-calcareous Foothill Grassland"           ~  33,
    LName == "Shield-Smith Valleys"           ~  33,
    LName == "Limy Foothill Grassland"           ~  33,
    LName == "Pryor-Bighorn Foothills"           ~  33,
    LName == "Unglaciated Montana High Plains"           ~  33,
    TRUE                            ~ 0     # 416
  ))

# 5/8. Filter out unique waterbodies when they are in l4
Treated_MT_l3 %<>% filter(!UniqueID %in% Treated_MT_l4$UniqueID) # 2651

# 5/8. Intensity by name
Treated_MT <- Treated_Rivers_and_Streams_withID %>%
  filter(str_detect(State, "MT")) %>%  
  mutate(Intensity = case_when(
    Name == "Flint Creek"           ~  72,
    # ID == "24139"                 ~  55, # Yellowstone River (Bighorn River confluence to Powder River confluence)
    # ID == "24156"                 ~  55, # Yellowstone River (Bighorn River confluence to Powder River confluence)
    # ID == "24109"                 ~  95, # Yellowstone River (Powder River confluence to stateline)
    # ID == "24113"                 ~  95, # Yellowstone River (Powder River confluence to stateline)

      TRUE                            ~ 0     
  )) #2753

# 5/8. Filter out unique waterbodies when they are in Treated_MT
Treated_MT_l3 %<>% filter(!UniqueID %in% Treated_MT$UniqueID) # 325

# 5/8. rbind all MT

Treated_MT <- rbind(Treated_MT_l3, Treated_MT_l4) %>% 
              select(-LName) %>% 
              rbind(Treated_MT) # 3494

# 6/8. Oklahoma
# scenic rivers ~ 0.037
Treated_OK <- Treated_Rivers_and_Streams_withID %>%
  filter(str_detect(State, "OK")) %>%  
  mutate(Intensity = 0.037) # 27

# 7/8. Wisconsin statewide criteria - YOU NEED TO UPDATE
Treated_WI <- Treated_Rivers_and_Streams_withID %>%
  filter(str_detect(State, "WI")) %>%  
  mutate(Intensity = case_when(
    # ID == "27773"           ~  100, # Apple River from the outlet of the Apple River Flowage to the St. Croix River, excluding Black Brook Flowage
    # ID == "27643"           ~  100, #  Bad River from confluence with the Marengo River within the Bad River Indian Reservation downstream to Lake Superior.
    # ID == "27805"           ~  100,
    # ID == "28954"           ~  100,
    # ID == "35381"           ~  100,
    # ID == "36076"           ~  100,
    # ID == "36117"           ~  100,
    # ID == "36138"           ~  100,
    # ID == "36145"           ~  100,
    # ID == "36212"           ~  100,
    # ID == "36231"           ~  100, # It is noted as white river, but on the map bad river; update it
    # ID == "36253"           ~  100,
    # ID == "36284"           ~  100,
    # ID == "36073"           ~  100,
    # ID == "29064"           ~  100,
    UniqueID == "107787" ~  100, # Baraboo River from highway 58 to the Wisconsin River.
    # ID == "28482" ~ 100, # Bark River from confluence with Scuppernong River to the Rock River
    # ID == "27726"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
    # ID == "28362"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
    # ID == "28482"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
    # ID == "28828"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
    # ID == "35211"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
    # ID == "36107"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
    # ID == "36203"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
    Name == "Buffalo River" ~ 100, # Buffalo River from confluence with Harvey Creek to Mississippi River.
    Name == "Chippewa River" ~ 100, # Chippewa River from Lake Chippewa in Sawyer County to Mississippi River
    Name == "Crawfish River" ~ 100, # Crawfish River from confluence with Beaver Dam River to Rock River
    # ID == "35990" ~ 100, # East Branch Pecatonica River from confluence with Apple Branch Creek 
    Name == "Eau Claire River" ~ 100, # Eau Claire River from confluence with Bridge Creek to Chippewa River, excluding Altoona Lake
    # ID == "35240" ~ 100, #  Embarrass River from confluence with Pigeon River near Clintonville to Wolf River
    Name == "Flambeau River" ~ 100, # Flambeau River from outlet of Turtle−Flambeau Flowage in Iron County to Chippewa River
    Name == "Fox River" ~ 100, # Fox River from outlet of Lake Puckaway near Princeton to Green Bay
    # ID == "27657" ~ 100, # Grant River from confluence with Rattlesnake Creek near Beetown to Mississippi River
    # ID == "35230" ~ 100, # Grant River from confluence with Rattlesnake Creek near Beetown to Mississippi River
    Name == "Jump River" ~ 100, # Jump River from confluence with the North Fork and the South Fork of the Jump rivers in Price County to Holcombe Flow
    # ID == "27995" ~ 100, #  Kickapoo River from confluence with Weister Creek near La Farge to Wisconsin River.
    # ID == "28601" ~ 100, #  Kickapoo River from confluence with Weister Creek near La Farge to Wisconsin River.
    # ID == "35476" ~ 100, #  Kickapoo River from confluence with Weister Creek near La Farge to Wisconsin River.
    # ID == "35980" ~ 100, #  Kickapoo River from confluence with Weister Creek near La Farge to Wisconsin River.
    # ID == "28451" ~ 100, # La Crosse River from confluence with Fish Creek near Bangor to Mississippi River
    Name == "Lemonweir River" ~ 100, # Lemonweir River from outlet of New Lisbon Lake in New Lisbon to Wisconsin River, excluding Decorah Lake
    # ID == "28603" ~ 100, #  Little Wolf River from confluence with South Branch Little Wolf River near Royalton to Wolf River
    # ID == "35469" ~ 100, #  Little Wolf River from confluence with South Branch Little Wolf River near Royalton to Wolf River
    Name == "Manitowoc River" ~ 100, # Manitowoc River from confluence of North Branch and South Branch Manitowoc rivers to the opening at the end of the piers at Lake Michigan
    Name == "Menominee River" ~ 100, # Menominee River from confluence with Brule River to the opening at the end of the piers at Green Bay
    # ID == "27249"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "27323"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "27453"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "27542"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "27582"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "28300"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "28886"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "35273"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "35280"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "35302"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "35413"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "35425"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "35426"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "35521"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    # ID == "36065"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
    Name == "Mississippi River" ~ 100, # Mississippi River main channels and side channels.
    # ID == "27543" ~ 100, # Namekagon River from outlet of Trego Lake near Trego to St. Croix River
    # ID == "28543" ~ 100, # Namekagon River from outlet of Trego Lake near Trego to St. Croix River
    # ID == "29149" ~ 100, # Namekagon River from outlet of Trego Lake near Trego to St. Croix River
    Name == "Oconto River" ~ 100, #  Oconto River from confluence with Peshtigo Brook to the opening at the end of the piers at Green Bay
    Name == "Pecatonica River" ~ 100, # Pecatonica River from confluence with Vinegar Branch near Darlington to state line
    Name == "Pelican River" ~ 100, # Pelican River from confluence with Slaughterhouse Creek near Rhinelander to Wisconsin River
    # ID =="28674" ~ 100, # Peshtigo River from confluence with Brandywine Creek downstream to Green Bay
    # ID == "28548" ~ 100, # Pine River from confluence with Popple River in Florence County to Menominee River, excluding Pine River Flowage
    Name == "Red Cedar River" ~ 100, # Red Cedar River from confluence with Brill River to Chippewa River
    Name == "Rock River" ~ 100, # Rock River from outlet of Sinissippi Lake downstream to the state line
    
    # (Name == "Saint Croix River" & ID != "27549" & ID!="29187") ~ 100, # St.  Croix River from confluence with Namekagon River downstream to Mississippi River
    
    Name =="Sheboygan River" ~ 100, # Sheboygan River from outlet of Sheboygan Marsh to the opening at the end of the piers at Lake Michigan
    # ID =="28385" ~ 100, # South Fork of Flambeau River from state highway 13 near Fifield to Flambeau River
    Name =="Sugar River" ~ 100, # Sugar River from outlet of Albany Lake to state line
    Name =="Tomahawk River" ~ 100, # Tomahawk River from outlet of Willow Reservoir to Lake Nokomis.
    Name =="Trempealeau River" ~ 100, # Trempealeau River from confluence with Pigeon Creek near Whitehall to Mississippi River
    Name =="White River" ~ 100, # White River from outlet of White River Flowage in Ashland County to Bad River       
    Name =="Wisconsin River" ~ 100, # Wisconsin River from the Rhinelander Dam to Mississippi River
    # Name =="Wolf River" & ID!="35442" ~ 100, # Wolf River from confluence with Hunting Creek in Langlade County to Lake Poygan
    Name =="Yahara River" ~ 100, #  Yahara River from outlet of Lake Kegonsa to Rock River
    TRUE                            ~ 0     
  ))


# 8/8. Utah
Treated_UT <- Treated_Rivers_and_Streams_withID %>%
  filter(str_detect(State, "UT")) %>%  
  mutate(Intensity = 0.035) # 1018

# ID == "26986" ~ 0.035, # Fishlake National Forest; Generate a Unique ID for this creek
# ID =="26936" ~ 0.035, # Red Butte Creek
# ID =="26979" ~ 0.035, # Red Butte Creek
# ID =="27128" ~ 0.035, # Kays Creek

# rbind everything
Treated_Rivers_and_Streams_withID <- rbind(Treated_AZ, Treated_CA, Treated_FL, Treated_NV, Treated_MT, Treated_OK, Treated_WI, Treated_UT) 
#############################################################################

# Update the regression dataset for California
df_mn %<>% 
  mutate(
    # California
    Treated = ifelse(ID == "29645", 1, Treated),
    UniqueID = ifelse(ID == "29645", "906001", UniqueID),
    Name = ifelse(ID == "29645", "Independence Creek", Name),
    
    Intensity = case_when(
      # California
      ID == "29645" ~ 0.03, # Independence Creek UniqueID needed 
      ID == "29468" ~ 0.03, # Truckee River at Lake Tahoe Outlet (13) 
      ID == "1533"  ~ 0.03, # Truckee River at Lake Tahoe Outlet  (13)
      ID == "30734" ~ 0.10, # Bear Creek at Mouth (11)
      ID == "1850"  ~ 0.10, # Truckee River above Bear Creek (12)   
      ID == "30941" ~ 0.10, # Truckee River above Bear Creek (12)   
      ID == "2285"  ~ 0.13, # Truckee River below Bear Creek (10)
      ID == "31158" ~ 0.13, # Squaw Creek at Mouth (8)
      ID == "29371" ~ 0.29, # Truckee River below Donner Creek (5)
      ID == "29362" ~ 0.29, # Truckee River below Martis Creek (4)
      ID == "29403" ~ 0.29, # Truckee River below Martis Creek (4)
      ID == "30836" ~ 0.29, # Truckee River below Martis Creek (4)
      ID == "30167" ~ 0.30, # Truckee River at stateline (1)   
      ID == "1640"  ~ 0.02, # West Fork Carson River at Woodfords (1)  
      ID == "29565" ~ 0.02, # West Fork Carson River at Woodfords  
      ID == "30702" ~ 0.02, # West Fork Carson River at Woodfords  
      ID == "31903" ~ 0.02, # West Fork Carson River at Woodfords  
      
      # Nevada
      ID == "23996"           ~ 0.10, # East Fork of the Carson River from the California-Nevada state line to the Riverview Mobile Home Park at U.S. Highway 395 south of Gardnerville
      ID == "33985"           ~ 0.10, # East Fork of the Carson River from the California-Nevada state line to the Riverview Mobile Home Park at U.S. Highway 395 south of Gardnerville
      ID == "23955"           ~ 0.10, # East Fork of the Carson River from the Riverview Mobile Home Park at U.S. Highway 395 to Muller Lane
      ID == "23956"           ~ 0.10, # East Fork of the Carson River from the Riverview Mobile Home Park at U.S. Highway 395 to Muller Lane
      ID == "23984"           ~ 0.10, # East Fork of the Carson River from the Riverview Mobile Home Park at U.S. Highway 395 to Muller Lane
      ID == "33943"           ~ 0.10, # East Fork of the Carson River from the Riverview Mobile Home Park at U.S. Highway 395 to Muller Lane
      ID == "23786"           ~ 0.10, # East Fork Carson River from Muller Lane to the West Fork
      ID == "23864"           ~ 0.10, # East Fork Carson River from Muller Lane to the West Fork
      ID == "23997"           ~ 0.10, # East Fork Carson River from Muller Lane to the West Fork
      ID == "33983"           ~ 0.10, # East Fork Carson River from Muller Lane to the West Fork
      ID == "33949"           ~ 0.10, # East Fork Carson River from Muller Lane to the West Fork
      ID == "34298"           ~ 0.065, # East Fork Carson River from Muller Lane to the West Fork
      ID == "34242"           ~ 0.10, # East Fork of the Carson River from Muller Lane to the West Fork
      ID == "33939"           ~ 0.10, # the main stem of the Carson River from the confluence of the East and West Forks to Genoa Lane
      ID == "33981"           ~ 0.10, # West Fork of the Carson River from the California-Nevada state line to the East Fork
      ID == "34206"           ~ 0.10, # West Fork of the Carson River from the California-Nevada state line to the East Fork
      ID == "34199"               ~ 0.33, # Humboldt River from Woolsey to Rodgers Dam
      ID == "34039"                   ~ 0.33, # Maggie Creek from its confluence with Soap Creek to its confluence with the Humboldt River
      ID == "34324"                   ~ 0.33, # Maggie Creek from its confluence with Soap Creek to its confluence with the Humboldt River
      ID == "34348"                   ~ 0.33, # Maggie Creek from its confluence with Jack Creek to its confluence with Soap Creek
      ID == "23950"                  ~ 0.1, # Muddy River from the river source to the Glendale Bridge
      ID == "33940"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
      ID == "33941"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
      ID == "33942"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
      ID == "33944"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
      ID == "33995"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
      ID == "34141"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
      ID == "34529"                 ~ 0.1, # Muddy River from the river source to the Glendale Bridge
      ID == "34249"                 ~ 0.10, # Secret Creek from the national forest boundary to its confluence with the Humboldt River
      ID == "34256"                 ~ 0.33, # Secret Creek from its origin to the national forest boundary
      ID == "34312"                  ~ 0.035, # Truckee River at the California-Nevada state line
      
      # Montana
      ID == "24139"                 ~  55, # Yellowstone River (Bighorn River confluence to Powder River confluence)
      ID == "24156"                 ~  55, # Yellowstone River (Bighorn River confluence to Powder River confluence)
      ID == "24109"                 ~  95, # Yellowstone River (Powder River confluence to stateline)
      ID == "24113"                 ~  95, # Yellowstone River (Powder River confluence to stateline)
      
      # Wisconsin
      (Name == "Saint Croix River" & ID != "27549" & ID!="29187")  ~ 100, # St.  Croix River from confluence with Namekagon River downstream to Mississippi River
      (Name =="Wolf River" & ID!="35442") ~ 100, # Wolf River from confluence with Hunting Creek in Langlade County to Lake Poygan
      
      ID == "27773"           ~  100, # Apple River from the outlet of the Apple River Flowage to the St. Croix River, excluding Black Brook Flowage
      ID == "27643"           ~  100, #  Bad River from confluence with the Marengo River within the Bad River Indian Reservation downstream to Lake Superior.
      ID == "27805"           ~  100,
      ID == "28954"           ~  100,
      ID == "35381"           ~  100,
      ID == "36076"           ~  100,
      ID == "36117"           ~  100,
      ID == "36138"           ~  100,
      ID == "36145"           ~  100,
      ID == "36212"           ~  100,
      ID == "36231"           ~  100, # It is noted as white river, but on the map bad river; update it
      ID == "36253"           ~  100,
      ID == "36284"           ~  100,
      ID == "36073"           ~  100,
      ID == "29064"           ~  100,
      ID == "28482"           ~  100, # Bark River from confluence with Scuppernong River to the Rock River
      ID == "27726"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
      ID == "28362"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
      ID == "28482"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
      ID == "28828"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
      ID == "35211"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
      ID == "36107"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
      ID == "36203"           ~  100, # Black River from confluence with Cunningham Creek near Neillsville to Mississippi River
      ID == "35990" ~ 100, # East Branch Pecatonica River from confluence with Apple Branch Creek 
      ID == "35240" ~ 100, #  Embarrass River from confluence with Pigeon River near Clintonville to Wolf River
      ID == "27657" ~ 100, # Grant River from confluence with Rattlesnake Creek near Beetown to Mississippi River
      ID == "35230" ~ 100, # Grant River from confluence with Rattlesnake Creek near Beetown to Mississippi River
      ID == "27995" ~ 100, #  Kickapoo River from confluence with Weister Creek near La Farge to Wisconsin River.
      ID == "28601" ~ 100, #  Kickapoo River from confluence with Weister Creek near La Farge to Wisconsin River.
      ID == "35476" ~ 100, #  Kickapoo River from confluence with Weister Creek near La Farge to Wisconsin River.
      ID == "35980" ~ 100, #  Kickapoo River from confluence with Weister Creek near La Farge to Wisconsin River.
      ID == "28451" ~ 100, # La Crosse River from confluence with Fish Creek near Bangor to Mississippi River
      ID == "28603" ~ 100, #  Little Wolf River from confluence with South Branch Little Wolf River near Royalton to Wolf River
      ID == "35469" ~ 100, #  Little Wolf River from confluence with South Branch Little Wolf River near Royalton to Wolf River
      ID == "27249"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "27323"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "27453"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "27542"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "27582"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "28300"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "28886"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "35273"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "35280"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "35302"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "35413"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "35425"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "35426"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "35521"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "36065"~ 100, # Milwaukee River from confluence with Cedar Creek downstream to the openings of the breakwaters at Lake Michigan
      ID == "27543" ~ 100, # Namekagon River from outlet of Trego Lake near Trego to St. Croix River
      ID == "28543" ~ 100, # Namekagon River from outlet of Trego Lake near Trego to St. Croix River
      ID == "29149" ~ 100, # Namekagon River from outlet of Trego Lake near Trego to St. Croix River
      ID =="28674" ~ 100, # Peshtigo River from confluence with Brandywine Creek downstream to Green Bay
      ID == "28548" ~ 100, # Pine River from confluence with Popple River in Florence County to Menominee River, excluding Pine River Flowage
      ID =="28385" ~ 100, # South Fork of Flambeau River from state highway 13 near Fifield to Flambeau River

      # Utah
      ID == "26986" ~ 0.035, # Fishlake National Forest; Generate a Unique ID for this creek
      ID =="26936" ~ 0.035, # Red Butte Creek
      ID =="26979" ~ 0.035, # Red Butte Creek
      ID =="27128" ~ 0.035, # Kays Creek
      
      TRUE          ~ Intensity  # Default to keep existing Intensity values unchanged
    ),
  )

# Wisconsin
df_mn %<>% mutate(
  Intensity = case_when(
    # Add 75 for all the other rivers and streams
   (StateCd == "55" & Intensity != 100) ~ 75, 
    TRUE ~ Intensity  # Default to keep existing Intensity values unchanged
  ),
  Treated = ifelse(ID == "26986", 1, Treated),
  UniqueID = ifelse(ID == "26986", "949001", UniqueID),
  Name = ifelse(ID == "26986", "Quitchupah Creek", Name)
)

df_mn %<>% mutate(Intensity = ifelse(Treated == 0, NA, Intensity))

