library(pacman)
p_load(readxl, dataRetrieval, dplyr, lubridate, sf, tidyr, magrittr, zoo)

# Import State Finance data
finance <- readxl::read_excel("C:/Users/Sol/OneDrive - Michigan State University/Saesol and Molly Shared Work/Water Quality/StateFinanceData/SewerageExpenditure.xlsx", sheet = "Data") # check the sheet name before importing

# Merge with States to obtain StateCodes
finance$State <- substr(finance$Name, 1, 2)
finance <- finance %>% left_join(select(States, State, Code), by = "State") %>% rename(StateCd=Code)


df_final_mn <- df_final_mn %>% 
  left_join(select(finance, Year, StateCd, SW_propor), by = c("Year", "StateCd"))

df_final <- df_final %>% 
  left_join(select(finance, Year, StateCd, SW_propor), by = c("Year", "StateCd"))


# Check the NAs 
for (col in names(df_final_mn)) {
  var_na <- sum(is.na(df_final_mn[[col]]))
  cat("Column", col, "has", var_na, "missing values.\n")
}  
  