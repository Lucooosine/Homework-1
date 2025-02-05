#' ---
#' title: "Datavisualization_HW1"
#' output: pdf
#' date: "2025-02-05"
#' ---
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crime_data <- read.csv("/Users/lusinegevorgyan/Desktop/crime_data.csv")
head(crime_data, 5)

#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missing_counts <- colSums(is.na(crime_data))
threshold <- 0.5 * nrow(crime_data)
columns_to_keep <- names(missing_counts[missing_counts <= threshold])
crime_data_cleaned <- crime_data[, columns_to_keep]
crime_data_cleaned 

#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(lubridate)
library(dplyr)

crime_data_cleaned$DATE_OCC <- gsub(" UTC", "", crime_data_cleaned$DATE.OCC)
crime_data_cleaned$DATE_OCC <- mdy_hms(crime_data_cleaned$DATE_OCC, tz = "UTC")
head(crime_data_cleaned$DATE_OCC)

crime_data_cleaned <- crime_data_cleaned %>%
  mutate(
    Year = year(DATE_OCC),
    Month = month(DATE_OCC),
    Day = day(DATE_OCC),
  )



#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(crime_data_cleaned$DATE_OCC)

#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crime_data_cleaned$Time_copy <- crime_data_cleaned$TIME.OCC
crime_data_cleaned <- crime_data_cleaned %>%
  mutate(
    Hour = as.integer(substr(Time_copy, 1, 2))
  )
head(crime_data_cleaned$Hour)





#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crime_2023 <- subset(crime_data_cleaned, Year == 2023 & Crm.Cd.Desc == "BURGLARY")
crime_2023
colnames(crime_2023)

#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crime_summary <- crime_data_cleaned %>%
  group_by(AREA.NAME) %>%
  summarise(
    Total_Crimes = n(),
    Avg_Victim_Age = mean(Vict.Age, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Crimes))
print(crime_summary)


#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
monthly_crimes <- crime_data_cleaned %>%
  group_by(Month) %>%
  summarise(
    Total_Crimes = n()
  ) %>%
  arrange(Month)  

print(monthly_crimes)


#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
weapon_used_count <- crime_data %>%
  filter(!is.na(Weapon.Used.Cd)) %>%
  summarise(Weapon_Used_Count = n())

print(weapon_used_count)


#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crime_by_premis <- crime_data_cleaned %>%
  group_by(Premis.Desc) %>%
  summarise(Crime_Count = n()) %>%
  arrange(desc(Crime_Count))  

print(crime_by_premis)


#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crime_data_cleaned<- crime_data_cleaned %>%
  mutate(Severity_Score = case_when(
    !is.na(Weapon.Desc) ~ 5,                          
    Crm.Cd.Desc == "BURGLARY" ~ 3,                    
    TRUE ~ 1                                           
  ))

severity_by_area <- crime_data_cleaned %>%
  group_by(AREA.NAME) %>%
  summarise(Total_Severity_Score = sum(Severity_Score, na.rm = TRUE)) %>%
  arrange(desc(Total_Severity_Score))  

print(severity_by_area)



