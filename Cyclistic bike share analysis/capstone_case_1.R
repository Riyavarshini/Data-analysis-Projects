#------------------------CASESTUDY1   CYCLISTIC BIKE SHARE ---------------

# ------ PREPARE ------
#======> IMPORTING DATASETS <=======

# Load libraries
library(tidyverse) # package for data manipulation, exploration and visualization
library(lubridate) # parse and manipulate dates

# Importing original .csv file from July 2022 to June 2023.
july_22 <- read_csv("202207-divvy-tripdata.csv") 
aug_22 <- read_csv("202208-divvy-tripdata.csv") 
sep_22 <- read_csv("202209-divvy-tripdata.csv") 
oct_22 <- read_csv("202210-divvy-tripdata.csv") 
nov_22 <- read_csv("202211-divvy-tripdata.csv") 
dec_22 <- read_csv("202212-divvy-tripdata.csv") 
jan_23 <- read_csv("202301-divvy-tripdata.csv") 
feb_23 <- read_csv("202302-divvy-tripdata.csv") 
mar_23 <- read_csv("202303-divvy-tripdata.csv") 
apr_23 <- read_csv("202304-divvy-tripdata.csv") 
may_23 <- read_csv("202305-divvy-tripdata.csv") 
june_23 <- read_csv("202306-divvy-tripdata.csv") 

# Check column names of each dataset.
colnames(july_22)
colnames(aug_22)
colnames(sep_22)
colnames(oct_22)
colnames(nov_22)
colnames(dec_22)
colnames(jan_23)
colnames(feb_23)
colnames(mar_23)
colnames(apr_23)
colnames(may_23)
colnames(june_23)

# Inspecting internal structure of all dataset to look for discrepancies.
str(july_22)
str(aug_22)
str(sep_22)
str(oct_22)
str(nov_22)
str(dec_22)
str(jan_23)
str(feb_23)
str(mar_23)
str(apr_23)
str(may_23)
str(june_23)

# Merge all data into a single dataframe
bike_trip <- bind_rows(july_22,aug_22,sep_22,oct_22,nov_22,dec_22,jan_23,feb_23,mar_23,apr_23,may_23,june_23)

# remove individual month dataset to clear up space in the environment 
remove(july_22,aug_22,sep_22,oct_22,nov_22,dec_22,jan_23,feb_23,mar_23,apr_23,may_23,june_23)

# ---------- PROCESS ----------------
# =========> MANIPULATING DATASET <=========

# copy bike_trip dataframe into new dataframe.
bike_trip_v1 <- bike_trip

# Adding new columns for date, month, year, days_of_week, hour by extracting value from started_at(YYYY-MM-DD HH:MM:SS UTC) column.
bike_trip_v1$date <- as.Date(bike_trip_v1$started_at)
bike_trip_v1$month <- format(as.Date(bike_trip_v1$date),"%B")
bike_trip_v1$year <- format(as.Date(bike_trip_v1$date),"%Y")
bike_trip_v1$days_of_week <- format(as.Date(bike_trip_v1$date),"%A")
bike_trip_v1$hour <- hour(bike_trip_v1$started_at)

# calculate ride length = ended_at - started_at
bike_trip_v1$ride_length <- difftime(bike_trip_v1$ended_at, bike_trip_v1$started_at, units = "mins")

# creating new column for seasons: Spring,summer, autumn, winter.
bike_trip_v1 <- bike_trip_v1 %>% 
  mutate(season = case_when(month == "March" ~ "Spring",
                            month == "April" ~ "Spring",
                            month == "May" ~ "Spring",
                            month == "June" ~ "Summer",
                            month == "July" ~ "Summer",
                            month == "August" ~ "Summer",
                            month == "September" ~ "Autumn",
                            month == "October" ~ "Autumn",
                            month == "November" ~ "Autumn",
                            month == "December" ~ "Winter",
                            month == "January" ~ "Winter",
                            month == "February" ~ "Winter"))

# column for parts_of_day : Morning, Afternoon, Evening, Night.
bike_trip_v1 <- bike_trip_v1 %>% 
  mutate(parts_of_day = case_when(hour == 5 ~ "Morning",
                                  hour == 6 ~ "Morning",
                                  hour == 7 ~ "Morning",
                                  hour == 8 ~ "Morning",
                                  hour == 9 ~ "Morning",
                                  hour == 10 ~ "Morning",
                                  hour == 11 ~ "Morning",
                                  hour == 12 ~ "Afternoon",
                                  hour == 13 ~ "Afternoon",
                                  hour == 14 ~ "Afternoon",
                                  hour == 15 ~ "Afternoon",
                                  hour == 16 ~ "Afternoon",
                                  hour == 17 ~ "Evening",
                                  hour == 18 ~ "Evening",
                                  hour == 19 ~ "Evening",
                                  hour == 20 ~ "Evening",
                                  hour == 21 ~ "Night",
                                  hour == 22 ~ "Night",
                                  hour == 23 ~ "Night",
                                  hour == 0 ~ "Night",
                                  hour == 1 ~ "Night",
                                  hour == 2 ~ "Night",
                                  hour == 3 ~ "Night",
                                  hour == 4 ~ "Night"))

# Ordering months and days_of_week properly.
bike_trip_v1$days_of_week <- ordered(bike_trip_v1$days_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
bike_trip_v1$month <- ordered(bike_trip_v1$month, levels = c("July","August","September","October","November","December","January","February","March","April","May","June"))

# =======> CLEANING DATASET <========

# Deleting columns not involved in analysis
bike_trip_v1 <- select(bike_trip_v1,-c(ride_id,start_station_id,end_station_id,start_lat,start_lng,end_lat,end_lng))
bike_trip_v1 <- na.omit(bike_trip_v1) # Removing rows with NA values.
bike_trip_v1 <- distinct(bike_trip_v1) # Removing duplicate rows.
bike_trip_v1 <- bike_trip_v1 %>% filter(ride_length > 0) # Removing ridelength value less than or equal to zero.

# View dataset
View(bike_trip_v1)

# Export dataset for visualization in Tableau
write_csv(bike_trip_v1,"cyclistic_bike_share.csv")

# -------------- ANALYZE ----------------

# --- TOTAL RIDE ---
# Total rides included in dataset for analysis
nrow(bike_trip_v1)

# User type
bike_trip_v1 %>% 
  group_by(member_casual) %>% 
  summarise(total_ride = n())

# average casual and member users 
bike_trip_v1 %>% 
  group_by(day,member_casual) %>% 
  summarise(total_ride = n()) %>%
  group_by(member_casual) %>% 
  summarise(mean(total_ride))

# bike type
bike_trip_v1 %>% 
  group_by(rideable_type) %>% 
  summarise(total_Ride = n())

# total rides on bike types by users
bike_trip_v1 %>% 
  group_by(rideable_type,member_casual) %>% 
  summarise(total_ride = n())

# total rides per month
bike_trip_v1 %>% 
  group_by(month) %>% 
  summarise(total_ride = n()) 

# total rides per month by users
bike_trip_v1 %>% 
  group_by(month,member_casual) %>% 
  summarise(total_ride = n()) %>% 
  print(n=24)

# total rides per days_of_week
bike_trip_v1 %>% 
  group_by(days_of_week) %>% 
  summarise(total_ride = n())

# total rides per days_of_week by users
bike_trip_v1 %>% 
  group_by(days_of_week,member_casual) %>% 
  summarise(total_ride = n()) 

# total rides per hour
bike_trip_v1 %>% 
  group_by(hour) %>% 
  summarise(total_ride = n()) %>% 
  print(n = 24)

# total ride per hour by users
bike_trip_v1 %>% 
  group_by(hour,member_casual) %>% 
  summarise(total_ride = n()) %>% 
  print(n = 48)

# total ride per season
bike_trip_v1 %>% 
  group_by(season) %>% 
  summarise(total_ride = n())

# total ride per season by users
bike_trip_v1 %>% 
  group_by(season,member_casual) %>% 
  summarise(total_ride = n())

# total ride for each parts of day
bike_trip_v1 %>% 
  group_by(parts_of_day) %>% 
  summarise(total_ride = n())

# total ride for each parts of day by user
bike_trip_v1 %>% 
  group_by(parts_of_day,member_casual) %>% 
  summarise(total_ride = n())


# ---AVERAGE RIDE LENGTH---
# average ride length for each users
bike_trip_v1 %>% group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length))

# average ride length for each bike type
bike_trip_v1 %>% group_by(rideable_type) %>% 
  summarise(average_ride_length = mean(ride_length))

# average ride length for each bike type per users
bike_trip_v1 %>% group_by(rideable_type,member_casual) %>% 
  summarise(average_ride_length = mean(ride_length))

# average ride length per month
bike_trip_v1 %>% group_by(month) %>% 
  summarise(average_ride_length = mean(ride_length))

# average ride length per month for each user   
bike_trip_v1 %>% group_by(month,member_casual) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  print(n=24)

# average ride length per days of week 
bike_trip_v1 %>% 
  group_by(days_of_week) %>% 
  summarise(average_ride_length = mean(ride_length))

# average ride length per days of week by users
bike_trip_v1 %>% 
  group_by(days_of_week,member_casual) %>% 
  summarise(average_ride_length = mean(ride_length))

# average ride length per hour
bike_trip_v1 %>% 
  group_by(hour) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  print(n = 24)

# average ride length per hour by users
bike_trip_v1 %>% 
  group_by(hour,member_casual) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  print(n = 48)

# average ride length per season
bike_trip_v1 %>% 
  group_by(season) %>% 
  summarise(average_ride_length = mean(ride_length))

# average ride length per season by users
bike_trip_v1 %>% 
  group_by(season,member_casual) %>% 
  summarise(average_ride_length = mean(ride_length))

# average ride length each parts of day
bike_trip_v1 %>% 
  group_by(parts_of_day) %>% 
  summarise(average_ride_length = mean(ride_length))

# average ride length each parts of day by users
bike_trip_v1 %>% 
  group_by(parts_of_day,member_casual) %>% 
  summarise(average_ride_length = mean(ride_length))

# --- TOP 20 STATION WITH HIGHEST TOTAL RIDES -----
# Top 20 stations with highest users.
bike_trip_v1 %>% 
  group_by(start_station_name,member_casual) %>% 
  summarise(total_users = n()) %>% 
  arrange(desc(total_users)) %>% 
  print(n=20)

# Top 20 station with highest casual users
bike_trip_v1 %>% 
  group_by(start_station_name) %>% 
  filter(member_casual == "casual") %>% 
  summarise(total_users = n()) %>% 
  arrange(desc(total_users)) %>% 
  print(n=20)


# Top 20 station with casual users greater than member users
t1 <- bike_trip_v1 %>% 
  group_by(start_station_name) %>% 
  filter(member_casual == "casual") %>% 
  summarise(casual = n())

t2 <- bike_trip_v1 %>% 
  group_by(start_station_name) %>% 
  filter(member_casual == "member") %>% 
  summarise(member = n())

greater_casual_users <- merge(x= t1,y=t2, by = "start_station_name")

greater_casual_users <- greater_casual_users %>% 
  filter(casual > member)

greater_casual_users <- greater_casual_users %>% 
  arrange(desc(casual))

head(greater_casual_users,20)


