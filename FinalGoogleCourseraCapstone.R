---
title: "Final Google Data Analytics Coursera Capstone Code"
author: "Rad Reyes"
date: "02/10/2023"
output: html_document
---
  
# First, install the packages needed!
# Also, all visualization charts will be at the bottom
install.packages("tidyverse")
library("tidyverse")
install.packages("here")
library("here")
install.packages("skimr")
library("skimr")
install.packages("janitor")
library("janitor")
install.packages("dplyr")
library("dplyr")
install.packages("lubridate")
library("lubridate")

# Convert the csv's to easier to understand names
library(readr)
bike_Jan22 <- read_csv("Desktop/Coursera Capstone/202201-divvy-tripdata.csv")
bike_Feb22 <- read_csv("Desktop/Coursera Capstone/202202-divvy-tripdata.csv")
bike_Mar22 <- read_csv("Desktop/Coursera Capstone/202203-divvy-tripdata.csv")
bike_Apr22 <- read_csv("Desktop/Coursera Capstone/202204-divvy-tripdata.csv")
bike_May22 <- read_csv("Desktop/Coursera Capstone/202205-divvy-tripdata.csv")
bike_Jun22 <- read_csv("Desktop/Coursera Capstone/202206-divvy-tripdata.csv")
bike_Jul22 <- read_csv("Desktop/Coursera Capstone/202207-divvy-tripdata.csv")
bike_Aug22 <- read_csv("Desktop/Coursera Capstone/202208-divvy-tripdata.csv")
bike_Sep22 <- read_csv("Desktop/Coursera Capstone/202209-divvy-publictripdata.csv")
bike_Oct22 <- read_csv("Desktop/Coursera Capstone/202210-divvy-tripdata.csv")
bike_Nov22 <- read_csv("Desktop/Coursera Capstone/202211-divvy-tripdata.csv")
bike_Dec22 <- read_csv("Desktop/Coursera Capstone/202212-divvy-tripdata.csv")

# Check data for consistent column names
colnames(bike_Jan22)
compare_df_cols(bike_Jan22, bike_Feb22, bike_Mar22, bike_Apr22, bike_May22, bike_Jun22, bike_Jul22, bike_Aug22, bike_Sep22, bike_Oct22, bike_Nov22, bike_Dec22, return = "mismatch")

# Merge all months into one year for analysis
bikedata2022 <- rbind(bike_Jan22, bike_Feb22, bike_Mar22, bike_Apr22, bike_May22, bike_Jun22, bike_Jul22, bike_Aug22, bike_Sep22, bike_Oct22, bike_Nov22, bike_Dec22)

# Prior to analysis, remember to clean and check the data
bikedata2022 <- bikedata2022 %>% 
  rename("ride_type" = "rideable_type", "member_status" = "member_casual")
clean_names(bikedata2022)

# View the data
View(bikedata2022)

glimpse(bikedata2022)
head(bikedata2022)
str(bikedata2022)
skim_without_charts(bikedata2022)

# Remove empty areas
bikedata2022 <- na.omit(bikedata2022)
skim_without_charts(bikedata2022)

summary(bikedata2022)

# Change name for calculations/analysis
bikecalc <- bikedata2022

# Ride_length column to compare
bikecalc$ride_length <- difftime(bikecalc$ended_at, bikecalc$started_at, units = "mins")

# Review ride_length
min(bikecalc$ride_length)
bikecalc <- bikecalc[bikecalc$ride_length >= 0, ]

# Day of week column
bikecalc$date <- as.Date(bikecalc$started_at)
bikecalc$day_of_week <- wday(bikecalc$started_at)
bikecalc$day_of_week <- format(as.Date(bikecalc$date), "%A")

# Month and Day columns
bikecalc$month <- format(as.Date(bikecalc$date), "%m")
bikecalc$day <- format(as.Date(bikecalc$date), "%d")

# Remove longitude and latitude columns (will not be used in analysis since unable to connect to actual station))
bikecalc = subset(bikecalc, select = -c(start_lat, start_lng, end_lat, end_lng))

# Check for any missing values
map(bikecalc, ~sum(is.na(.)))

# How many different columns? Indicates what data to look at
ncol(bikecalc)
# How many different rows? Indicates how many unique riders used a Cyclistic bike
nrow(bikecalc)

# Member Analysis
bikecalc %>%
  group_by(member_status) %>%
  count(member_status)
bikecalc %>%
  group_by(member_status, ride_type) %>%
  count(ride_type)
# As tibble
bikecalc %>%
  count(member_status)
# As table
table(bikecalc["member_status"])
# Members: 2611141 Casual: 1758150

# Day of the week Analysis
bikecalc %>%
  group_by(member_status) %>%
  count(day_of_week)
bikecalc %>%
  count(day_of_week)
names(sort(-table(bikecalc$day_of_week))) [1]

# Day of the month Analysis
bikecalc %>%
  group_by(member_status) %>%
  count(day)
# As tibble
bikecalc %>% 
  count(day)
# As table
table(bikecalc["day"])

# Month Analysis
bikecalc %>%
  group_by (member_status) %>%
  count(month)
bikecalc %>%
  count(month)
names(sort(-table(bikecalc$month))) [1]

# Station Analysis
bikecalc %>%
  group_by(member_status, ride_type) %>%
  count(start_station_name)
bikecalc %>% 
  count(start_station_name)
bikecalc %>% 
  count(end_station_name)
names(sort(-table(bikecalc$start_station_name))) [1]
names(sort(-table(bikecalc$end_station_name))) [1]

# Seasons Analysis
bikecalc <- bikecalc %>% 
  mutate(season = case_when(
    month %in% c("01", "02", "03") ~ 'Winter',
    month %in% c("04", "05", "06") ~ 'Spring',
    month %in% c("07","08", "09") ~ 'Summer',
    month %in% c("10", "11", "12") ~ 'Fall'
  ))
bikecalc %>%
  group_by(member_status) %>%
  filter(season == "Spring") %>%
  count(season)
# Make sure it adds up
bikecalc %>%
  filter(season == "Spring") %>%
  count(season)
bikecalc %>%
  group_by(member_status) %>%
  filter(season == "Summer") %>%
  count(season)
bikecalc %>%
  group_by(member_status) %>%
  filter(season == "Fall") %>%
  count(season)
bikecalc %>%
  group_by(member_status) %>%
  filter(season == "Winter") %>%
  count(season)
bikecalc %>% 
  group_by(season,member_status) %>% 
  summarize_at(vars(ride_length),list(time=mean))

# Mean, Max, and Mode for Ride Length
mean(bikecalc$ride_length)
round(mean(bikecalc$ride_length))
# The mean ride length is 17.09584 minutes (17 mins rounded)
max(bikecalc$ride_length)/60/24
round(max(bikecalc$ride_length)/60/24)
# The max ride length is 23.85699 minutes (24 mins rounded)
names(sort(-table(bikecalc$ride_length))) [1]
# The mode is 5.98333 minutes

View(bikecalc)

# Member Status Pie Chart
totalriderspie <- c(1758150, 2611141)
pie(totalriderspie)
pie_labels <- c("Casual", "Members")
colors <- c("#f0fc42", "#0097a7")
pie(totalriderspie, label=pie_labels, main = "Total Cyclistic Riders", col=colors)
legend("bottomright", pie_labels, fill = colors)

# Most Used Bike Type
bikecalc %>% 
  ggplot(aes(x= ride_type, fill=member_status))+
  geom_bar(position= "dodge")+
  labs(title="Most Used Bike Type",x="Ride Type",y="Count")+
  theme(legend.title=element_blank())

# Popular Days of the Week for Casual and Member Separately
ggplot(data=bikecalc) + geom_bar(mapping=aes(x=day_of_week), fill="#0097a7")+facet_wrap(~member_status) +labs(title="Popular Days for Cyclistic Riders",x="Days of the week",y="Count")+theme(panel.spacing = unit(4, "lines"), axis.text.x=element_text(angle=45, hjust=1))

# Average Ride Length per day of week Chart
ggplot(data=bikecalc,aes(x=day_of_week,y=ride_length,fill=day_of_week))+geom_bar(stat='identity')+labs(title="Average Ride Length per Day", x="Day of the Week")+scale_y_continuous(name="Average Ride Length (minutes)", labels = scales::number_format(accuracy = 0.1))+theme(legend.title=element_blank())

# Average Ride Length for Members and Casual Riders separately
ggplot(data=bikecalc) + geom_col(mapping=aes(x=day_of_week,y=ride_length), fill="#f0fc42")+facet_wrap(~member_status) +labs(title="Average Ride Length for Members and Casual Riders",x="Days of the week",y="Avg Ride Length")+theme(panel.spacing = unit(4, "lines"), axis.text.x=element_text(angle=45, hjust=1))+scale_y_continuous(name="Average Ride Length (minutes)", labels = scales::number_format(accuracy = 0.1))

# Number of Rides per month
ridespermonth %>% 
  ggplot(aes(month, num_rides_month, fill = member_status))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(title = "Number of Rides per month",x = "month",y = "number of rides"
  )+theme()
ridespermonth %>% 
  ggplot(aes(month, num_rides_month, fill = member_status))+
  geom_col(position = "dodge")+
  scale_y_continuous()+
  labs(title = "Number of Rides per month",x = "month",y = "number of rides"
  )+theme(legend.title=element_blank())

# Seasons
ggplot(data=bikecalc,aes(x=season,y=season,fill=season))+geom_bar(stat='identity')+
  + labs(title="Seasons and Ride Usage",x="Seasons",y="Usage")

# Popular Stations (Start and End)
# START
start_station <- bikecalc %>% 
  group_by(member_status, start_station_name) %>% 
  summarize(startpoint = n()) %>% 
  arrange(-startpoint)
start_station[1:10,] %>% 
  ggplot(aes(start_station_name, startpoint, fill = member_status))+
  geom_col(position = "dodge")+
  coord_flip()+
  labs(title = "Top 10 Most Popular Start Stations",x = "Station Name",y = "Number of outbound rides")+
  theme(legend.title=element_blank())
# END
end_station <- bikecalc %>% 
  group_by(member_status, end_station_name) %>% 
  summarize(endpoint = n()) %>% 
  arrange(-endpoint)
end_station[1:10,] %>% 
  ggplot(aes(end_station_name, endpoint, fill = member_status))+
  geom_col(position = "dodge")+
  coord_flip()+
  labs(title = "Top 10 Most Popular End Stations",subtitle="(where riders end their journey)",x = "Station Name",y = "Number of inbound rides")+
  theme(legend.title=element_blank())
