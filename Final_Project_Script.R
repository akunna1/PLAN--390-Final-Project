# Setting my working directory
setwd("C:/Users/akunna1/Desktop/PLAN 390/Final Project/Files Used")


# Load libraries
library(tidyverse)  # dealing with tabular data
library(lubridate) # handling dates
library(dplyr) # data manipulation
library(tidyr) # tidy data
library(data.table) # table formated result
library(ggplot2) # visualization
library(scales)# graphical scaling

# Importing my .csv data to the Global Environment
# 3 years of data
police_data_2020 = read_csv("2020_police_data.csv")
police_data_2021 = read_csv("2021_police_data.csv")
police_data_2022 = read_csv("2022_police_data.csv")

police_data <- rbind(police_data_2020,police_data_2021,police_data_2022)

#Data Analysis
#police_data$reported_date <- as.POSIXct(police_data$reported_date, format = "%Y/%m/%d %H:%M:%S")

#extracting time from date time
police_data$reported_time <- format(as.POSIXct(police_data$reported_date, format = "%Y/%m/%d %H:%M:%S"), format = "%H:%M:%S")
police_data$reported_time # includes hours, mins and secs

# changing hour factors
police_data$reported_hour <- as.factor(police_data$reported_hour)

# plotting the incidents by hours in a day
hour_data <- police_data %>%
  group_by(reported_hour) %>%
  summarise(Total = n()) # grouping the data with hour and count

#see in tabular form
data.table(hour_data)

# Visualize the data
ggplot(hour_data, aes(reported_hour,Total)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  ggtitle("Incidents By Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

month_hour_data <- police_data %>%
  group_by(reported_month,reported_hour) %>%
  summarise(Total = n()) # grouping the data with hour and count

#see in tabular form
data.table(month_hour_data)

# plot the same
ggplot(month_hour_data, aes(reported_hour,Total, fill=reported_month)) +
  geom_bar(stat = "identity") +
  ggtitle("Incidents By Hour and Month") +
  scale_y_continuous(labels = comma)
  
