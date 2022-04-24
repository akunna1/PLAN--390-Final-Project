# Set working directory to Source File Location

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
arrest_data_2018 = read_csv("2018_police_data.csv")
arrest_data_2019 = read_csv("2019_police_data.csv")
arrest_data_2020 = read_csv("2020_police_data.csv")

arrest_data <- rbind(arrest_data_2018,arrest_data_2019,arrest_data_2020)

# **********************
# Analysis of Year 2018 Data
# ** Run Each Month Analysis at a time to see each plot**
# Month Analysis
#extracting month from date and making a new column for it
arrest_data_2018$month_of_arrest <- format(as.POSIXct(arrest_data_2018$Date_of_Arrest, format = "%Y/%m/%d %H:%M:%S"), format = "%m")

# changing months to factors
arrest_data_2018$month_of_arrest <- as.factor(arrest_data_2018$month_of_arrest)

# plotting the arrests in 2018 by months
month_data <- arrest_data_2018 %>%
  group_by(month_of_arrest) %>%
  summarise(Total = n()) # grouping the data with month and count

#see in tabular form
data.table(month_data)

# Visualize the data
ggplot(month_data, aes(month_of_arrest,Total)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  ggtitle("Year 2018 Arrests By Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# **********************
# Analysis of Year 2019 Data
# Month Analysis
#extracting month from date and making a new column for it
arrest_data_2019$month_of_arrest <- format(as.POSIXct(arrest_data_2019$Date_of_Arrest, format = "%Y/%m/%d %H:%M:%S"), format = "%m")

# changing months to factors
arrest_data_2019$month_of_arrest <- as.factor(arrest_data_2019$month_of_arrest)

# plotting the arrests in 2019 by months
month_data <- arrest_data_2019 %>%
  group_by(month_of_arrest) %>%
  summarise(Total = n()) # grouping the data with month and count

#see in tabular form
data.table(month_data)

# Visualize the data
ggplot(month_data, aes(month_of_arrest,Total)) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  ggtitle("Year 2019 Arrests By Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# **********************
# Analysis of Year 2020 Data
# Month Analysis
#extracting month from date and making a new column for it
arrest_data_2020$month_of_arrest <- format(as.POSIXct(arrest_data_2020$Date_of_Arrest, format = "%Y/%m/%d %H:%M:%S"), format = "%m")

# changing months to factors
arrest_data_2020$month_of_arrest <- as.factor(arrest_data_2020$month_of_arrest)

# plotting the arrests in 2020 by months
month_data <- arrest_data_2020 %>%
  group_by(month_of_arrest) %>%
  summarise(Total = n()) # grouping the data with month and count

#see in tabular form
data.table(month_data)

# Visualize the data
ggplot(month_data, aes(month_of_arrest,Total)) +
  geom_bar(stat = "identity", fill = "wheat2", color = "black") +
  ggtitle("Year 2020 Arrests By Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# **********************
# Analysis of 2018 -2020 Data
# Month Analysis
#extracting month from date and making a new column for it
arrest_data$month_of_arrest <- format(as.POSIXct(arrest_data$Date_of_Arrest, format = "%Y/%m/%d %H:%M:%S"), format = "%m")

# changing months to factors
arrest_data$month_of_arrest <- as.factor(arrest_data$month_of_arrest)

# plotting the arrests in 2018 - 2020 by months
month_data <- arrest_data %>%
  group_by(month_of_arrest) %>%
  summarise(Total = n()) # grouping the data with month and count

#see in tabular form
data.table(month_data)

# Visualize the data
ggplot(month_data, aes(month_of_arrest,Total)) +
  geom_bar(stat = "identity", fill = "thistle1", color = "black") +
  ggtitle("Year 2018-2020 Arrests By Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)




