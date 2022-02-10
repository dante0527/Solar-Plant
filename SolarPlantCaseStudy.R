# Install required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("skimr")
install.packages("janitor")
install.packages("chron")

# Load packages
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(skimr)  #summary statistics
library(janitor)  #helps with cleaning data
library(chron)  #helps deal with dates

#ADD DATA AND CLEAN FOR ANALYSIS

# Load data set
plant0 <- read_csv("Plant_1_Generation_Data.csv")

# Make copy of data frame with unique and lowercase column names
plant1 <- plant0 %>%
  rename_with(tolower) %>%
  clean_names()

# Change "date_time" from "chr" to "dttm" data type
plant1 <- plant1 %>%
  mutate(date_time = dmy_hm(date_time))

# Create new variables "date" and "time" from existing variable "date_time"
plant1 <- plant1 %>%
  mutate(date = as_date(date_time)) %>%
  mutate(time = format(date_time, format = "%H:%M:%S"))

# Change "time" variable to type "chron" to make it continuous
plant1 <- plant1 %>%
  mutate(time = chron(times(plant1$time)))

# Calculate mean DC production for each inverter by time of day
# and save as new variable
plant1 <- plant1 %>%
  group_by(time, source_key) %>%
  mutate(mean_dc_power = mean(dc_power))

# DESCRIPTIVE ANALYSIS

#Find number of inverters on the plant
length(unique(plant1$source_key))

#Calculate average daily yield
average_daily_yield <- mean(plant1$daily_yield)

#Min and max DC power
min(plant1$dc_power)
max(plant1$dc_power)

plant1 %>%
  group_by(source_key) %>%
  summarise(maxYield = max(dc_power))



#============VISUALIZATION ANALYSIS====================



# Visualize daily yield for entire date range
ggplot(plant1, aes(date_time, daily_yield)) +
  geom_line(color = "blue") +
  labs(title = "Daily Yield",
       caption = paste0("Data from: ", mindate, " to ", maxdate),
       x = "Date/Time",
       y = "kW")

# Visualize AC and DC power yield during day
ggplot(plant1, aes(time)) +
  geom_point(mapping = aes(y = ac_power, color = "red")) +
  geom_point(mapping = aes(y = dc_power, color = "blue")) +
  labs(title = "AC and DC power yield during day",
       caption = paste0("Data from: ", mindate, " to ", maxdate),
       x = "Time of day",
       y = "Power yield (kW)") +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.8, 0.8)) +
  scale_colour_manual(name = "", guide = "legend",
                      values = c("red" = "red", "blue" = "blue"),
                      labels = c("AC Power", "DC Power"))

# Visualize Total Yield by Date
ggplot(plant1) +
  geom_col(aes(date, total_yield, width = 0.5), color = "blue") +
  scale_x_date(date_labels = "%b %e", date_breaks = "2 days") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Total Yield by Date",
       caption = paste0("Data from: ", mindate, " to ", maxdate),
       x = "Date",
       y = "Total Yield (kW)")

# Establish min and max dates
mindate <- min(plant1$date)
maxdate <- max(plant1$date)

# Visualize DC power for each inverter by time of day
ggplot(plant1, aes(time, mean_dc_power, color = source_key)) +
  geom_line() +
  labs(title = "Average DC power production for each inverter by time of day",
       caption = paste0("Data from: ", mindate, " to ", maxdate),
       x = "Time of day",
       y = "Average DC Power Output (kW)",
       color = "Inverter Source Key") +
  theme(axis.text.x = element_blank())

# Visualize DC power and daily yield for each day
ggplot(plant1) +
  geom_line(aes(time, dc_power)) +
  geom_line(aes(time, daily_yield), linetype = "dashed") +
  facet_wrap(~date)