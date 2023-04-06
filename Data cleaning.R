library(dplyr)
library(ggplot2)
library("stringr")

data <- read.csv('hotel_bookings.csv')
data$children[is.na(data$children)] = round(mean(data$children, na.rm = TRUE),0)

data <- mutate(data, all_guests = adults + children + babies)             
data <-  filter(data, all_guests != 0) 

data <- data %>% select(-c(agent, company))

data <- mutate(data, stays_in_nights = stays_in_weekend_nights + stays_in_week_nights)  

data %>% group_by(stays_in_nights) %>% filter(stays_in_nights == 0) %>%   print(n = Inf)

data <-  filter(data, stays_in_nights != 0)

data$required_car_parking_spaces[data$required_car_parking_spaces > 2] <- 2

data <-  filter(data, adr > 0)
data <-  filter(data, adr < 400)

data <-  filter(data, babies < 5)
data <-  filter(data, children < 10)

data <-  filter(data, adults < 5)

data <-  filter(data, lead_time  < 700)

data <-  filter(data, country != 'NULL')

colSums(is.na(data))
dim(data)
