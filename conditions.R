library(dplyr)
library(ggplot2)
library("stringr")

data <- read.csv('hotel_bookings.csv')

#lead_time
data1 <- data  %>%  filter(lead_time < 600) 

#stays_in_weekend_nights
data2 <- data  %>% filter(stays_in_weekend_nights < 10)

#stays_in_week_nights
data3 <- data  %>% filter(stays_in_week_nights < 30)

#previous_cancellations
data4 <- data  %>% filter(previous_cancellations < 10) 

#previous_bookings_not_canceled
data5 <- data  %>% filter(previous_bookings_not_canceled < 25)

#booking_changes
data6 <- data  %>% filter(booking_changes < 5)

#days_in_waiting_list
data7 <- data  %>% filter(days_in_waiting_list < 200)

#total_of_special_requests
data8 <- data  %>% filter(total_of_special_requests < 3)

data9 <- data  %>% filter(country != 'PRT', country != 'CHN')

data9 %>% 
  filter(is_canceled == 0) %>%
  group_by(country) %>%  
  summarise(booking_count = n()) %>%
  arrange(desc(booking_count)) %>%
  head(15)
