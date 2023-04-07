library(dplyr)
library(ggplot2)
library("stringr")

data <- read.csv('hotel_bookings.csv')

#lead_time
data1 <-  filter(lead_time > 600) 

#stays_in_weekend_nights
data2 <-  filter(stays_in_weekend_nights > 10)

#stays_in_week_nights
data3 <-  filter(stays_in_week_nights > 30)

#previous_cancellations
data4 <-  filter(previous_cancellations > 10) 

#previous_bookings_not_canceled
data5 <-  filter(previous_bookings_not_canceled > 25)

#booking_changes
data6 <-  filter(booking_changes > 5)

#days_in_waiting_list
data7 <-  filter(days_in_waiting_list > 200)

#total_of_special_requests
data8 <-  filter(total_of_special_requests > 3)