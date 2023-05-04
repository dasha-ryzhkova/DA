library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)


# Залежність adr від booking_changes , total_of_special_requests , required_car_parking_spaces , lead_time 

adr_data <- data

adr_data <- adr_data %>%  select(c(adr, booking_changes, total_of_special_requests, 
                                   required_car_parking_spaces, stays_in_nights, lead_time))
adr_data <- adr_data[with(adr_data, order(-adr)), ]

adr_data <- mutate(adr_data, total_requests = booking_changes + total_of_special_requests
                   + required_car_parking_spaces)

adr_data1 <- adr_data %>%  select(-c(stays_in_nights))

ggcorr(adr_data1, method = c("everything", "pearson"))



#for data1
adr_data <- data1

adr_data <- adr_data %>%  select(c(adr, booking_changes, total_of_special_requests, 
                                   required_car_parking_spaces, stays_in_nights, lead_time))
adr_data <- adr_data[with(adr_data, order(-adr)), ]

adr_data <- mutate(adr_data, total_requests = booking_changes + total_of_special_requests
                   + required_car_parking_spaces)

adr_data1 <- adr_data %>%  select(-c(stays_in_nights))

ggcorr(adr_data1, method = c("everything", "pearson"))
