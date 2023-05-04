library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)


# Залежність adr від booking_changes , total_of_special_requests , required_car_parking_spaces , lead_time 

adr_data <- data

adr_data <- adr_data %>%  select(c(adr, booking_changes, total_of_special_requests, 
                                   required_car_parking_spaces, lead_time))
adr_data <- adr_data[with(adr_data, order(-adr)), ]
ggcorr(adr_data, method = c("everything", "pearson"))
adr_data <- mutate(adr_data, total_requests = booking_changes + total_of_special_requests
                   + required_car_parking_spaces)

ggcorr(adr_data, method = c("everything", "pearson")) 

