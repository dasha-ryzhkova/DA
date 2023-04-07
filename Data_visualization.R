library(dplyr)
library(ggplot2)
library("stringr")

data <- read.csv('hotel_bookings.csv')
str(data)

colSums(is.na(data))
na_rows <- data[is.na(data$children), ]
na_rows
data$children[is.na(data$children)] = round(mean(data$children, na.rm = TRUE),0)
#Факторні змінні

table(data$hotel)
ggplot(data, aes(x=hotel)) + 
  geom_bar(fill = "#FF6666")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$is_canceled)
ggplot(data, aes(x=is_canceled)) + 
  geom_bar(fill = "#601018")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)


table(data$arrival_date_year)
ggplot(data, aes(x=arrival_date_year)) + 
  geom_bar(fill = "#6362ed")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$arrival_date_month)
ggplot(data, aes(x=arrival_date_month)) + 
  geom_bar(fill = "#39cba0")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$arrival_date_week_number)
ggplot(data, aes(x=arrival_date_week_number)) + 
  geom_bar(fill = "#5cc605")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$arrival_date_day_of_month)
ggplot(data, aes(x=arrival_date_day_of_month)) + 
  geom_bar(fill = "#60cde5")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$meal)
ggplot(data, aes(x=meal)) + 
  geom_bar(fill = "#e90785")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$country)
data %>% 
  filter(is_canceled == 0) %>%
  group_by(country) %>%  
  summarise(booking_count = n()) %>%
  arrange(desc(booking_count)) %>%
  head(10)

table(data$market_segment)
ggplot(data, aes(x=market_segment)) + 
  geom_bar(fill = "#fcad2b")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$distribution_channel)
ggplot(data, aes(x=distribution_channel)) + 
  geom_bar(fill = "#1a7ee5")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$is_repeated_guest)
ggplot(data, aes(x=is_repeated_guest)) + 
  geom_bar(fill = "#f95eb9")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$reserved_room_type)
ggplot(data, aes(x=reserved_room_type)) + 
  geom_bar(fill = "#5973aa")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$assigned_room_type)
ggplot(data, aes(x=assigned_room_type)) + 
  geom_bar(fill = "#fcad2b")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$deposit_type)
ggplot(data, aes(x=deposit_type)) + 
  geom_bar(fill = "#be1b52")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$customer_type)
ggplot(data, aes(x=customer_type)) + 
  geom_bar(fill = "#a268df")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

table(data$reservation_status)
ggplot(data, aes(x=reservation_status)) + 
  geom_bar(fill = "#16ec73")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)


#Числові змінні

summary(data %>% select(where(is.numeric)))

summary(data$lead_time)
plot(data$lead_time)
qqnorm(data$lead_time)
# ggplot(data, aes(x=lead_time)) + 
#   geom_boxplot(fill = "#60cde5")

data %>% 
  group_by(lead_time) %>% 
  filter(lead_time > 600) %>%  
  nrow()
data %>% 
  group_by(lead_time) %>% 
  filter(lead_time > 610) %>%   
  print(n = Inf)


summary(data$stays_in_weekend_nights)
plot(data$stays_in_weekend_nights)
qqnorm(data$stays_in_weekend_nights)
# ggplot(data, aes(x=stays_in_weekend_nights)) + 
#   geom_boxplot(fill = "#16ec73")
data %>%  
  group_by(stays_in_weekend_nights) %>% 
  filter(stays_in_weekend_nights > 10) %>%   
  print(n = Inf)

#data %>%  select(lead_time, arrival_date_day_of_month, arrival_date_month, arrival_date_year,  stays_in_weekend_nights, stays_in_week_nights, reservation_status, reservation_status_date) %>% filter(stays_in_weekend_nights > 10)
#data %>% filter(reservation_status != 'Canceled') %>%  nrow()
summary(data$stays_in_week_nights)
plot(data$stays_in_week_nights)
qqnorm(data$stays_in_week_nights)
# ggplot(data, aes(x=stays_in_week_nights)) + 
#   geom_boxplot(fill = "#16ec73")

data %>% 
  group_by(stays_in_week_nights) %>% 
  filter(stays_in_week_nights > 30) %>%   
  print(n = Inf)


summary(data$adults)
plot(data$adults)
qqnorm(data$adults)
# ggplot(data, aes(x=adults)) + 
#   geom_boxplot(fill = "#16ec73")

data %>%  
  group_by(children) %>%   
  filter(adults == 0 & children == 0 & babies == 0 ) %>%   
  print(n = Inf)

data %>%  
  group_by(adults) %>%   
  filter(adults > 4) %>%   
  print(n = Inf)

summary(data$children)
plot(data$children)
qqnorm(data$children)
# ggplot(data, aes(x=children)) + 
#   geom_boxplot(fill = "#16ec73")


summary(data$babies)
plot(data$babies)
qqnorm(data$babies)
# ggplot(data, aes(x=babies)) + 
#   geom_boxplot(fill = "#16ec73")


summary(data$previous_cancellations)
plot(data$previous_cancellations)
qqnorm(data$previous_cancellations)
# ggplot(data, aes(x=previous_cancellations)) + 
#   geom_boxplot(fill = "#16ec73")

data %>%  
  group_by(previous_cancellations) %>%   
  filter(previous_cancellations > 10) %>%   
  print(n = Inf)


summary(data$previous_bookings_not_canceled)
plot(data$previous_bookings_not_canceled)
qqnorm(data$previous_bookings_not_canceled)
# ggplot(data, aes(x=previous_bookings_not_canceled)) + 
#   geom_boxplot(fill = "#16ec73")

data %>%  
  group_by(previous_bookings_not_canceled) %>%   
  filter(previous_bookings_not_canceled > 25) %>%   
  print(n = Inf)


summary(data$booking_changes)
plot(data$booking_changes)
qqnorm(data$booking_changes)
# ggplot(data, aes(x=booking_changes)) + 
#   geom_boxplot(fill = "#16ec73")

data %>%  
  group_by(booking_changes) %>%   
  filter(booking_changes > 5) %>%   
  print(n = Inf)

summary(data$days_in_waiting_list)
plot(data$days_in_waiting_list)
qqnorm(data$days_in_waiting_list)
# ggplot(data, aes(x=log(days_in_waiting_list))) + 
#   geom_boxplot(fill = "#16ec73")

data %>%  
  group_by(days_in_waiting_list) %>%   
  filter(days_in_waiting_list > 200) %>%   
  print(n = Inf)


summary(data$adr)
plot(data$adr)
qqnorm(data$adr)
# ggplot(data, aes(x=log(adr))) + 
#   geom_boxplot(fill = "#16ec73")

data %>%  
  group_by(adr) %>%   
  filter(adr < 0) %>%   
  print(n = Inf)

data %>%  
  group_by(adr) %>%   
  filter(adr == 0) %>%   
  print(n = Inf)

data %>%  
  group_by(adr) %>%   
  filter(adr > 400) %>%   
  print(n = Inf)


summary(data$required_car_parking_spaces)
plot(data$required_car_parking_spaces)
qqnorm(data$required_car_parking_spaces)
# ggplot(data, aes(x=log(required_car_parking_spaces))) + 
#   geom_boxplot(fill = "#16ec73")

data %>%  
  group_by(required_car_parking_spaces) %>%   
  filter(required_car_parking_spaces > 2) %>%   
  print(n = Inf)


summary(data$total_of_special_requests)
plot(data$total_of_special_requests)
qqnorm(data$total_of_special_requests)
# ggplot(data, aes(x=total_of_special_requests)) + 
#   geom_boxplot(fill = "#16ec73")

data %>%  
  group_by(total_of_special_requests) %>%   
  filter(total_of_special_requests > 3) %>%   
  print(n = Inf)


#Інші змінні

table(data$agent)


table(data$company)


rex <- '^(2014|2015|2016|2017)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$'

data %>% 
  filter(str_detect(reservation_status_date, rex)) %>% 
  nrow()


