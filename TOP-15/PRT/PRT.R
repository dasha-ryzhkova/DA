#
#
# Portugal       
#
#
library(dplyr)
library(ggplot2)

#1. Доля вибору типу готелів
prt_data <- top_countries %>%
  filter(country == "PRT")

prt_hotelbookings <- prt_data %>%
  group_by(hotel) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(prt_hotelbookings, aes(x = hotel, y = n, fill = hotel)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label = paste0(round(percentage), "%"), y = percentage), 
            position = position_dodge(width = 0.9),
            size = 10, vjust = -0.7) +
  labs(title = "Доля вибору типу готелів замовників із Португалії", 
       x = "Типи готелів", y = "Кількість бронювань") +
  theme(plot.title = element_text(hjust = 0.5))

#2. Дослідження популярних місяців для відпочинку
prt_month_hotelbookings <- prt_data %>% 
  group_by(arrival_date_month, hotel) %>% 
  summarize(n = n()) %>% 
  mutate(total = sum(n), percentage = n/total * 100) %>% 
  arrange(arrival_date_month, hotel)

ggplot(prt_month_hotelbookings, aes(x = arrival_date_month, y = n, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage), "%"), y = percentage), 
            position = position_dodge(width = 0.9),
            size = 2, vjust = -0.7) +
  labs(title = "Кількість бронювань(замовлень) в залежності від місяця", x = "Місяці", y = "Кількість бронювань") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 330, hjust = 0.1))

