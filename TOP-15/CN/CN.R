#
#
# Китай (China)     
#
#
library(dplyr)
library(ggplot2)

#1. Доля вибору типу готелів
cn_data <- top_countries %>%
  filter(country == "CN")

cn_hotelbookings <- cn_data %>%
  group_by(hotel) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(cn_hotelbookings, aes(x = hotel, y = n, fill = hotel)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label = paste0(round(percentage), "%"), y = percentage), 
            position = position_dodge(width = 0.9),
            size = 10, vjust = -0.7) +
  labs(title = "Доля вибору типу готелів, які обирають замовники із Китай", 
       x = "Типи готелів", y = "Кількість бронювань") +
  theme(plot.title = element_text(hjust = 0.5))

#2. Дослідження популярних місяців для відпочинку
cn_month_hotelbookings <- cn_data %>% 
  group_by(arrival_date_month, hotel) %>% 
  summarize(n = n()) %>% 
  mutate(total = sum(n), percentage = n/total * 100) %>% 
  arrange(arrival_date_month, hotel)

ggplot(cn_month_hotelbookings, aes(x = arrival_date_month, y = n, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_dodge(width = 0.9),
            size = 2, vjust = -0.5) +
  labs(title = "Кількість бронювань(замовлень) в залежності від місяця", x = "Місяці", y = "Кількість бронювань") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 330, hjust = 0.1))
