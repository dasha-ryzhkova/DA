# data8 + data3 from condition
data8.3 <- data8 %>% 
  filter(stays_in_week_nights < 30)


ggplot(data8.3, aes(x = arrival_date_month, y = adr)) +
  geom_boxplot() +
  labs(x = "Місяць", y = "Середня ціна за ніч")

ggplot(subset(data8.3, hotel == "Resort Hotel"), aes(x = arrival_date_month, y = adr)) +
  geom_boxplot() +
  labs(title = "Середня ціна за ніч у курортному готелі",
       x = "Місяць", y = "Середня ціна за ніч")

ggplot(subset(data, hotel == "City Hotel"), aes(x = arrival_date_month, y = adr)) +
  geom_boxplot() +
  labs(title = "Середня ціна за ніч у готелі в місті",
       x = "Місяць", y = "Середня ціна за ніч")


#виключаючи португалію

data8.3.9 <- data8.3 %>% 
  filter(country != "PRT")

ggplot(data8.3.9, aes(x = arrival_date_month, y = adr)) +
  geom_boxplot() +
  labs(x = "Місяць", y = "Середня ціна за ніч")


ggplot(subset(data9, hotel == "Resort Hotel"), aes(x = arrival_date_month, y = adr)) +
  geom_boxplot() +
  labs(title = "Середня ціна за ніч у курортному готелі",
       x = "Місяць", y = "Середня ціна за ніч")

ggplot(subset(data9, hotel == "City Hotel"), aes(x = arrival_date_month, y = adr)) +
  geom_boxplot() +
  labs(title = "Середня ціна за ніч у готелі в місті",
       x = "Місяць", y = "Середня ціна за ніч")



