library(dplyr)
library(ggplot2)
library(GGally)
library(reshape2)


without_children <- data 
without_children$with_children <- with(without_children, ifelse(all_children > 0, 
                                                      'З дітьми', 'Без дітей'))

meal_data <- without_children %>%  select(c(meal, with_children))

meal_data <- meal_data %>%
  group_by(meal, with_children) %>%
  summarise(n = n())
meal_data <- meal_data[-c(9, 10),]

ggplot(meal_data, aes(fill = with_children, x = meal, y = log(n))) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип харчування", y = "log(Бронювання)", fill='З/без дітей') +
  scale_fill_discrete(labels = c('З дітьми', 'Без дітей')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

ggplot(meal_data, aes(fill = with_children, x = meal, y = n)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип харчування", y = "Бронювання", fill='З/без дітей') +
  scale_fill_discrete(labels = c('З дітьми', 'Без дітей')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))



room_data <- without_children %>%  select(c(reserved_room_type, with_children))
room_data <- room_data %>% 
  group_by(reserved_room_type, with_children) %>% 
  summarize(n =  n()) 

ggplot(room_data, aes(fill = with_children, x = reserved_room_type, y = log(n))) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип харчування", y = "log(Бронювання)", fill='З/без дітей') + 
  scale_fill_discrete(labels = c('З дітьми', 'Без дітей')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

ggplot(room_data, aes(fill = with_children, x = reserved_room_type, y = n)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип номеру", y = "Бронювання", fill='З/без дітей') + 
  scale_fill_discrete(labels = c('З дітьми', 'Без дітей')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))







distribution_data <- without_children %>%  select(c(distribution_channel, with_children))
distribution_data <- distribution_data %>% 
  group_by(distribution_channel, with_children) %>% 
  summarize(n =  n()) 
distribution_data <- distribution_data[-c(8, 9),]


ggplot(distribution_data, aes(fill = with_children, x = distribution_channel, y = log(n))) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип бронювання", y = "log(Бронювання)", fill='З/без дітей') + 
  scale_fill_discrete(labels = c('З дітьми', 'Без дітей')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))


ggplot(distribution_data, aes(fill = with_children, x = distribution_channel, y = n)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип бронювання", y = "Бронювання", fill='З/без дітей') + 
  scale_fill_discrete(labels = c('З дітьми', 'Без дітей')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))


datamm <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)
