library(dplyr)
library(ggplot2)
library(GGally)


without_children <- data 
without_children$with_children <- with(without_children, ifelse(all_children > 0, 
                                                                'З дітьми', 'Без дітей'))

meal_data <- without_children %>%  select(c(meal, with_children))
meal_data <- meal_data %>% 
  group_by(meal, with_children) %>% 
  summarize(n = n()) 
meal_data <- meal_data[-c(9, 10),]

room_data <- without_children %>%  select(c(reserved_room_type, with_children))
room_data <- room_data %>% 
  group_by(reserved_room_type, with_children) %>% 
  summarize(n =  n()) 

distribution_data <- without_children %>%  select(c(distribution_channel, with_children))
distribution_data <- distribution_data %>% 
  group_by(distribution_channel, with_children) %>% 
  summarize(n =  n()) 
distribution_data <- meal_data[-c(8, 9),]



# new_data = filter(data, all_children == 0) 
# ggplot(new_data, aes(x=meal)) + 
#   geom_bar(fill = "#f61751")+
#   geom_text(stat='count', aes(label=..count..), vjust=-0.5)
# 
# ggplot(meal_data, aes(n, meal)) +
#   geom_line(aes(group = meal)) +
#   geom_point(aes(color = with_children)) +
#   labs(x = "Кількість", y = "Тип замовленого харчування", color = "З/без дітей")
  
