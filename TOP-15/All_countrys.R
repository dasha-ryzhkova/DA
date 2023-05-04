#у який тип готелю люблять подорожувати різні країни (усі бронювання)

top_countries_list <- data %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  pull(country)

top_countries <- data %>%
  filter(country %in% top_countries_list)

relative_data <- top_countries %>%
  group_by(country, hotel) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)

relative_data %>%   print(n = Inf)




countries <- unique(top_countries[, 'country'])
# Побудувати діаграми
ggplot(relative_data, aes(x = hotel, y = percentage, fill = hotel)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~country) +
  labs(title = "Доля кількості бронювань країни для кожного типу готелю", 
       x = "Тип готелю", y = "Відносна частка бронювань (%)") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 330, hjust = 0.1))

ggplot(relative_data, aes(x = countries, y = percentage, fill = hotel)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип готелю", y = "Відносна частка бронювань (%)") +
  scale_x_discrete(labels = c(unique(relative_data[, 'country']))) +
  scale_fill_discrete(labels = c("City Hotel", "Resort Hotel", fill='Hotel')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))


specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data_t <- data.frame(specie,condition,value)
data_t
