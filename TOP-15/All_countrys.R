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

# Побудувати діаграми
ggplot(relative_data, aes(x = hotel, y = percentage, fill = hotel)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~country) +
  labs(title = "Доля кількості бронювань країни для кожного типу готелю", 
       x = "Тип готелю", y = "Відносна частка бронювань (%)") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 330, hjust = 0.1))



