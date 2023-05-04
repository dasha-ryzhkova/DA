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


ggplot(relative_data, aes(fill = hotel, x = country, y = percentage)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип готелю", y = "Відносна частка бронювань (%)") +
  scale_x_discrete(labels = c(unique(relative_data[, 'country']))) +
  scale_fill_discrete(labels = c("Resort Hotel", "City Hotel", fill='Hotel')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))
relative_data

ggplot(relative_data, aes(fill = hotel, x = country, y = log(n))) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип готелю", y = "log(Бронювання)") +
  scale_x_discrete(labels = c(unique(relative_data[, 'country']))) +
  scale_fill_discrete(labels = c("Resort Hotel", "City Hotel", fill='Hotel')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

ggplot(relative_data, aes(fill = hotel, x = country, y = n)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип готелю", y = "Бронювання") +
  scale_x_discrete(labels = c(unique(relative_data[, 'country']))) +
  scale_fill_discrete(labels = c("Resort Hotel", "City Hotel", fill='Hotel')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))


# для data9
top_countries_list <- data9 %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  pull(country)

top_countries <- data9 %>%
  filter(country %in% top_countries_list)

relative_data <- top_countries %>%
  group_by(country, hotel) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)


ggplot(relative_data, aes(fill = hotel, x = country, y = percentage)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип готелю", y = "Відносна частка бронювань (%)") +
  scale_x_discrete(labels = c(unique(relative_data[, 'country']))) +
  scale_fill_discrete(labels = c("Resort Hotel", "City Hotel", fill='Hotel')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

ggplot(relative_data, aes(fill = hotel, x = country, y = log(n))) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип готелю", y = "log(Бронювання)") +
  scale_x_discrete(labels = c(unique(relative_data[, 'country']))) +
  scale_fill_discrete(labels = c("Resort Hotel", "City Hotel", fill='Hotel')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

ggplot(relative_data, aes(fill = hotel, x = country, y = n)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Тип готелю", y = "Бронювання") +
  scale_x_discrete(labels = c(unique(relative_data[, 'country']))) +
  scale_fill_discrete(labels = c("Resort Hotel", "City Hotel", fill='Hotel')) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))
