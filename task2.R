library(fmsb)
library(reshape2)

top_countries_list <- data %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  pull(country)

top_countries <- data %>%
  filter(country %in% top_countries_list)

hotel_booking <- top_countries %>%
  group_by(hotel) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)

month_hotelbookings <- top_countries %>% 
  group_by(country, arrival_date_month, hotel) %>% 
  summarize(n = n()) %>% 
  mutate(total = sum(n), percentage = n/total * 100) %>% 
  arrange(arrival_date_month, hotel)


month_hotelbookings <- month_hotelbookings %>%  select(-c(hotel, n, percentage))
month_hotelbookings <- month_hotelbookings %>% distinct()



month_hotelbookings <- dcast(month_hotelbookings,country ~arrival_date_month )

month_hotelbookings2 <- month_hotelbookings[,-1]
rownames(month_hotelbookings2) <- month_hotelbookings[,1]


month_hotelbookings2

max_min <- data.frame(
  April = c(3938, 56), August = c(5020, 76), 
  December = c(2880, 58), February = c(3555, 22), 
  January = c(2555, 23), July = c(4764, 123), 
  June = c(4278, 112), March = c(3602, 68), 
  May = c(4174, 76), November = c(2882, 24),
  October = c(4714, 102), September = c(4657, 61)
)

rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, month_hotelbookings2)

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), 
                                        vlcex = 0.7, caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    axislabcol = "grey", 
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
  data
}

colors <- c("#00AFBB", "#E7B800", "#FC4E07", "#FCE607", "#A3FC07", "#28FC07"
                     , "#07FC85", "#07FCE6", "#07C4FC", "#0760FC", "#C807FC"
                     , "#FC07B9", "#8E44AD", "#FC073F", "#3407FC")

titles <- c("Австрія", "Бельгія", "Бразилія", "Швейцарія", "Китай", "Німеччина",
            "Іспанія", "Франція", "Велика Британія", "Ірландія", "Італія",
            "Нідерланди", "Португалія", "Швеція", "США")

op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(3,5))

for(i in 1:15){
      create_beautiful_radarchart(
        data = df[c(1, 2, i+2), ],
        color = colors[i], title = titles[i]
      )
    }

# aut_bookings <- df[c("Max", "Min", "AUT"), ]
# 
# radarchart(aut_bookings)
# 
# 
# bel_bookings <- df[c("Max", "Min", "BEL"), ]
# 
# radarchart(bel_bookings)
# 
# 
# bra_bookings <- df[c("Max", "Min", "BRA"), ]
# 
# radarchart(bra_bookings)
# 
# 
# che_bookings <- df[c("Max", "Min", "CHE"), ]
# 
# radarchart(che_bookings)
# 
# 
# cn_bookings <- df[c("Max", "Min", "CN"), ]
# 
# radarchart(cn_bookings)
# 
# 
# deu_bookings <- df[c("Max", "Min", "DEU"), ]
# 
# radarchart(deu_bookings)
# 
# 
# esp_bookings <- df[c("Max", "Min", "ESP"), ]
# 
# radarchart(esp_bookings)
# 
# fra_bookings <- df[c("Max", "Min", "FRA"), ]
# 
# radarchart(fra_bookings)
# 
# 
# gbr_bookings <- df[c("Max", "Min", "GBR"), ]
# 
# radarchart(gbr_bookings)
# 
# 
# irl_bookings <- df[c("Max", "Min", "IRL"), ]
# 
# radarchart(irl_bookings)
# 
# 
# ita_bookings <- df[c("Max", "Min", "ITA"), ]
# 
# radarchart(ita_bookings)
# 
# 
# 
# nld_bookings <- df[c("Max", "Min", "NLD"), ]
# 
# radarchart(nld_bookings)
# 
# 
# prt_bookings <- df[c("Max", "Min", "PRT"), ]
# 
# radarchart(prt_bookings)
# 
# 
# swe_bookings <- df[c("Max", "Min", "SWE"), ]
# 
# radarchart(swe_bookings)
# 
# 
# usa_bookings <- df[c("Max", "Min", "USA"), ]
# 
# radarchart(usa_bookings)

# for data9

top_countries_list <- data9 %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  pull(country)

top_countries <- data9 %>%
  filter(country %in% top_countries_list)

hotel_booking <- top_countries %>%
  group_by(hotel) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)

month_hotelbookings <- top_countries %>% 
  group_by(country, arrival_date_month, hotel) %>% 
  summarize(n = n()) %>% 
  mutate(total = sum(n), percentage = n/total * 100) %>% 
  arrange(arrival_date_month, hotel)


month_hotelbookings <- month_hotelbookings %>%  select(-c(hotel, n, percentage))
month_hotelbookings <- month_hotelbookings %>% distinct()



month_hotelbookings <- dcast(month_hotelbookings,country ~arrival_date_month )

month_hotelbookings2 <- month_hotelbookings[,-1]
rownames(month_hotelbookings2) <- month_hotelbookings[,1]


month_hotelbookings2

max_min <- data.frame(
  April = c(1140, 56), August = c(1690, 76), 
  December = c(928, 27), February = c(810, 22), 
  January = c(465, 23), July = c(1329, 76), 
  June = c(1374, 112), March = c(982, 68), 
  May = c(1546, 61), November = c(709, 24),
  October = c(1310, 62), September = c(1130, 61)
)

rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, month_hotelbookings2)

colors <- c("#00AFBB", "#E7B800", "#FC4E07", "#FCE607", "#A3FC07", "#28FC07"
                     , "#07FC85", "#07FCE6", "#07C4FC", "#0760FC", "#C807FC"
                     , "#FC07B9", "#8E44AD", "#FC073F", "#3407FC")


titles <- c("Австрія", "Бельгія", "Бразилія", "Швейцарія", "Китай", "Німеччина",
            "Іспанія", "Франція", "Велика Британія", "Ірландія", "Італія",
            "Нідерланди", "Польща", "Швеція", "США")


op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(3,5))

for(i in 1:15){
  create_beautiful_radarchart(
    data = df[c(1, 2, i+2), ],
    color = colors[i], title = titles[i]
  )
}

# aut_bookings <- df[c("Max", "Min", "AUT"), ]
# 
# radarchart(aut_bookings)
# 
# 
# bel_bookings <- df[c("Max", "Min", "BEL"), ]
# 
# radarchart(bel_bookings)
# 
# 
# bra_bookings <- df[c("Max", "Min", "BRA"), ]
# 
# radarchart(bra_bookings)
# 
# 
# che_bookings <- df[c("Max", "Min", "CHE"), ]
# 
# radarchart(che_bookings)
# 
# 
# cn_bookings <- df[c("Max", "Min", "CN"), ]
# 
# radarchart(cn_bookings)
# 
# 
# deu_bookings <- df[c("Max", "Min", "DEU"), ]
# 
# radarchart(deu_bookings)
# 
# 
# esp_bookings <- df[c("Max", "Min", "ESP"), ]
# 
# radarchart(esp_bookings)
# 
# 
# fra_bookings <- df[c("Max", "Min", "FRA"), ]
# 
# radarchart(fra_bookings)
# 
# 
# gbr_bookings <- df[c("Max", "Min", "GBR"), ]
# 
# radarchart(gbr_bookings)
# 
# 
# irl_bookings <- df[c("Max", "Min", "IRL"), ]
# 
# radarchart(irl_bookings)
# 
# 
# ita_bookings <- df[c("Max", "Min", "ITA"), ]
# 
# radarchart(ita_bookings)
# 
# 
# 
# nld_bookings <- df[c("Max", "Min", "NLD"), ]
# 
# radarchart(nld_bookings)
# 
# 
# swe_bookings <- df[c("Max", "Min", "SWE"), ]
# 
# radarchart(swe_bookings)
# 
# 
# usa_bookings <- df[c("Max", "Min", "USA"), ]
# 
# radarchart(usa_bookings)
# 
# 
# pol_bookings <- df[c("Max", "Min", "POL"), ]
# 
# radarchart(pol_bookings)
