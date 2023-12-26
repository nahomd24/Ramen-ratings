library("tidyverse")
# step 1
ramen <- read.csv("ramen-ratings.csv")
ramen
# step 2 
ramen_stats <- ramen %>%
  group_by(Style) %>%
  summarize(MeanStars = mean(Stars),
            SDStars = sd(Stars))
ramen_stats
# step 3
ggplot(data = ramen_stats) +
  geom_col(aes(x = Style, y = MeanStars, fill = Style))
# step 4
ggplot(data = ramen_stats) +
  geom_col(aes(x = Style, y = MeanStars, fill = Style))+
  geom_errorbar(aes(x = Style, 
                    ymin = MeanStars - SDStars,
                    ymax = MeanStars + SDStars),
                width = 0.25)
# step 5 
ggplot(data = map_data("world"),
       aes(x = long , y = lat , group = group))+
  geom_polygon(fill= "white", color = "black") + 
  coord_quickmap()
# step 6 
countries <- ramen %>% select(Country) %>% unique()
countries <- countries$Country
map_ramen <- map_data("world") %>% filter(region %in% countries)
map_ramen <- as_tibble(map_ramen)
map_ramen

# step 7
ggplot(data = map_ramen,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(fill= "white", color = "black") + 
  coord_quickmap()
# step 8

mean_ratings <- ramen %>%
  group_by(Country)%>%
  summarize(MeanRating = mean(Stars)) %>%
  ungroup()
mean_ratings

# step 9
mean_ratings <- mean_ratings %>% rename(region = Country)
map_ramen <- left_join(map_ramen, mean_ratings, by = "region")
map_ramen
# step 10
ggplot(data = map_ramen,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill= MeanRating), color = "black") + 
  coord_quickmap()
# step 11
ggplot(data = map_ramen,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill= MeanRating), color = "black") + 
  coord_quickmap()+
  labs(title = "Ramen Ratings by region", x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

