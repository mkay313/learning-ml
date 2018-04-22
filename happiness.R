library(tidyverse)
library(Amelia)
library(ggthemes)
library(rworldmap)

# data source: https://www.kaggle.com/unsdsn/world-happiness

# read in the data and add the year column
h_2015 <- read.csv("2015.csv", header = TRUE) %>%
  mutate(year = 2015)
h_2016 <- read.csv("2016.csv", header = TRUE) %>%
  mutate(year = 2016)
h_2017 <- read.csv("2017.csv", header = TRUE) %>%
  mutate(year = 2017)

h_2015 <- h_2015 %>%
  select(-Standard.Error)

h_2016 <- h_2016 %>%
  select(-c(Lower.Confidence.Interval, Upper.Confidence.Interval))

h_2017 <- h_2017 %>%
  select(-c(Whisker.high, Whisker.low))

# 2017 is missing the regions -- lets add them
h_2017$Region <- h_2015[match(h_2017$Country, h_2015$Country), "Region"]

# there are some NAs, lets address that
print(is.na(h_2017$Region))
v <- 0
for (i in 1:nrow(h_2017)) {
  if (is.na(h_2017$Region[i])) {
    v <- c(v, i)
  }
}
print(h_2017$Country[v])
h_2017$Region[v[2]] <- "Eastern Asia"
h_2017$Region[v[3]] <- "Latin America and Caribbean"
h_2017$Region[v[4]] <- "Eastern Asia"
h_2017$Region[v[5]] <- "Sub-Saharan Africa"
h_2017$Region[v[6]] <- "Sub-Saharan Africa"
h_2017$Region[v[7]] <- "Sub-Saharan Africa"
print(is.na(h_2017$Region))


# check if there's any missing data
# missmap(h_2017, main = "Missing data for 2017", col = c('red', 'black'), legend = FALSE)
# missmap(h_2016, main = "Missing data for 2016", col = c('red', 'black'), legend = FALSE)
# missmap(h_2015, main = "Missing data for 2015", col = c('red', 'black'), legend = FALSE)

# looks like we're good to go!
hap <- rbind(h_2015, h_2016, h_2017)

# let's see how GDP per capita works with happiness scores
mean_hap_gdp <- h_2015 %>%
  group_by(Region) %>%
  summarise(Happiness.Score = mean(Happiness.Score),
            Economy..GDP.per.Capita. = mean(Economy..GDP.per.Capita.),
            Country = n())

ggplot(data = h_2015, aes(x = Happiness.Score, 
                          y = Economy..GDP.per.Capita.,
                          label = Country)) +
  geom_smooth(method = "lm") +
  labs(title = "Higher GDP per capita walks hand in hand with higher happiness scores. Dots = mean regional scores",
      y = "GDP per capita",
      x = "Happiness score") +
  geom_text(aes(colour = factor(Region)),
            size = 3,
            angle = 30,
            alpha = 0.7,
            check_overlap = TRUE) +
  geom_point(data = mean_hap_gdp, aes(colour = Region, size = Country), show.legend = FALSE) +
  theme_few() +
  theme(legend.title = element_blank())

# that's kinda depressing. How about life expectancy and health?
mean_hap_health <- h_2015 %>%
  group_by(Region) %>%
  summarise(Happiness.Score = mean(Happiness.Score),
            Health..Life.Expectancy. = mean(Health..Life.Expectancy.),
            Country = n())

ggplot(data = h_2015, aes(x = Happiness.Score, 
                          y = Health..Life.Expectancy.,
                          label = Country)) +
  geom_smooth(method = "lm") +
  labs(title = "Fitter = happier = more productive? Dots = mean regional scores",
       y = "Health & Life expectancy score",
       x = "Happiness score") +
  geom_text(aes(colour = factor(Region)),
            size = 3,
            angle = 30,
            alpha = 0.7,
            check_overlap = TRUE) +
  geom_point(data = mean_hap_health, aes(colour = Region, size = Country), show.legend = FALSE) +
  theme_few() + 
  theme(legend.title = element_blank())

# how happiness scores change over time
mean_world_changes <- hap %>%
  group_by(year) %>%
  summarise(Happiness.Score = mean(Happiness.Score),
            Happiness.Label = round(Happiness.Score, 3))

ggplot(data = mean_world_changes, aes(x = year, y = Happiness.Score, label = Happiness.Label)) +
  geom_bar(stat = "identity", aes(fill = factor(year)), show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6)) +
  geom_text(nudge_y = 0.2) +
  theme_few() +
  labs(title = "The world is getting less happy :(",
       x = "",
       y = "Mean happiness score")

mean_region_changes <- hap %>%
  group_by(year, Region) %>%
  summarise(Happiness.Score = mean(Happiness.Score),
            Happiness.Label = round(Happiness.Score, 3))

ggplot(data = mean_region_changes, aes(x = Region, y = Happiness.Score)) +
  geom_bar(stat = "identity", aes(fill = factor(Region))) +
  facet_grid(year ~ .) +
  scale_y_continuous(limits = c(0, 8)) +
  theme_few() +
  labs(title = "Happiness scores per region across 2015-2017",
       x = "",
       y = "Mean happiness score") +
  coord_flip() +
  theme(legend.title = element_blank())

mean_country_changes <- hap %>%
  group_by(Country) %>%
  summarise(Happiness.Score.Improved = ifelse(sum(diff(Happiness.Score)) > 0, 1, 0),
            Mean.Happiness.Score = mean(Happiness.Score))

# there's some missing data -- let's fix this so the biggest missing countries are displayed
missing <- map_world$region[is.na(map_world$happiness)]
missing_df <- data.frame(missing=missing) %>%
  group_by(missing) %>%
  count() %>%
  arrange(desc(n))
print(missing_df)

mean_country_changes$Country <- as.character(mean_country_changes$Country)
mean_country_changes <- data.frame(mean_country_changes)

mean_country_changes$Country[150] <- "UK"
mean_country_changes$Country[151] <- "USA"

map_world <- map_data(map = "world")
map_world$happiness <- mean_country_changes[match(map_world$region, mean_country_changes$Country), "Happiness.Score.Improved"]
ggplot() +
  geom_map(data = map_world, map = map_world, aes(map_id = region,
                                                  x = long,
                                                  y = lat,
                                                  fill = happiness),
           show.legend = FALSE) +
  scale_fill_gradientn(colours = c("blue", "green")) +
  labs(title = "Blue countries got less happy over time, while green countries improve. (Grey = no data)") +
  coord_equal() +
  theme_few()