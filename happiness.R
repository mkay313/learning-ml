library(tidyverse)
library(Amelia)
library(ggthemes)

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
missmap(h_2017, main = "Missing data for 2017", col = c('red', 'black'), legend = FALSE)
missmap(h_2016, main = "Missing data for 2016", col = c('red', 'black'), legend = FALSE)
missmap(h_2015, main = "Missing data for 2015", col = c('red', 'black'), legend = FALSE)

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
