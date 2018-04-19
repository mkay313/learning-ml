library(tidyverse)

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

hap <- rbind(h_2015, h_2016, h_2017)