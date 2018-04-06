library(tidyverse)
#dataset: https://www.kaggle.com/c/bike-sharing-demand (train and test combined into a single file)

bike <- read.csv("bikeshare.csv", header = TRUE)

plot_temp_count <- ggplot(data = bike, 
                          mapping = aes(x = temp, y = count, color = temp))
plot_temp_count + geom_point(alpha = 0.2) + 
  ggtitle("Count vs temperature")

bike$datetime_posixct <- as.POSIXct(bike$datetime)
plot_datetime_count <- ggplot(data = bike, 
                              mapping = aes(x = datetime_posixct, y = count, color = temp))
plot_datetime_count + 
  geom_point(aes(color = temp), alpha = 0.2) + 
  ggtitle("Count vs datetime") + 
  scale_color_continuous(low = "light blue", high = "orange")

cor(bike[,c('temp', 'count')])

plot_season_count <- ggplot(data = bike, 
                            mapping = aes(factor(season), y = count))
plot_season_count + 
  geom_boxplot(aes(color = factor(season))) + 
  ggtitle("Count vs season")

bike$hour <- format.Date(bike$datetime, "%H")

bike_working_days <- bike %>%
  filter(workingday == 1)
plot_working_hour_count <- ggplot(data = bike_working_days, 
                                  mapping = aes(x = hour, y = count))
plot_working_hour_count + 
  geom_point(aes(color = temp), alpha = 0.5) + 
  scale_color_gradientn(colors = c('purple', 'light blue', 'yellow', 'red')) +
  ggtitle("Count vs hours on working days")
  
bike_nonworking_days <- bike %>%
  filter(workingday == 0)
plot_nonworking_hour_count <- ggplot(data = bike_nonworking_days, 
                                     mapping = aes(x = hour, y = count))
plot_nonworking_hour_count + 
  geom_point(aes(color = temp), alpha = 0.5) + 
  scale_color_gradientn(colors = c('purple', 'light blue', 'yellow', 'red')) +
  ggtitle("Count vs hours on non-working days")

# predicting the changes in the rental numbers based off the temperature
# linear regression might not work too well as seen from exploratory analysis but let's try
# how about # of rentals at 25'C?
temp_model <- lm(formula = count ~ temp, data = bike)
predict(temp_model, data.frame(temp = c(25)))

bike$hour <- sapply(bike$hour, as.numeric)

model <- lm(count ~ . -casual -registered -datetime -atemp -datetime_posixct, bike)
summary(model)
