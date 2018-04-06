#dataset: https://www.kaggle.com/c/bike-sharing-demand (train and test combined into a single file)

bike <- read.csv("bikeshare.csv", header = TRUE)

plot_temp_count <- ggplot(data = bike, mapping = aes(x = temp, y = count, color = temp))
plot_temp_count + geom_point(alpha = 0.2) + ggtitle("Count vs temperature")

bike$datetime_posixct <- as.POSIXct(bike$datetime)
plot_datetime_count <- ggplot(data = bike, mapping = aes(x = datetime_posixct, y = count, color = temp))
plot_datetime_count + geom_point(alpha = 0.2) + ggtitle("Count vs datetime")
