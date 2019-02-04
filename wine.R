# datasets: http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/

library(tidyverse)

df1 <- read.csv("winequality-red.csv", sep=";")
df2 <- read.csv("winequality-white.csv", sep=";")

df1$type <- rep("red", nrow(df1))
df2$type <- rep("white", nrow(df2))

wine <- rbind(df1, df2)
print(str(wine))

#EDA
ggplot(wine,aes(x=residual.sugar)) + 
  geom_histogram(aes(fill=type),color='black',bins=50) + 
  scale_fill_manual(values = c('red','white')) +
  ggtitle("Residual sugars count")

ggplot(wine, aes(x=citric.acid)) +
  geom_histogram(aes(fill=type), color='black',bins=50) +
  scale_fill_manual(values = c('red', 'white')) +
  ggtitle("Citric acid count")

ggplot(wine, aes(x=alcohol)) +
  geom_histogram(aes(fill=type), color="black", bins=50) +
  scale_fill_manual(values= c("red", "white")) +
  ggtitle("Alcohol content count")

ggplot(wine, aes(x = citric.acid, y = residual.sugar)) +
  geom_point(aes(color = type), alpha = 0.2) +
  scale_color_manual(values = c("red", "white")) +
  ggtitle("Residual sugar vs citric acid") +
  theme_dark()

ggplot(wine, aes(x= volatile.acidity, y = residual.sugar)) +
  geom_point(aes(color = type), alpha = 0.2) +
  scale_color_manual(values = c("red", "white")) +
  ggtitle("Residual sugar vs volatile acidity") +
  theme_dark()

# kmeans
clus.data <- wine[ , -13]
wine.cluster <- kmeans(clus.data, centers = 2, nstart = 20)
print(wine.cluster)

table(wine$type, wine.cluster$cluster)
