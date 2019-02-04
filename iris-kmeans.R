library(ISLR)
library(tidyverse)
library(cluster)

ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point(size = 4)

irisCluster <- kmeans(iris[,1:4], centers = 3, nstart = 20)
print(irisCluster)

table(irisCluster$cluster, iris$Species)

clusplot(iris, irisCluster$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0)
