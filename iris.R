library(ISLR)
library(caTools)
library(class)
library(ggplot2)

standardized_iris <- scale(iris[-5])
standardized_iris <- cbind(standardized_iris, iris[5])

split <- sample.split(standardized_iris$Species, SplitRatio = 0.7)
iris_train <- subset(standardized_iris, split == TRUE)
iris_test <- subset(standardized_iris, split == FALSE)

predicted_flower <- NULL
error_rate <- NULL

for (i in 1:10) {
  predicted_flower <- knn(iris_train[1:4], iris_test[1:4], iris_train$Species, i)
  error_rate[i] <- misClassError <- mean(iris_test$Species != predicted_flower)
}

k_values <- 1:10
error_df <- data.frame(error_values = error_rate, k_values = k_values)

print(ggplot(data = error_df, aes(x = k_values, y = error_values)) + geom_point() + geom_line())