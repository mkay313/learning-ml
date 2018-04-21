library(ISLR) # Caravan package

print(any(is.na(Caravan)))
print(var(Caravan[,1]))
print(var(Caravan[,2]))

purchase <- Caravan$Purchase

standardized_Caravan <- scale(Caravan[-86])

# Train Test Split

test_index <- 1:1000
test_data <- standardized_Caravan[test_index, ]
test_purchase <- purchase[test_index]

# Train
train_data <- standardized_Caravan[-test_index, ]
train_purchase <- purchase[-test_index]

###############
# KNN MODEL
###############
library(class)
# set.seed(101)
# 
# predicted_purchase <- knn(train_data, test_data, train_purchase, k=5)
# misClassError <- mean(test_purchase != predicted_purchase)
# print(misClassError)

##########
# choosing a k value
##########

predicted_purchase <- NULL
error_rate <- NULL

for (i in 1:20) {
  set.seed(101)
  predicted_purchase <- knn(train_data, test_data, train_purchase, k=i)
  error_rate[i] <- mean(test_purchase != predicted_purchase)
}

print(error_rate)

#### Visualize K elbow method

library(ggplot2)
k_values <- 1:20
error_df <- data.frame(error_rate, k_values)