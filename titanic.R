# dataset: 

library(Amelia)
library(tidyverse)

df_train <- read.csv("titanic_train.csv")
df_test <- read.csv("titanic_test.csv")

missmap(df_train, main = "Missing values in training data", col = c('red', 'black'), legend = FALSE)
missmap(df_test, main = "Missing values in test data", col = c('red', 'black'), legend = FALSE)
# loads of Age values missing in both train and test, single fare value missing from test

# Survivors
ggplot(data = df_train, aes(Survived)) + 
  geom_bar(aes(fill = factor(Survived)))
# Ticket class
ggplot(data = df_train, aes(Pclass)) + 
  geom_bar(aes(fill = factor(Pclass)))
# Passenger sex
ggplot(data = df_train, aes(Sex)) + 
  geom_bar(aes(fill = factor(Sex)))
# Fare ranges
ggplot(data = df_train, aes(Fare)) + 
  geom_histogram(bins = 50, fill = 'yellow', color = 'black', alpha = 0.5)
# Age vs ticket class
ggplot(df_train, aes(Pclass, Age)) + 
  geom_boxplot(aes(group = Pclass, fill = factor(Pclass)), alpha = 0.4) +
  scale_y_continuous(breaks = seq(min(0), max(80), by = 2))

train_df_set_age <- df_train %>%
  filter(!(is.na(Age)))

CalculateMeanAge <- function(age) {
  return(round(mean(age),0))
}

train_mean_ages <- vector(mode = "numeric", length = 3)
for (i in 1:3) {
  train_age_per_class <- train_df_set_age %>%
    filter(train_df_set_age$Pclass == i)
  train_mean_ages[i] <- CalculateMeanAge(train_age_per_class$Age)
}

AddMissingAge <- function(ages, class, mean_ages) {
  this_ages <- vector(mode = "numeric", length = length(ages))
  for (i in 1:length(ages)) {
    if (is.na(ages[i])) {
      if (class[i] == 1) {
        this_age[i] <- mean_ages[1]
      } else if (class[i] == 2) {
        this_age[i] <- mean_ages[2]
      } else {
        this_age[i] <- mean_ages[3] }
    } else {
      this_age[i] <- ages[i]
    } 
  }
  return(this_age)
}

df_train$Age <- AddMissingAge(df_train$Age, df_train$Pclass, train_mean_ages)
