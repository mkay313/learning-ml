# dataset: 

library(Amelia)
library(tidyverse)
library(caTools)

df_train <- read.csv("titanic_train.csv")
df_test <- read.csv("titanic_test.csv")

missmap(df_train, main = "Missing values in training data", col = c('red', 'black'), legend = FALSE)
missmap(df_test, main = "Missing values in test data", col = c('red', 'black'), legend = FALSE)
# loads of Age values missing in both train and test, single fare value missing from test
# we'll try to fill those out after some more analysis

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
    filter(Pclass == i)
  train_mean_ages[i] <- CalculateMeanAge(train_age_per_class$Age)
}

AddMissingAge <- function(ages, class, mean_ages) {
  this_age <- vector(mode = "numeric", length = length(ages))
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

test_df_set_age <- df_test %>%
  filter(!(is.na(Age)))

test_mean_ages <- vector(mode = "numeric", length = 3)
for (i in 1:3) {
  test_age_per_class <- test_df_set_age %>%
    filter(Pclass == i)
  test_mean_ages[i] <- CalculateMeanAge(test_age_per_class$Age)
}

df_test$Age <- AddMissingAge(df_test$Age, df_test$Pclass, test_mean_ages)

df_train <- df_train %>%
  select(-PassengerId, -Name, -Ticket, -Cabin)

df_train$Survived <- factor(df_train$Survived)
df_train$Pclass <- factor(df_train$Pclass)
df_train$SibSp <- factor(df_train$SibSp)
df_train$Parch <- factor(df_train$Parch)

log_model <- glm(Survived ~ ., family = binomial(link = "logit"),
                 data = df_train)
print(summary(log_model))

# first check the model fit
split <- sample.split(df_train$Survived, SplitRatio = 0.7)
final_train <- subset(df_train, split == TRUE)
final_test <- subset(df_train, split == FALSE)
final_log_model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = final_train)
print(summary(final_log_model))
fitted_probabilities <- predict(final_log_model, final_test, type = "response")
fitted_results <- ifelse(fitted_probabilities > 0.5, 1, 0)
summary(fitted_results)
misClassError <- mean(fitted_results != final_test$Survived)
print(table(final_test$Survived, fitted_probabilities > 0.5))

# df_test$Pclass <- factor(df_test$Pclass)
# df_test$SibSp <- factor(df_test$SibSp)
# 
# # there are 2 people with 9 Parch in test which leaves us with more factors than needed
# # lets replace those 9s with 6s
# df_test$Parch[df_test$Parch==9] <- 6
# df_test$Parch <- factor(df_test$Parch)
# 
# fitted_probabilities <- predict(log_model, df_test, type = 'response')
# fitted_results <- ifelse(fitted_probabilities > 0.5, 1, 0)
# print(summary(fitted_results))