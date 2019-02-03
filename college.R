library(ISLR)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)

data("College")
df <- College

# exploration

# scatterplot of Grad.Rate vs Room.Board coloured by the Private column
ggplot(data = df, aes(x = Room.Board, y = Grad.Rate)) + 
  geom_point(aes(color=factor(Private))) + 
  ggtitle("Grad rate vs Room board. Blue = private, red = not private.") + 
  xlab("Room board") + 
  ylab("Grad rate") +
  theme(legend.title = element_blank())

# histogram of full time undergrad students, color by Private
ggplot(data = df, aes(x = F.Undergrad)) + 
  geom_histogram(aes(fill = factor(Private))) +
  ggtitle("Full-time undergrad students count. Blue = private, red = not private") +
  xlab("No. of fulltime undergrads") +
  ylab("Count") +
  theme(legend.title = element_blank())

# histogram of Grad.Rate colored by Private
ggplot(data = df, aes(x = Grad.Rate)) +
  geom_histogram(aes(fill = factor(Private))) +
  ggtitle("Graduation rates count. Blue = private, red = not private") +
  xlab("Graduation rate") +
  ylab("Count") +
  theme(legend.title = element_blank())

print(df['Grad.Rate' >100])
df['Cazenovia College','Grad.Rate'] <- 100

split <- caTools::sample.split(df$Private, SplitRatio = 0.7)
df.train <- subset(df, split == TRUE)
df.test <- subset(df, split == FALSE)

tree <- rpart(Private ~ ., data = df.train)
print(printcp(tree))
print(prp(tree))

predictions <- predict(tree, df.test)
print(head(predictions))
predictions <- as.data.frame(predictions)

predictions <- predictions %>% 
  transmute(isPrivate = ifelse(Yes > No, TRUE, FALSE))

print(table(predictions$isPrivate,df.test$Private))

rf.model <- randomForest(Private ~ ., data = df.train, importance = TRUE)
print(rf.model)

print(rf.model$confusion)
print(rf.model$importance)

predictions.rf <- predict(rf.model, df.test)
predictions.rf <- as.data.frame(predictions.rf)

print(table(predictions.rf$predictions.rf, df.test$Private))
