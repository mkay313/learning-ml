library(tidyverse)
library(e1071)

loans <- read.csv("loan_data.csv", sep=",")

print(summary(loans))
print(str(loans))

loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <- factor(loans$credit.policy)

# EDA
ggplot(data = loans, aes(x = fico)) + 
  geom_bar(aes(fill = not.fully.paid)) +
  ggtitle("Count of fico values. Blue = fully paid, red = not fully paid.")

ggplot(data = loans, aes(x = purpose)) +
  geom_bar(aes(fill = not.fully.paid), position = "dodge") +
  ggtitle("Count of loan purposes. Blue = fully paid, red = not fully paid.")

ggplot(data = loans, aes(x = int.rate, y = fico)) +
  geom_point(aes(fill = not.fully.paid), alpha = 0.3) +
  ggtitle("Interest rate vs FICO") +
  xlab("Interest rate") +
  ylab("FICO")

# SVM
split <- caTools::sample.split(loans, SplitRatio = 0.7)
loans.train <- subset(loans, split == TRUE)
loans.test <- subset(loans, split == FALSE)

loans.model <- svm(not.fully.paid ~ ., loans.train)
summary(loans.model)

loans.predict <- predict(loans.model, loans.test)
loans.predict <- as.data.frame(loans.predict)

table(loans.predict$loans.predict, loans.test$not.fully.paid)

# sad :( let's try tuning the model -- this is going to take a while :)))

# tune.loans <- tune(svm,
#                    train.x=not.fully.paid~., 
#                    data=loans.train,
#                    kernel='radial',
#                    ranges=list(cost=seq(0.1,10,1), gamma=c(0.1, 1, 2)))

# the line above gives us the cost of 0.1 and gamma of 0.1

loans.model.improved <- svm(not.fully.paid ~ ., loans.train, cost = 0.1, gamma = 0.1)
loans.predict.improved <- predict(loans.model.improved, loans.test)
loans.predict.improved <- as.data.frame(loans.predict.improved)

table(loans.predict.improved$loans.predict.improved, loans.test$not.fully.paid)

# dammit! the model performed the same ]:-<