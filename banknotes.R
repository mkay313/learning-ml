# dataset: https://archive.ics.uci.edu/ml/datasets/banknote+authentication

library(neuralnet)
library(tidyverse)
library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)

df <- read.delim("data_banknote_authentication.txt", sep = ",")
colnames(df) <- c("Image.Var", "Image.Skew", "Image.Curt", "Entropy", "Class")
print(summary(df))
print(str(df))

split <- sample.split(df$Class, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

f <- as.formula(paste("Class ~ ", paste(colnames(df)[1:length(colnames(df))-1], collapse = " + ")))

nn <- neuralnet(f, train, hidden = 10, linear.output = FALSE)

predictions <- neuralnet::compute(nn, test[1:4])
predictions <- round(predictions$net.result, digits = 0)

table(predictions, test$Class)

# vs random forest
df$Class <- factor(df$Class)
split = sample.split(df$Class, SplitRatio = 0.70)

train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

model <- randomForest(f,data=train)
rf.pred <- predict(model,test)

table(rf.pred,test$Class)