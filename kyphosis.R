library(rpart)
library(rpart.plot)
library(randomForest)

### decision tree

print(str(kyphosis))

tree <- rpart(Kyphosis ~ ., method="class", data=kyphosis)
print(printcp(tree))

# plot(tree, uniform = TRUE, main = "Kyphosis tree")
# text(tree, use.n= TRUE, all = TRUE)

prp(tree)

### random forest
rf.model <- randomForest(Kyphosis ~ ., data = kyphosis)
print(rf.model)
print(rf.model$ntree) # number of trees in the model