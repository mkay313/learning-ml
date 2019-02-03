library(ISLR)
library(e1071)

model <- svm(Species ~ ., data= iris)
print(summary(model))

tune.results <- tune(svm, 
                     train.x = iris[1:4], 
                     train.y = iris[,5], 
                     kernel = 'radial',
                     ranges = list(cost = seq(0.1, 10, 0.1), 
                                   gamma = seq(0.1, 3, 0.1)))

summary(tune.results)

# optimal: cost 2.2, gamma 0.1
tuned.svm = svm(Species ~ ., 
                data = iris, 
                kernel = 'radial', 
                cost=2.2, 
                gamma=0.1)

summary(tuned.svm)