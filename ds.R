library(MASS)
library(caTools)
library(tree)
data(Boston)

names(Boston)

attach(Boston)


set.seed(0819)
split = sample.split(medv, SplitRatio = 0.8)
training_set = subset(Boston, split == TRUE)
test_set = subset(Boston, split == FALSE)




regressor = tree(formula = medv ~ lstat+rm, data = training_set)

y_pred = predict(regressor,newdata = test_set)
mse = rmse(y_pred,test_set$medv)
