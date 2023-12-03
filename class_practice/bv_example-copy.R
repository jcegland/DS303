library(MASS)

m1 = lm(medv~lstat,data=Boston)
m5 = lm(medv~poly(lstat,5,raw=TRUE),data=Boston)
m9 = lm(medv~poly(lstat,9,raw=TRUE),data=Boston)

par(mfrow=c(1,1))
plot(medv~lstat,data=Boston)
points(Boston$lstat,m1$fitted.values,col='red')
## m1$fitted values are the predicted values we obtain from the model m1

plot(medv~lstat,data=Boston)
points(Boston$lstat,m5$fitted.values,col='green')
## m5$fitted values are the predicted values we obtain from the model m5

plot(medv~lstat,data=Boston)
points(Boston$lstat,m9$fitted.values,col='blue')
## m9$fitted values are the predicted values we obtain from the model m9

set.seed(13)
n = dim(Boston)[1]
train_index = sample(1:n,n/2,replace=F)
train_boston = Boston[train_index,]
test_boston = Boston[-train_index,]

#### fit 9 models of increasing complexity on the training set:
M1 = lm(medv~poly(lstat,1,raw=TRUE),data=train_boston)
M2 = lm(medv~poly(lstat,2,raw=TRUE),data=train_boston)
M3 = lm(medv~poly(lstat,3,raw=TRUE),data=train_boston)
M4 = lm(medv~poly(lstat,4,raw=TRUE),data=train_boston)
M5 = lm(medv~poly(lstat,5,raw=TRUE),data=train_boston)
M6 = lm(medv~poly(lstat,6,raw=TRUE),data=train_boston)
M7 = lm(medv~poly(lstat,7,raw=TRUE),data=train_boston)
M8 = lm(medv~poly(lstat,8,raw=TRUE),data=train_boston)
M9 = lm(medv~poly(lstat,9,raw=TRUE),data=train_boston)

## training MSE
m1tr = mean((train_boston$medv - M1$fitted.values)^2)
m2tr = mean((train_boston$medv - M2$fitted.values)^2)
m3tr = mean((train_boston$medv - M3$fitted.values)^2)
m4tr = mean((train_boston$medv - M4$fitted.values)^2)
m5tr = mean((train_boston$medv - M5$fitted.values)^2)
m6tr = mean((train_boston$medv - M6$fitted.values)^2)
m7tr = mean((train_boston$medv - M7$fitted.values)^2)
m8tr = mean((train_boston$medv - M8$fitted.values)^2)
m9tr = mean((train_boston$medv - M9$fitted.values)^2)
training = c(m1tr, m2tr, m3tr, m4tr, m5tr, m6tr, m7tr, m8tr, m9tr)
plot(training, main = "Training data", ylab = "MSE", xlab = "Model poly")

# Test MSE
m1ts = mean((test_boston$medv - predict(M1,newdata=test_boston))^2)
m2ts = mean((test_boston$medv - predict(M2,newdata=test_boston))^2)
m3ts = mean((test_boston$medv - predict(M3,newdata=test_boston))^2)
m4ts = mean((test_boston$medv - predict(M4,newdata=test_boston))^2)
m5ts = mean((test_boston$medv - predict(M5,newdata=test_boston))^2)
m6ts = mean((test_boston$medv - predict(M6,newdata=test_boston))^2)
m7ts = mean((test_boston$medv - predict(M7,newdata=test_boston))^2)
m8ts = mean((test_boston$medv - predict(M8,newdata=test_boston))^2)
m9ts = mean((test_boston$medv - predict(M9,newdata=test_boston))^2)
test = c(m1ts, m2ts, m3ts, m4ts, m5ts, m6ts, m7ts, m8ts, m9ts)
plot(test, main="Test data", ylab = "MSE", xlab = "Model poly")



###########################
#### In-class Activity ####
###########################

#### fit 9 models of increasing complexity on the training set: 
##M1 = lm(medv~poly(lstat,1,raw=TRUE),data=train_boston)
##M2 = lm(medv~poly(lstat,2,raw=TRUE),data=train_boston)
##M3 = lm(medv~poly(lstat,3,raw=TRUE),data=train_boston)
##M4 = lm(medv~poly(lstat,4,raw=TRUE),data=train_boston)
##M5 = lm(medv~poly(lstat,5,raw=TRUE),data=train_boston)
##M6 = lm(medv~poly(lstat,6,raw=TRUE),data=train_boston)
##M7 = lm(medv~poly(lstat,7,raw=TRUE),data=train_boston)
##M8 = lm(medv~poly(lstat,8,raw=TRUE),data=train_boston)
##M9 = lm(medv~poly(lstat,9,raw=TRUE),data=train_boston)

## obtain the training MSE and test MSE for each of these models. 
## Create a plot where the training MSE is on the y-axis and the 
## model complexity (1 - 9) is on the x-axis. 
## Create a similar plot for the test MSE. 

## Work in groups to come up with a collective solution. 
## Copy/paste your code and plots into Ed Discussion 
## Please make sure you set.seed(13) so that we all get comparable results!
## Please be sure to list all your group members names so that everyone gets participation credit. 
## Only one group member needs to post on Ed Discussion. 

