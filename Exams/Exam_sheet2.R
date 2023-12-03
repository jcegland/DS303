# read in data 
spam = read.csv("/Users/jillianeglandschool/Desktop/DS303/spambase.data",header=FALSE)

# generate random data 
x1 = sample(16)
n = dim(data)[1]
train_index = sample(1:n,n/2,replace=F)
train = data[train_index,]
test = data[-train_index,] 

# testing for linearity 
m1 = lm(medv~.,data=Boston)
par(mfrow=c(2,2))


#### Qualitative Predictors/Interactions ##### 
# MLR_OtherConsiderations_Part1.R 
# ex from HW5 pt 4 
# ex from HW6 pt 1 
str(data) # to check if stored as factors 
fit = lm(Balance~Income + Student, data=Credit) # normal fit, assumes same slope for each category 
new_fit = lm(Balance ~ Income + Student + Income:Student, data=Credit) # interaction between predictors to allow for differing slopes 
summary(new_fit)


#### Multicollinearity #### 
# MLR_OtherConsiderations_Part1.R 
# effects seen in HW5 pt 3 
# effects seen in HW6 pt 2
cor(x1,x2) 


##### Regularized Regression ##### 
# HW6 pt 3 
### Ridge Regression ### 
# shrinkage_methods.R 
grid = 10^seq(10,-2,length=100)
ridge_model = glmnet(x,Y,alpha=0, lambda=grid)

ridge_model$lambda[50]
coef(ridge_model)[,50] #estimated regression coefficients for each value of lambda
sqrt(sum(coef(ridge_model)[-1, 50]^2))

## obtain the ridge regression coefficients for a new value of lambda, say 50: 
# s is lambda 
predict(ridge_model, s = 50, type = 'coefficients')[1:20,]

## selecting tuning parameter 
cv.out = cv.glmnet(x[train,],Y[train],alpha = 0, lambda = grid, nfolds=10) 
plot(cv.out)
bestlambda = cv.out$lambda.min
ridge.pred = predict(ridge.train,s=bestlambda,newx=x[test,])
mean((ridge.pred-Y.test)^2)

ridge.pred = predict(ridge.train,s=bestlambda,newx=x[test,])
mean((ridge.pred-Y.test)^2)

final = glmnet(x,Y,alpha=0,lambda = bestlambda)
coef(final)

### lasso regression ### 
cv.out.lasso = cv.glmnet(x[train,],Y[train],alpha = 1, lambda = grid) 
#default performs 10-fold CV, but you can change this using the argument `nfolds` 
plot(cv.out.lasso)
bestlambda2 = cv.out.lasso$lambda.min
bestlambda2

lasso.train = glmnet(x[train,],Y[train],alpha=1,lambda=grid)

which(grid==bestlambda2)
lasso.train$lambda[77]
coef(lasso.train)[,77]

lasso.pred = predict(lasso.train,s=bestlambda2,newx=x[test,])
mean((lasso.pred-Y.test)^2)

final.lasso = glmnet(x,Y,alpha=1,lambda=bestlambda2)
coef(final.lasso)


#### Bootstrap #### 
# bootstrap.R 
# HW7 pt 3 

## bootstrap standard errors
n = dim(data)[1]
B = 2000
beta_0 = rep(0,2000)
for(b in 1:B){
  index = sample(1:n,n,replace=TRUE)
  bootsample = Auto[index,]
  fit = lm(mpg~horsepower,data=bootsample)
  beta_0[b] = coef(fit)[1]
}

sqrt(sum((beta_0-mean(beta_0))^2)/(B-1))

# compare with analytical formulas
summary(lm(mpg~horsepower,data=Auto))

## bootstrap Mu 
mean(Boston$medv) 
n = dim(Boston)[1]
set.seed(1)
B = 2000
mu_list = rep(0,B)
for(b in 1:B){
  index = sample(1:n,n,replace=TRUE)
  bootsample = Boston[index,]
  mu_list[b] = mean(bootsample$medv)
} 

se_boot = sqrt(sum((mu_list-mean(mu_list))^2)/(B-1)) 
se_boot 

sd(Boston$medv)/sqrt(n)

## bootstrap confidence intervals 
# see bootstrap.R 


#### Logistic Regression #### 
glm.fit = glm(Direction~Lag2, data=train_weekly, family='binomial')
#summary(glm.fit)
glm.prob = predict(glm.fit,test_weekly,type='response') 
glm.pred = rep('Down',nrow(test_weekly))
glm.pred[glm.prob >0.5] ='Up'
table(glm.pred,test_weekly$Direction)
mean(glm.pred == test_weekly$Direction)


#### LDA #### 
lda.fit = lda(Direction~Lag2,data=train_weekly)
lda.pred = predict(lda.fit,test_weekly)
table(lda.pred$class,test_weekly$Direction)
mean(lda.pred$class==test_weekly$Direction)


#### QDA ####
qda.fit = qda(Direction~Lag2,data=train_weekly)
qda.pred = predict(qda.fit,test_weekly)
table(qda.pred$class,test_weekly$Direction)
mean(qda.pred$class==test_weekly$Direction)


#### Naive Bayes #### 
nb.fit = naiveBayes(Direction~Lag2, data=train_weekly)
#nb.fit

nb.class = predict(nb.fit, test_weekly)
table(nb.class, test_weekly$Direction)
mean(nb.class == test_weekly$Direction) 






