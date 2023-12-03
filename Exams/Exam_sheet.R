

# generating data 
x = matrix(NA,1000,150)
for(i in 1:150){
  x[,i] = rnorm(1000)
}
y = rnorm(1000)
data = as.data.frame(cbind(y,x))


#plotting linear model 
m1 = lm(medv~lstat,data=Boston)
m5 = lm(medv~poly(lstat,5,raw=TRUE),data=Boston)


# extracting p values 
p_values = summary(fit)$coefficients[,4] 
length(which(p_values<0.05))



# train/test split 
set.seed(13)
n = dim(Boston)[1]
train_index = sample(1:n,n/2,replace=F)
train_boston = Boston[train_index,]
test_boston = Boston[-train_index,] 

# training/test MSE 
m1tr = mean((train_boston$medv - M1$fitted.values)^2) 
m1ts = mean((test_boston$medv - predict(M1,newdata=test_boston))^2)

# MSE: (mean squared error) avg squared difference between observation and prediction 
# expected test MSE: true test MSE of an infinite number of training sets 
# training MSE: computed using training data, evaluates discrepancy 
  #between observed response and predicted value in training set 
# bias: the error introduced by approximating our real-life phenomenon, 
  #diff between true and trained model, any deviation from prediction and truth 
# variance: the amount by which our model would change if we estimated 
  #it using a different training set 
# irreducible error: inherent noise and randomness observed in Y, cannot be reduced 


# simulation for unbiased betas 
B=5000 
beta0hat = beta1hat = rep(NA, B) 

for (i in 1:B){
  error = rnorm(n,0,1)
  Y = beta_0 + beta_1*X1 + error  
  fit = lm(Y~X1) 
  
  beta0hat[i] = fit$coefficients[[1]]
  beta1hat[i] = fit$coefficients[[2]]
  
}
mean(beta0hat)
mean(beta1hat)


# simulation for multiple testing 
  ## multiple testing.R 


# prediction/confidence intervals 
# prediction: predict, accounts for irreducible, wider 
predict(model,newdata=Xh,interval='prediction',level=0.95)
# confidence: estimate: only accounts for reducible, narrower 
predict(model,newdata=Xh,interval='confidence',level=0.95)


# regsubsets subset selection, when p<30 
regfit = regsubsets(Salary~.,data=Hitters,nbest=1,nvmax=19)

# AIC/BIC 
n = dim(Hitters)[1]
p = rowSums(regfit.sum$which)
adjr2 = regfit.sum$adjr2
cp = regfit.sum$cp
rss = regfit.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)
which.min(BIC) 
coef(regfit,6) 


# cross validation 
M1 = lm(mpg~horsepower,data=train)
M2 = lm(mpg~poly(horsepower,2), data=train)

M1_mpg = predict(M1,newdata=test[,-1]) # -1 to remove the response variable 
M2_mpg = predict(M2,newdata=test[,-1])

mean((test$mpg - M1_mpg)^2)
mean((test$mpg - M2_mpg)^2)



# LOOCV 
MSE_M1 = MSE_M2 = rep(0,n)
for(i in 1:n){
  test = Auto[i,]
  train = Auto[-i,]
  
  M1 = lm(mpg~horsepower,data=train)
  M2 = lm(mpg~poly(horsepower,2),data=train)
  
  M1_mpg = predict(M1,newdata=test)
  M2_mpg = predict(M2,newdata=test)
  
  MSE_M1[i] = (test$mpg - M1_mpg)^2
  MSE_M2[i] = (test$mpg - M2_mpg)^2
}

mean(MSE_M1)
mean(MSE_M2)


# k folds 
folds = sample(1:k,nrow(Auto),replace=TRUE)

MSE_M1 = MSE_M2 = rep(NA,k)
for(i in 1:k){
  train = Hitters[folds==j,]
  test = Hitters[folds!=j,] 
  
  M1 = lm(mpg~horsepower,data=train)
  M2 = lm(mpg~poly(horsepower,2),data=train)
  
  M1_mpg = predict(M1,newdata=test[,-1])
  M2_mpg = predict(M2,newdata=test[,-1])
  
  MSE_M1[i]=mean((test$mpg - M1_mpg)^2)
  MSE_M2[i]=mean((test$mpg - M2_mpg)^2)
  
}


# k folds and subset 
###in cv_subset.R 


# finding predicted values after using regsubsets 
best.train = regsubsets(Salary~.,data=train,nbest=1,nvmax=19)
# finds test MSE's 
val.errors = rep(NA,19)
for(i in 1:19){
  test.mat = model.matrix(Salary~.,data=test)
  
  coef.m = coef(best.train,id=i)
  
  pred = test.mat[,names(coef.m)]%*%coef.m
  val.errors[i] = mean((test$Salary-pred)^2)
}
which.min(val.errors)
coef(best.train,id=11)


# stepwise selection 
regfit.fwd = regsubsets(Salary~.,data=Hitters,nvmax=19, method="forward")
regfit.bwd = regsubsets(Salary~.,data=Hitters,nvmax=19, method="backward") 
## more in stepwise.R if needed 




