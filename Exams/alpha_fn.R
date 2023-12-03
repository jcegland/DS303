alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))}

## run this on Portfolio data
library(ISLR2)
alpha.fn(Portfolio,1:100)

# 1(1) 
# training MSE will decrease from 0 as s gets bigger 
# test MSE will be u-shape 

# 1 (2) 
# perfectly separated data 
# coefficient cannot converge because the maximum likelihod maxed at infinity 

# 1 (3) 
# false, cannot calculate bias and variance without the true population 

# 1 (4) 
# false, the training MSE for for ridge will be smaller than lasso, 
# ridge better when want all predictors 

# 1 (5) 
# true, QDA does mimic Bayes and assumes normal distribution 

# 1 (6) 
# true, multicollinearity does cause problems because 
# we can take the inverse but it is not full rank, so 
# the classifier gets really large 
# if perfectly correlated, cannot take inverse 


# simulation 1 
# finding bias for ridge given true Y 
library (glmnet)
error = rnorm(100, 0, 1) 
n = 100
X1 = seq(0,10,length.out =100) #generates 100 equally spaced values from 0 to 10.
X2 = runif(100) #generates 100 uniform values. 
Y = 2 + 3*X1 + 5*log(X2) + error 
lambda = 2 
x=cbind(X1, X2)

B=100
beta0=beta1=beta2=rep(NA,B)
for (b in 1:B){
  error = rnorm(100, 0, 1) 
  Y = 2 + 3*X1 + 5*log(X2) + error 
  ridge_model = glmnet(x,Y, alpha=0, lambda=2) 
  beta0[b] = coef(ridge_model)[1] 
  beta1[b] = coef(ridge_model)[2]
  beta2[b] = coef(ridge_model)[3] 
  
}
mean(beta0)-1
mean(beta1)-3
mean(beta2)-5



# simulation 2 
n = dim(Portfolio)[1]

alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))}

## bootstrap standard errors
B = 2000
alpha = rep(0,B)
for(b in 1:B){
  index = sample(1:n,n,replace=TRUE)
  #bootsample = Portfolio[index,]
  #alpha[b] = alpha.fn(bootsample, 1:100)
  alpha[b] = alpha.fn(Portfolio[index,], 1:100)
}

sqrt(sum((alpha-mean(alpha))^2)/(B-1))



