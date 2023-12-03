########################
## In-class Activity  ##
########################
library(ISLR2)
library(glmnet)

## Adapt the code from shrinkage_methods to implement elastic net on the Hitters dataset. 
## You now have two tuning parameters (alpha and lambda) to tune. 
## (1) Report your optimal alpha and lambda from the training set. 
## (2) Report your test MSE for elastic net. 


Hitters = na.omit(Hitters)
x = model.matrix(Salary~.,data=Hitters)[,-1] 
#the [,-1] removes the intercept term. 
Y = Hitters$Salary


set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test=(-train)
Y.test = Y[test]

grid = 10^seq(10,-2,length=100)
alphas = seq(0.01, 0.99, .01)
cv_error = rep(NA, length(alphas))

for (i in 1:99){
  cv_elastic = cv.glmnet(x[train,],Y[train],alpha=alphas[i],lambda=grid)
  #cv.out = cv.glmnet(x[train,],Y[train],alpha = alphas[i], lambda = grid, nfolds=10) 
  #bestlambda = cv.out$lambda.min
  
  cv_error[i] = min(cv_elastic$cvm)
  #which.min(cv_error)
  
}
best_alpha = alpha[which.min(cv_error)]

# finding optimal lambda with best_alpha 
elastic_cv = cv.glmnet(x[train,], Y[train], alpha = best_alpha, lambda=grid)
best_lambda = elastic_cv$lambda.min 

enet.train = glmnet(x[train,], Y[train], alpha = best_alpha, lambda=grid)

enet.pred = predict(enet.train,s=best_lambda,newx=x[test,])
mean((enet.pred-Y.test)^2)





## Work in groups to come up with a solution. 
## Copy and paste your relevant code on Ed Discussion. 
## Please be sure to list all your group members names. Only one group member needs to post on Ed Discussion. 
## link: https://edstem.org/us/courses/42400/discussion/


