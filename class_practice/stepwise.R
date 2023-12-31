## Forward and Backward Stepwise Selection

library(ISLR)
Hitters = na.omit(Hitters)

library(leaps)

# we can also use regsubsets() to perform backward/forward stepwise selection 

regfit.fwd = regsubsets(Salary~.,data=Hitters,nvmax=19, method="forward")
regfit.bwd = regsubsets(Salary~.,data=Hitters,nvmax=19, method="backward")

head(Credit)
regfit.fwd2 = regsubsets(Balance~.-ID-Ethnicity,data=Credit, nvmax=9, method="forward") 

regfit.fwd.sum = summary(regfit.fwd2)
names(regfit.fwd.sum)
n = dim(Credit)[1]
p = rowSums(regfit.fwd.sum$which) #number of predictors + intercept in the model 
adjr2 = regfit.fwd.sum$adjr2
cp = regfit.fwd.sum$cp
rss = regfit.fwd.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)

plot(AIC,type='b')
plot(BIC,type='b')
plot(cp,type='b')
plot(adjr2,type='b')

# we can also use a function called stepAIC()

model0 = lm(Balance~1,data=Credit)
summary(model0)

modelfull = lm(Balance~.,data=Credit)
summary(modelfull)

# stops when AIC stops decreasing 
library(MASS)
?stepAIC

stepAIC(model0,scope=list(lower=model0,upper=modelfull),direction="forward")

stepAIC(modelfull,scope=list(lower=model0,upper=modelfull),direction="backward")

stepAIC(modelfull,scope=list(lower=model0,upper=modelfull),direction="both")
