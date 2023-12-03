##############################
### Qualitative predictors ###
##############################

library(ISLR2)
head(Credit)
str(Credit)

summary(lm(Balance~Limit+Region,data=Credit))

#force to be factor if not already 
#summary(lm(Balance~Limit+as,factor(Region),data=Credit))


# What are the fitted regression lines for each category of region? 
# South: yhat = -307 + 0.17*Limit + 13.80 
# West: yhat = -307 + 0.17*Limit + 28.35 
# East: yhat = -307 + 0.17*Limit 


summary(lm(Balance~Limit,data=Credit))

## Can change baseline:
Credit$Region <- relevel(Credit$Region, ref = "South")

summary(lm(Balance~Limit+Region,data=Credit))

lm.fit <- lm(Balance~Limit+Region+Student+Married,data=Credit)
lm.fit2 <- lm(Balance~Limit+Student+Married,data=Credit)

# partial F-test 
# have to assume that errors are normal 
anova(lm.fit2, lm.fit)
#large p value suggests that adding region does not improve model 


##############################
##### Multicollinearity ######
##############################
library(MASS)
set.seed(6)
n=100;

# simulated 2 highly correlated predictors 
Covmat = matrix(0.88,3,3);
diag(Covmat)=1;
betas=c(1,2,3);
X=matrix(0,n,3);
y=numeric(n);
for(i in 1:n){
  X[i,]<- mvrnorm(1,rep(0,3),Covmat);
  y[i]<-sum(X[i,]*betas)+8*rnorm(1);
}


X1 = X[,1]
X2 = X[,2]
X3 = X[,3]

# model 
lm1 = lm(y~X1+X2+X3)

summary(lm1)

# small changes to the first 5 observations of X1
# everything else stays the same
set.seed(12)
X1new = X1
X1new[1:5] = X1[1:5] + rnorm(5)
summary(lm(y~X1new+X2+X3))

######## More than one way to diagnose multicollinearity ########

##### simulation illustrating perfect correlation 

x1 = rnorm(100, mean=70, sd=15)
x2 = rnorm(100, mean=70, sd=15)

# Add in a linear combination of X1 and X2
x3 = (x1+x2)/2

# X4 is somewhat correlated with X1 but not relevant to Y
x4 = x1+runif(100,min=-100,max=100)

# Y is a linear combination of X1 and X2 plus noise
y = 0.7*x1 + 0.3*x2 + rnorm(100, 0, sqrt(15))

summary(lm(y~x1+x2+x3+x4))
summary(lm(y~x1+x2+x4))

X = cbind(rep(1,100),x1,x2,x3,x4)
# what do we expect the rank to be if X is full rank? 
qr(X)$rank

eigenvaluesX = eigen(t(X)%*%X)$values
eigenvaluesX

#install.packages("car")
library(car)

vif(lm(y~x1+x2+x3+x4))

#######################
#### data example #####
#######################

library(ISLR2)
head(Credit)

fit1 = lm(Balance~Age+Limit,data=Credit)
summary(fit1)

fit2 = lm(Balance~Rating+Limit,data=Credit)
summary(fit2)

plot(Rating~Limit,Credit)

## check rank of your design matrix X
n = dim(Credit)[1]
X = cbind(rep(1,n),Credit$Rating,Credit$Limit)
qr(X)$rank

## check eigenvalues of X^TX
eigenvaluesX = eigen(t(X)%*%X)$values
eigenvaluesX

vif(fit2)


########################
## In-class Activity  ##
########################

## Download the insurance.csv file on Canvas. 
## You can load your data into R using: 
insurance=read.csv("/Users/jillianeglandschool/Desktop/DS303/insurance.csv") 

## Specify your pathway to be where you saved this file. 

## 1. This data set contains a few categorical predictors. 
## Check that all the categorical predictors in our dataset are stored correctly using str()
## If they are not, fix it. Copy and paste your output here. 
insurance$gender = as.factor(insurance$gender)
insurance$smoker = as.factor(insurance$smoker)
insurance$region = as.factor(insurance$region)
str(insurance)


## 2. Fit a model with the response (Y) as health care charges and predictors
## x_1 age, x2 = bmi, and x3 = gender. 
## Based on your output, write out the fitted model for males only (gendermale = 1) and 
## write out the fitted model for females only (gendermale = 0). 
full_model = lm(charges~age+bmi+gender,data=insurance)
full_model_sum = summary(full_model)
# males: -5642.36 + 243.19*age + 327.54*bmi 
# females: -6986.82 + 243.19*age + 327.54*bmi 



## 3. Your classmate tells you that including gender as a dummy variable in the model is not necessary. 
## Instead you can just fit a model for males only and a separate model for females only.
## Your classmate claims this is approach is equivalent to what you did in part 2. 


## To see whether or not your classmate's approach makes sense, 
## subset your data into two groups: data for males and data for females. 
## Fit a model with bmi and age for the male group only. Call this model fit_males. 
## Now do the same for the female group. Call this model fit_females. 
## Based on your output, write out both model's estimated regression coefficients.
insurance_m = insurance[insurance$gender=="male",]
insurance_f = insurance[insurance$gender=="female",]
model_m = lm(charges~age + bmi, data=insurance_m)
model_f = lm(charges~age + bmi, data=insurance_f)
sum_m = summary(model_m)
sum_f = summary(model_f)

# males: -8012.79 + 238.63*age + 409.87*bmi 
# females: -4515.22 + 246.92*age + 241.32*bmi 


## 4. Compare your results in part 2 with part 3. Are they equivalent? 
## Explain in plain language to your classmate 
## why these two approaches will not give the same results. 

# ignores the interaction between gender and the other variables 
# first assumes that bmi and age have the same effect on charges for both males and females (same slope) 
# adding interactions will allow the slopes to be different for the variables 

fit = lm(charges~age + bmi + gender + age*gender + bmi*gender, data=insurance)
summary(fit)
# will results in same as subsetting data by gender 



