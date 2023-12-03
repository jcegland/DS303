x=c(-2,5,-1,10,6)
y=c('red', 'blue', 'red', 'blue', 'blue')
y=c(0,1,0,1,1)


data = data.frame(x,y)
str(data)


glm.fit = glm(y~x, data=data,family='binomial')
# Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred
# too small sample size and # of predictors 
# means that there is no randomness, perfect split line to separate data 
# no unique solution 
# no convergence

summary(glm.fit)
head(glm.fit$fitted.values)

library(ggplot2)

ggplot(data, aes(x=x, y=y, color=y)) + geom_point(size=2)


# P(y2=blue|x2=5, B0, B1) = 1 = exp(beta0+beta1*5)/(1+exp(beta0+beta1*5)) 
# want to get as close to 1 as possible 
beta0 = 0 
beta1 = 100 # could be infinity 

exp(beta0+beta1*5)/(1+exp(beta0+beta1*5))





