library(ISLR2)
library(tree)

head(Carseats)
# response is Sales 
# We will convert it to a binary variable

Carseats$High <- factor(ifelse(Carseats$Sales <=8, "No", "Yes"))
head(Carseats)

set.seed(6)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train, ]

########################
## In-class Activity  ##
########################
## 1. Build a classification tree for this new variables (High vs. Low Sales). 
## use gini index as your criteria. Obtain the training and test misclassification error. 
tree.carseats = tree(High~.-Sales,data=Carseats, subset=train, split='gini')
tree.carseats
plot(tree.carseats)
text(tree.carseats,pretty=0)


summary(tree.carseats)

tree.pred = predict(tree.carseats, newdata=Carseats.test, type='class')
mean(tree.pred!=Carseats.test$High) #test misclassification 

tree.pred = predict(tree.carseats, newdata=Carseats[train,], type='class')
mean(tree.pred!=Carseats[train,]$High) #training misclassification


## 2. How does the tree handle qualitative predictors? Does it create dummy variables? Explain. 
## does not create dummy variables, uses the categories as nodes or cutoffs 


## 3. Prune the tree. Plot the 10-fold CV error as a function of tree size. What is the optimal tree size?
## Note: you'll need to specify: cv.tree(...., FUN = prune.misclass)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass) #performs CV in on order to the determine the optimal level of tree complexity. 
cv.carseats
plot(cv.carseats$size,cv.carseats$dev,type='b')
which.min(cv.carseats$dev)

prune.carseats= prune.misclass(tree.carseats,best=6) #optimal size of 6 
plot(prune.carseats)
text(prune.carseats,pretty=0)



## 4. Report the test misclassification error for the pruned tree. Does it benefit from pruning? 
prune.pred = predict(prune.carseats, newdata=Carseats.test, type='class')
mean(prune.pred!=Carseats.test$High) #test misclassification 

## yes, much better 




## 5. As a group, discuss why single trees (even after pruning) might still be 
## subject to poor performance on the test set. As we'll see ensembling (or aggregating trees) will lead to superior
## performance. As a group, propose a natural way to aggregate trees and explain why this might help. 

## these trees don't do as well because they are grown from greedy algorithms, suboptimal splits 
## binary splits are very sensitive to change with different data, very overfit 

# take the averge of a whole bunch of different trees 




## Work in groups to come up with a solution. 
## Copy and paste your relevant code on Ed Discussion. 
## Please be sure to list all your group members names. Only one group member needs to post on Ed Discussion. 
## link: https://edstem.org/us/courses/42400/discussion/


