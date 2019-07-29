library(randomForest)
library(tree)
library(ISLR)
attach(Carseats)
set.seed(1)


# ------------------ (a) ------------------
idx.train = sample(nrow(Carseats), nrow(Carseats)%/%2)
train = Carseats[idx.train, ]
test = Carseats[-idx.train, ]


# ------------------ (b) ------------------
tree.fit = tree(Sales~., data=train)
tree.pred = predict(tree.fit, newdata=test)
tree.mse = mean( (tree.pred-test$Sales)^2 )    # MSE = 4.922039

plot(tree.fit)
text(tree.fit, pretty=0)


# ------------------ (c) ------------------
cv.fit = cv.tree(tree.fit, FUN=prune.tree)
par(mfrow=c(1, 2))
plot(cv.fit$size, cv.fit$dev, type='b', xlab='tree size', ylab='deviance')
plot(cv.fit$k, cv.fit$dev, type='b', xlab='tuning parameter', ylab='deviance')

# we choose size=18
prune.fit = prune.tree(tree.fit, best = 18)
par(mfrow=c(1,1))
plot(prune.fit)
text(prune.fit, pretty=0)
prune.pred = predict(prune.fit, newdata=test)
prune.mse = mean( (prune.pred-test$Sales)^2 )    # MSE = 4.922039

# Pruning the tree does not improve the test MSE in this case


# ------------------ (d) ------------------
bag.fit = randomForest(Sales~., data=train, mtry=10, importance=TRUE)
bag.pred = predict(bag.fit, newdata=test)
bag.mse = mean( (bag.pred-test$Sales)^2 )    # MSE = 2.657296

importance(bag.fit)
varImpPlot(bag.fit)

# Price and ShelveLoc are the most important 2 factors


# ------------------ (e) ------------------
m.vec = 1:10
mse.vec = rep(0, max(m.vec))
for(m in m.vec){
    rf.fit = randomForest(Sales~., data=train, mtry=m, importance=TRUE)    
    rf.pred = predict(rf.fit, newdata=test)
    mse.vec[m] = mean( (rf.pred-test$Sales)^2 )
}

plot(m.vec, mse.vec, type='b', xlab='m', ylab='mse')
# The m that leads to the minimum mse is 7
# The minimum mse is 2.599039