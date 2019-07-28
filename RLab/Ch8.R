library(tree)
set.seed(1)

# -------------------- 8.3.1 --------------------
library(ISLR)
attach(Carseats)

thresh = 8
High = ifelse(Sales>thresh, 'Yes', 'No')
Carseats = data.frame(Carseats, High)

tree.carseats = tree(High~.-Sales, data=Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.carseats

idx.train = sample(nrow(Carseats), nrow(Carseats) %/% 2)
train = Carseats[idx.train, ]
test = Carseats[-idx.train, ]
high.test = test$High

tree.carseats = tree(High~.-Sales, data=train)
high.pred = predict(tree.carseats, newdata=test, type='class')
table(high.pred, high.test)

(84+44)/length(high.test)     # correct prediction is 64%


cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow=c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type='b')
plot(cv.carseats$k, cv.carseats$dev, type='b')

par(mfrow=c(1, 1))
prune.carseats = prune.misclass(tree.carseats, best=8)
plot(prune.carseats)
text(prune.carseats, pretty=0)

high.pred = predict(prune.carseats, newdata=test, type='class')
table(high.pred, high.test)
(83 + 57) / length(high.test)    # correct prediction rate is 70%


par(mfrow=c(1, 1))
prune.carseats = prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats, pretty=0)

high.pred = predict(prune.carseats, newdata=test, type='class')
table(high.pred, high.test)
(84 + 44) / length(high.test)    # correct prediction rate is 64%



# -------------------- 8.3.2 --------------------
library(MASS)
attach(Boston)
set.seed(1)

idx.train = sample(nrow(Boston), nrow(Boston) %/% 2)
train = Boston[idx.train, ]
test = Boston[-idx.train, ]

tree.boston = tree(medv~., data=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty=0)

cv.boston = cv.tree(tree.boston)
par(mfrow=c(1, 2))
plot(cv.boston$size, cv.boston$dev, type='b')
plot(cv.boston$k, cv.boston$dev, type='b')

prune.boston = prune.tree(tree.boston, best=7)
par(mfrow=c(1,1))
plot(prune.boston)
text(prune.boston, pretty=0)

medv.pred = predict(tree.boston, newdata=test)
medv.test = test$medv
plot(medv.pred, medv.test)
abline(0, 1)

mean( (medv.pred - medv.test)^2 )    # the MSE is 35.28688


# -------------------- 8.3.3 --------------------
library(randomForest)
set.seed(1)

bag.boston = randomForest(medv~., data=train, mtry=13, importance=TRUE)
bag.boston

medv.pred = predict(bag.boston, newdata=test)
plot(medv.pred, medv.test)
abline(0, 1)
mean( (medv.pred - medv.test)^2 )   # MSE is 23.59273

bag.boston = randomForest(medv~., data=train, mtry=13, importance=TRUE, ntree=25)
medv.pred = predict(bag.boston, newdata=test)
mean( (medv.pred-medv.test)^2 )     # MSE is 22.53606


rf.boston = randomForest(medv~., data=train, mtry=6, importance=TRUE)
medv.pred = predict(rf.boston, newdata=test)
mean( (medv.pred-medv.test)^2 )     # MSE is 19.54087

importance(rf.boston)
varImpPlot(rf.boston)


# -------------------- 8.3.4 --------------------
library(gbm)
set.seed(1)

boost.boston = gbm(medv~., data=train, distribution='gaussian', n.trees=5000, interaction.depth=4)
summary(boost.boston)

par(mfrow=c(1, 2))
plot(boost.boston, i='rm')
plot(boost.boston, i='lstat')

medv.pred = predict(boost.boston, newdata=test, n.trees=5000)
mean( (medv.pred-medv.test)^2 )      # The MSE is 18.84709

boost.boston = gbm(medv~., data=train, distribution='gaussian', n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose=F)
medv.pred = predict(boost.boston, newdata=test, n.trees=5000)
mean( (medv.pred-medv.test)^2 )      # The MSE is 18.33455

