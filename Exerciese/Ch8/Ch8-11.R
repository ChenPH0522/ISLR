library(class)
library(gbm)
library(ISLR)
attach(Caravan)
set.seed(1)


# -------------------------- (a) --------------------------
Caravan$Purchase = ifelse(Caravan$Purchase=='Yes', 1, 0)
idx.train = 1:1000
train = Caravan[idx.train, ]
test = Caravan[-idx.train, ]


# -------------------------- (b) --------------------------
ntree = 1000
lambda = 0.01
boost.fit = gbm(Purchase~., data=train, distribution='bernoulli', n.trees=ntree, shrinkage=lambda)
summary(boost.fit)

# The variable PPERSAUT seems the mose important


# -------------------------- (c) --------------------------
boost.pred = predict(boost.fit, newdata=test, n.trees=ntree, type='response')
boost.pred = ifelse(boost.pred>0.2, 1, 0)
table(pred=boost.pred, actual=test$Purchase)

# Fraction of predicted Yes is actual Yes (True Positive): 33/(33+123) = 0.2115385

# KNN: Cross-validating k omitted
k = 20
train.mat = subset(train, select=-Purchase)
test.mat = subset(test, select=-Purchase)
knn.pred = knn(train.mat, test.mat, train$Purchase, k=k, prob=TRUE)

prob = attr(knn.pred, 'prob')
idx.0 = ifelse(knn.pred==0, TRUE, FALSE)
prob[idx.0] = 1 - prob[idx.0]
knn.pred = ifelse(prob>0.2, 1, 0)

table(pred=knn.pred, actual=test$Purchase)

# True Positive Rate: 28/(138+28) = 0.1686747


# logistic regression
log.fit = glm(Purchase~., data=train, family='binomial')
log.pred = predict(log.fit, newdata=test, type='response')
log.pred = ifelse(log.pred>0.2, 1, 0)
table(pred=log.pred, actual=test$Purchase)

# True Positive rate: 58/(350+58) = 0.1421569


# Conclusion: For the current choice of variables and parameters, 
# and measured by true positive rate,
# it seems that the boosting method performs better than KNN and logistic regression