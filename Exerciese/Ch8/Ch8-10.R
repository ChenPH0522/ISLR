library(randomForest)
library(glmnet)
library(boot)
library(gbm)
library(ISLR)
attach(Hitters)
set.seed(1)


# -------------------------- (a) --------------------------
Hitters = Hitters[!is.na(Hitters$Salary), ]
Hitters[['logSalary']] = log(Hitters$Salary)


# -------------------------- (b) --------------------------
idx.train = 1:200
train = Hitters[idx.train, ]
test = Hitters[-idx.train, ]


# -------------------------- (c) --------------------------
ntree = 1000
lambda = seq(0, 1, by=0.05)
mse.train = c()
mse.test = c()
for(l in lambda){
    boost.fit = gbm(logSalary~.-Salary, distribution='gaussian', data=train, n.trees=ntree, shrinkage=l)
    boost.train = predict(boost.fit, newdata=train, n.trees=ntree)
    boost.test = predict(boost.fit, newdata=test, n.trees=ntree)
    err.train = mean( (boost.train-train$logSalary)^2 )
    err.test = mean( (boost.test-test$logSalary)^2 )
    
    mse.train = c(mse.train, err.train)
    mse.test = c(mse.test, err.test)
}


plot(lambda, mse.train, type='l', xlab='lambda', ylab='Training MSE')


# -------------------------- (d) --------------------------
plot(lambda, mse.test, type='l', xlab='lambda', ylab='Test MSE')
# The optimal shrinkage parameter is 0.15
# The optimal MSE is 0.255685


# -------------------------- (e) --------------------------

# Applying linear regression
# Cross-validation and Factor selection omitted
lm.fit = lm(logSalary~.-Salary, data=train)
lm.pred = predict(lm.fit, newdata=test)
mse.lm.test = mean( (lm.pred-test$logSalary)^2 )     # MSE is 0.4917959


# Applying ridge regression
x = model.matrix(logSalary~.-Salary, data=train)
y = train$logSalary
ridge.cv = cv.glmnet(x, y, nfolds=10)
lambda.min = ridge.cv$lambda.min

new.x = model.matrix(logSalary~.-Salary, data=test)
ridge.fit = glmnet(x, y, family='gaussian', alpha=0, lambda=lambda.min)
ridge.pred = predict(ridge.fit, newx=new.x)
mse.ridge.test = mean( (ridge.pred-test$logSalary)^2 )   # MSE is 0.4755723


# -------------------------- (f) --------------------------
lambda.opt = lambda[which.min(mse.test)]
boost.opt = gbm(logSalary~.-Salary, distribution='gaussian', data=train, n.trees=ntree, shrinkage=lambda.opt)
summary(boost.opt)

# Variable CAtBat seems the most important variable


# -------------------------- (g) --------------------------
bag.fit = randomForest(logSalary~.-Salary, data=train, mtry=ncol(train)-2)
bag.pred = predict(bag.fit, newdata=test)
mse.bag.test = mean( (bag.pred-test$logSalary)^2 )    # MSE is 0.2287061