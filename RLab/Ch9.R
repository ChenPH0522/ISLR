library(ROCR)
library(e1071)
set.seed(1)


# ----------------------- 9.6.1 -----------------------
x = matrix(rnorm(20*2), ncol=2)
y = c(rep(-1, 10), rep(1, 10))
x[y==1, ] = x[y==1, ] + 1
plot(x, col=(3-y))

dat = data.frame(x=x, y=as.factor(y))
svm.fit = svm(y~., data=dat, kernel='linear', cost=10, scale=FALSE)
plot(svm.fit, dat)

svm.fit$index
summary(svm.fit)

svm.fit = svm(y~., data=dat, kernel='linear', cost=0.1, scale=FALSE)
plot(svm.fit, dat)
svm.fit$index

tune.out = tune(svm, y~., data=dat, kernel='linear', ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)    # best cost = 5

bestmod = tune.out$best.model
summary(bestmod)

xtest = matrix(rnorm(20*2), ncol=2)
ytest = sample(c(-1, 1), 20, replace=TRUE)
xtest[ytest==1, ] = xtest[ytest==1, ] + 1
dat.test = data.frame(x=xtest, y=as.factor(ytest))

y.pred = predict(bestmod, newdata=dat.test)
table(pred=y.pred, actual=ytest)    # The correct rate is (10+7)/20 = 0.85

svm.fit = svm(y~., data=dat, kernel='linear', cost=0.01, scale=FALSE)
y.pred = predict(svm.fit, newdata=dat.test)
table(pred = y.pred, actual=ytest)   # The correct rate is (11+4)/20

x[y==1, ] = x[y==1, ] + 1
plot(x, col=(y+5)/2, pch=19)

dat = data.frame(x=x, y=as.factor(y))
svm.fit = svm(y~., data=dat, kernel='linear', cost=1e5, scale=FALSE)
summary(svm.fit)
plot(svm.fit, dat)

svm.fit = svm(y~., data=dat, kernel='linear', cost=1, scale=FALSE)
summary(svm.fit)
plot(svm.fit, dat)


# ----------------------- 9.6.2 -----------------------
x = matrix(rnorm(200*2), ncol=2)
x[1:100, ] = x[1:100, ] + 2
x[101:150, ] = x[101:150, ] - 2
y = c( rep(1, 150), rep(2, 50) )
dat = data.frame(x=x, y=as.factor(y))

plot(x, col=y)

idx.train = sample(200, 100)
train = dat[idx.train, ]
test = dat[-idx.train, ]
svm.fit = svm(y~., data=train, kernel='radial', gamma=1, cost=1)
plot(svm.fit, train)
summary(svm.fit)

svm.fit = svm(y~., data=train, kernel='radial', gamma=1, cost=1e5)
plot(svm.fit, train)

tune.out = tune(svm, y~., data=train, kernel='radial', ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4)))
summary(tune.out)    # best cost = 1, best gamma = 0.5

svm.best = tune.out$best.model
svm.pred = predict(svm.best, data=test)
table(pred=svm.pred, actual=test$y)     # the correct rate is (55+7)/100 = 0.62


# ----------------------- 9.6.3 -----------------------
plot.roc    <- function(pred, actual, ...){
    predob = prediction(pred, actual)
    perf = performance(predob, 'tpr', 'fpr')
    plot(perf, ...)
}

svm.opt = svm(y~., data=train, kernel='radial', gamma=0.5, cost=1, decision.values=TRUE)
svm.pred = predict(svm.opt, train, decision.values=TRUE)
fitted = attr(svm.pred, 'decision.values')

par(mfrow=c(1, 2))
plot.roc( fitted, train$y, main='Training data' )

svm.flex = svm(y~., data=train, kernel='radial', gamma=50, cost=1, decesion.values=TRUE)
svm.pred = predict(svm.flex, train, decision.values=TRUE)
fitted = attr(svm.pred, 'decision.values')
plot.roc( fitted, train$y, add=TRUE, col='red')

svm.opt.pred = predict(svm.opt, test, decision.values=TRUE)
fitted.opt = attr(svm.opt.pred, 'decision.values')

svm.flex.pred = predict(svm.flex, test, decision.values=TRUE)
fitted.flex = attr(svm.flex.pred, 'decision.values')

plot.roc(fitted.opt, test$y, main='Test data')
plot.roc(fitted.flex, test$y, add=TRUE, col='red')


# ----------------------- 9.6.4 -----------------------
x = rbind(x, matrix(rnorm(50*2), ncol=2))
y = c(y, rep(0, 50))
x[y==0, ] = x[y==0, ] + 2
dat = data.frame(x=x, y=as.factor(y))

par(mfrow=c(1, 1))
plot(x, col=(3-y))

svm.fit = svm(y~., data=dat, kernel='radial', gamma=1, cost=10)
plot(svm.fit, dat)


# ----------------------- 9.6.5 -----------------------
library(ISLR)
xtrain = Khan$xtrain
xtest = Khan$xtest
ytrain = Khan$ytrain
ytest = Khan$ytest

table(ytrain)
table(ytest)

dat.train = data.frame(x=xtrain, y=as.factor(ytrain))
svm.linear = svm(y~., dat.train, kernel='linear', cost=10)
summary(svm.linear)

dat.test = data.frame(x=xtest, y=as.factor(ytest))
pred.linear = predict(svm.linear, dat.test)
table(pred=pred.linear, actual=ytest)