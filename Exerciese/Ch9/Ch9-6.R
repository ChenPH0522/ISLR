library(e1071)
set.seed(1)

# ------------------------- (a) -------------------------
n = 100
p = 2
x = matrix(runif(n*p), ncol=p)
y = ifelse(x[,1]+x[,2]>1, 1, 0)
plot(x, col=(2-y), main='Original Data')


# ------------------------- (b) -------------------------
costs = c(0.01, 0.1, 1, 5, 10, 100)
dat = data.frame(x=x, y=as.factor(y))
tune.out = tune(svm, y~., data=dat, kernel='linear', ranges=list(cost=costs))
err.cv = tune.out$performances$error
plot(costs, err.cv, type='b', xlab='cost', ylab='cross-validation error')


err.train = rep(0, length(costs))
for(i in length(costs)){
    svm.fit = svm(y~., data=dat, kernel='linear', cost=costs[i])
    mat = table(pred=svm.fit$fitted, actual=dat$y)
    err.train[i] = (mat[1, 2]+mat[2, 1])/sum(mat)
}
plot(costs, err.train, type='b', xlab='cost', ylab='training error')

# training error < corss-validation error


# ------------------------- (c) -------------------------
set.seed(9)
xtest = matrix(runif(n*p), ncol=p)
ytest = ifelse(x[, 1]+x[, 2]>1, 1, 0)
dat.test = data.frame(x=xtest, y=ytest)
err.test = rep(0, length(costs))
for(i in 1:length(costs)){
    svm.fit = svm(y~., data=dat, kernel='linear', cost=costs[i])
    svm.pred = predict(svm.fit, newdata=dat.test)
    mat = table(pred=svm.pred, actual=ytest)
    err.test[i] = (mat[1, 2]+mat[2, 1])/sum(mat)
}

plot(costs, err.test, type='b', xlab='cost', ylab='test error')

# cost = 0.1 leads to the smallest test error
# this is smaller than the costs leading to smallest training error
# and cross-validation error


# ------------------------- (d) -------------------------
# large cost tends to cause over-fitting