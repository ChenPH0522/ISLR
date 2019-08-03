library(e1071)
set.seed(1)

# ------------------------------------
n = 100
p = 2
x = matrix(rnorm(n*p), ncol=p)
y = ifelse( x[,1]^2+x[, 2]^2 < 1, 1, 0 )
plot(x, col=(2-y))


# SVM: polynomial kernal
dat = data.frame(x=x, y=as.factor(y))
svm.poly = svm(y~., data=dat, kernel='polynomial', degree=2, cost=1)
summary(svm.poly)
plot(svm.poly, dat)

svm.pred = predict(svm.poly, newdata=dat)
table(pred=svm.pred, actual=y)     # error rate: 4%


# SVM: radial kernel
dat = data.frame(x=x, y=as.factor(y))
svm.radial = svm(y~., data=dat, kernel='radial', gamma=0.1, cost=1)
summary(svm.radial)
plot(svm.radial, dat)

svm.pred = predict(svm.radial, newdata=dat)
table(pred=svm.pred, actual=y)     # error rate: 10%


# Support Vector Classifier
dat = data.frame(x=x, y=as.factor(y))
svm.linear = svm(y~., data=dat, kernel='linear', cost=10, scale=FALSE)
summary(svm.linear)
plot(svm.linear, dat)

svm.pred = predict(svm.linear, newdata=dat)
table(pred=svm.pred, actual=y)     # error rate: 44%