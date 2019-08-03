library(e1071)
set.seed(1)


# ---------------------- (a) ----------------------
n = 500
p = 2
x = matrix(rnorm(n*p), ncol=p)
y = 1 * (x[, 1]^2 + x[, 2]^2 > 1)


# ---------------------- (b) ----------------------
plot(x, col=(2-y), main='Original Data')


# ---------------------- (c) ----------------------
dat = data.frame(x=x, y=as.factor(y))
log.fit = glm(y~., data=dat, family='binomial')


# ---------------------- (d) ----------------------
log.pred = predict(log.fit, newdata=dat, type='response')
log.pred = 1 * (log.pred>0.6)
plot(x, col=(2-log.pred), main='Linear Logistic Regression')


# ---------------------- (e) ----------------------
log.fit.poly = glm(y~poly(x, 2), family='binomial')


# ---------------------- (f) ----------------------
log.pred.poly = predict(log.fit.poly, newdata=dat, type='response')
log.pred.poly = 1 * (log.pred.poly > 0.5)
plot(x, col=(2 - log.pred.poly), main='Polynomial Logistic Regression')


# ---------------------- (g) ----------------------
svm.fit = svm(y~., data=dat, kernel='linear', cost=1)
svm.pred = predict(svm.fit, newdata=dat)
svm.pred = as.numeric(levels(svm.pred))[svm.pred]
plot(x, col=(2-svm.pred), main='Support Vector Classifier')


# ---------------------- (h) ----------------------
svm.fit.poly = svm(y~., data=dat, kernel='polynomial', degree=2, cost=1)
svm.pred.poly = predict(svm.fit.poly, newdata=dat)
svm.pred.poly = as.numeric(levels(svm.pred.poly))[svm.pred.poly]
plot(x, col=(2-svm.pred.poly), main='Support Vector Machine')


# ---------------------- (i) ----------------------
# Obviously, for non-linear decision boundaries, polynomial logistic regression
# and Support Vector Machine with polynomial kernel have better performance
# than their linear counterparties