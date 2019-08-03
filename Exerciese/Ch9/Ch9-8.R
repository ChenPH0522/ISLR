library(e1071)
library(ISLR)
attach(OJ)
set.seed(1)


# ------------------------ (a) ------------------------
idx.train = sample(nrow(OJ), 800)
train = OJ[idx.train, ]
test = OJ[-idx.train, ]


# ------------------------ (b) ------------------------
svm.linear = svm(Purchase~., data=train, kernel='linear', cost=0.01)
summary(svm.linear)


# ------------------------ (c) ------------------------
mat.train = table(pred=svm.linear$fitted, actual=train$Purchase)
err.train = (mat.train[1, 2] + mat.train[2, 1])/sum(mat.train)
# training error = 0.175

svm.linear.pred = predict(svm.linear, newdata=test)
mat.test = table(pred=svm.linear.pred, actual=test$Purchase)
err.test = (mat.test[1, 2] + mat.test[2, 1])/sum(mat.test)
# test error = 0.177778


# ------------------------ (d) ------------------------
costs = c(0.01, 0.1, 0.5, 1, 3, 5, 7, 10)
svm.linear.cv = tune(svm, Purchase~., data=train, kernel='linear', ranges=list(cost=costs))
summary(svm.linear.cv)
# optimal cost=7


# ------------------------ (e) ------------------------
svm.linear.opt = svm.linear.cv$best.model
mat.opt.train = table(pred=svm.linear.opt$fitted, actual=train$Purchase)
err.opt.train = (mat.opt.train[1,2]+mat.opt.train[2,1])/sum(mat.opt.train)
# training error = 0.1625

svm.linear.opt.pred = predict(svm.linear.opt, newdata=test)
mat.opt.test = table(pred=svm.linear.opt.pred, actual=test$Purchase)
err.opt.test = ( mat.opt.test[1,2]+mat.opt.test[2,1] )/sum(mat.opt.test)
# test error = 0.1519


# ------------------------ (f) ------------------------
svm.radial = svm(Purchase~., data=train, kernel='radial', cost=0.01)
summary(svm.radial)

mat.train = table(pred=svm.radial$fitted, actual=train$Purchase)
err.train = (mat.train[1, 2] + mat.train[2, 1])/sum(mat.train)
# training error = 0.39375

svm.radial.pred = predict(svm.radial, newdata=test)
mat.test = table(pred=svm.radial.pred, actual=test$Purchase)
err.test = (mat.test[1, 2] + mat.test[2, 1])/sum(mat.test)
# test error = 0.37778

costs = c(0.01, 0.1, 0.5, 1, 3, 5, 7, 10)
svm.radial.cv = tune(svm, Purchase~., data=train, kernel='radial', ranges=list(cost=costs))
summary(svm.radial.cv)
# optimal cost=0.5

svm.radial.opt = svm.radial.cv$best.model
mat.opt.train = table(pred=svm.radial.opt$fitted, actual=train$Purchase)
err.opt.train = (mat.opt.train[1,2]+mat.opt.train[2,1])/sum(mat.opt.train)
# training error = 0.1475

svm.radial.opt.pred = predict(svm.radial.opt, newdata=test)
mat.opt.test = table(pred=svm.radial.opt.pred, actual=test$Purchase)
err.opt.test = ( mat.opt.test[1,2]+mat.opt.test[2,1] )/sum(mat.opt.test)
# test error = 0.17778


# ------------------------ (g) ------------------------
svm.polynomial = svm(Purchase~., data=train, kernel='polynomial', cost=0.01, degree=2)
summary(svm.polynomial)

mat.train = table(pred=svm.polynomial$fitted, actual=train$Purchase)
err.train = (mat.train[1, 2] + mat.train[2, 1])/sum(mat.train)
# training error = 0.3725

svm.polynomial.pred = predict(svm.polynomial, newdata=test)
mat.test = table(pred=svm.polynomial.pred, actual=test$Purchase)
err.test = (mat.test[1, 2] + mat.test[2, 1])/sum(mat.test)
# test error = 0.36667

costs = c(0.01, 0.1, 0.5, 1, 3, 5, 7, 10)
svm.polynomial.cv = tune(svm, Purchase~., data=train, kernel='polynomial', degree=2, ranges=list(cost=costs))
summary(svm.polynomial.cv)
# optimal cost=7

svm.polynomial.opt = svm.polynomial.cv$best.model
mat.opt.train = table(pred=svm.polynomial.opt$fitted, actual=train$Purchase)
err.opt.train = (mat.opt.train[1,2]+mat.opt.train[2,1])/sum(mat.opt.train)
# training error = 0.15

svm.polynomial.opt.pred = predict(svm.polynomial.opt, newdata=test)
mat.opt.test = table(pred=svm.polynomial.opt.pred, actual=test$Purchase)
err.opt.test = ( mat.opt.test[1,2]+mat.opt.test[2,1] )/sum(mat.opt.test)
# test error = 0.18889


# ------------------------ (h) ------------------------
# Overall, SVM with radial kernel has the best performance