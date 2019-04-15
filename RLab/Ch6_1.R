# *************** 6.5.1 ***************
library(ISLR)
attach(Hitters)

names(Hitters)
dim(Hitters)
sum( is.na(Hitters$Salary) )

Hitters = na.omit(Hitters)
dim(Hitters)
sum( is.na( Hitters$Salary ) )

library(leaps)
regfit.full = regsubsets(Salary~., Hitters)
summary(regfit.full)

regfit.full = regsubsets(Salary~., Hitters, nvmax=19)
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab='Number of Variables', ylab='RSS', type='l')
idx = which.min(reg.summary$rss)
points(idx, reg.summary$rss[idx], col='red', cex=2, pch=20)

plot(reg.summary$rsq, xlab='Number of Variables', ylab='R2', type='l')
idx = which.max(reg.summary$rsq)
points(idx, reg.summary$rsq[idx], col='red', cex=2, pch=20)

plot(reg.summary$cp, xlab='Number of Variables', ylab='Cp', type='l')
idx = which.min(reg.summary$cp)
points(idx, reg.summary$cp[idx], col='red', cex=2, pch=20)

plot(reg.summary$bic, xlab='Number of Variables', ylab='BIC', type='l')
idx = which.min(reg.summary$bic)
points(idx, reg.summary$bic[idx], col='red', cex=2, pch=20)

par(mfrow=c(2,2))
plot(regfit.full, scale='r2')
plot(regfit.full, scale='adjr2')
plot(regfit.full, scale='Cp')
plot(regfit.full, scale='bic')

coef(regfit.full, 6)


# *************** 6.5.2 ***************
par(mfrow=c(1,1))

regfit.fwd = regsubsets(Salary~., Hitters, nvmax=19, method='forward')
summary(regfit.fwd)

regfit.bwd = regsubsets(Salary~., Hitters, nvmax=19, method='backward')
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


# *************** 6.5.3 ***************
set.seed(1)
n.obs = nrow(Hitters)
train = sample(n.obs, round(n.obs/2), replace=F)

regfit.best = regsubsets(Salary~., data=Hitters[train, ], nvmax=19)
test.mat = model.matrix(Salary~., data=Hitters[-train, ])

val.err = rep(NA, 19)
for (i in 1:19){
  coeff = coef(regfit.best, i)
  pred = test.mat[, names(coeff)] %*% coeff
  val.err[i] = mean( (Hitters$Salary[-train] - pred)^2 )
}

plot(val.err, xlab='Number of Variables', ylab='Test Error', type='l')
idx = which.min(val.err)
points(idx, val.err[idx], col='red', cex=2, pch=20)
coef(regfit.best, idx)

# the best model has 8 predictors


predict <- function(object, newdata, id){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coeff = coef(object, id)
  return( mat[, names(coeff)] %*% coeff )
}

regfit.best = regsubsets(Salary~., Hitters, nvmax=idx)
coef(regfit.best, idx)


set.seed(1)
k = 10
folds = sample(1:k, n.obs, replace=T)
cv.err = matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for (i in 1:k){
  best.fit = regsubsets(Salary~., Hitters[folds!=i, ], nvmax=19)
  for (j in 1:19){
    pred = predict(best.fit, Hitters[folds==i, ], j)
    cv.err[i, j] = mean( (Hitters$Salary[folds==i] - pred)^2 )
  }
}

mean.cv.err = colMeans(cv.err)
plot(mean.cv.err, xlab='Number of Variables', type='b')
idx = which.min(mean.cv.err)
points(idx, mean.cv.err[idx], col='red', cex=2, pch=20)

reg.best = regsubsets(Salary~., Hitters, nvmax=idx)
coef(reg.best, idx)