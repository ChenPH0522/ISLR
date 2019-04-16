# **************** 6.7 ****************
library(pls)
library(ISLR)
attach(Hitters)

Hitters = na.omit(Hitters)

set.seed(1)
train = sample(nrow(Hitters), nrow(Hitters)/2)
test = -train
x = model.matrix(Salary~., Hitters)[, -1]
y = Hitters$Salary
y.test = y[test]


# **************** 6.7.1 ****************
set.seed(1)
pcr.fit = pcr(Salary~., data=Hitters, scale=T, validation='CV')
summary(pcr.fit)
validationplot(pcr.fit, val.type='MSEP')

set.seed(1)
pcr.fit = pcr(Salary~., data=Hitters, subset=train, scale=T, validation='CV')
validationplot(pcr.fit, val.type='MSEP')

pcr.pred = predict(pcr.fit, x[test, ], ncomp=7)
mean( (pcr.pred - y.test)^2 )

pcr.fit = pcr(y~x, data=Hitters, ncomp=7, scale=T)
summary(pcr.fit)


# **************** 6.7.2 ****************
set.seed(1)
pls.fit = plsr(Salary~., data=Hitters, subset=train, scale=T, validation='CV')
summary(pls.fit)
validationplot(pls.fit, val.type='MSEP')

pls.pred = predict(pls.fit, newdata=x[test, ], ncomp=2)
mean( (pls.pred - y.test)^2 )

pls.fit = plsr(y~x, scale=T, ncomp=2)
summary(pls.fit)
