# *************** 6.6 ***************
library(glmnet)
library(ISLR)
attach(Hitters)

Hitters = na.omit(Hitters)
x = model.matrix(Salary~., data=Hitters)[, -1]
y = Hitters$Salary


# *************** 6.6.1 ***************
grid = 10^seq(10, -2, length=100)
ridge.mod = glmnet(x, y, alpha=0, lambda=grid)
dim( coef(ridge.mod) )

ridge.mod$lambda[50]
coef( ridge.mod )[, 50]
sqrt( sum(coef(ridge.mod)[-1, 50]^2) )    # L2 norm

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt( sum(coef(ridge.mod)[-1,60]^2) )

predict(ridge.mod, s=50, type='coefficients')


set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train, ], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred = predict(ridge.mod, s=4, newx=x[test, ])
mean( (ridge.pred-y.test)^2 )

mean( (mean(y[train]) - y.test)^2 )

ridge.pred = predict( ridge.mod, s=1e10, newx=x[test, ])
mean( (ridge.pred - y.test)^2 )

ridge.pred = predict( ridge.mod, s=0, x=x[train, ], y=y[train], newx=x[test, ], exact=T)
mean( (ridge.pred - y.test)^2 )

lm.fit = lm(y~x, subset=train)
predict(ridge.mod, s=0, x=x[train, ], y=y[train], exact=T, type='coefficients')[1:20, ]

set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min

ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test, ])
mean( (ridge.pred - y.test)^2 )

ridge.mod = glmnet(x, y, alpha=0)
predict(ridge.mod, s=bestlam, type='coefficients')


# *************** 6.6.2 ***************
lasso.mod = glmnet(x[train, ], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test, ])
mean( (lasso.pred - y.test)^2 )

lasso.mod = glmnet(x, y, alpha=1)
lasso.coef = predict(lasso.mod, s=bestlam, type='coefficients')
lasso.coef
