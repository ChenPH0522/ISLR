# ************* 5.3.1 *************
library(ISLR)
attach(Auto)
set.seed(1)

n.obs = dim(Auto)[1]
train = sample(n.obs, round(n.obs/2) )

lm.fit = lm(mpg~horsepower, data=Auto, subset=train)
lm.pred = predict(lm.fit, newdata=Auto)
mse = mean( (lm.pred - mpg)[-train]^2 )

lm.fit2 = lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
lm.pred2 = predict(lm.fit2, newdata=Auto)
mse2 = mean( (lm.pred2 - mpg)[-train]^2 )

lm.fit3 = lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
lm.pred3 = predict(lm.fit3, newdata=Auto)
mse3 = mean( (lm.pred3 - mpg)[-train]^2 )

# ************* 5.3.2 *************
library(boot)

glm.fit = glm(mpg~horsepower, data=Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

cv.error = rep(0, 5)
for (i in 1:5) {
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}

cv.error

# ************* 5.3.3 *************
set.seed(1)

cv.error.10 = rep(0, 10)
for (i in 1:10) {
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}

cv.error.10

# ************* 5.3.4 *************

# 5.3.4.1
alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  return( (var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X, Y)) )
}
alpha.fn(Portfolio, 1:100)

set.seed(1)
sample1 = sample(100, 100, replace=T)
alpha.fn(Portfolio, sample1)

boot(Portfolio, alpha.fn, R=1000)

# 5.3.4.2
boot.fn = function(data, index){
  return( coef(lm(mpg~horsepower, data=data, subset=index)) )
}
n.obs = dim(Auto)[1]
boot.fn(Auto, 1:n.obs)

set.seed(1)
sample1 = sample(n.obs, n.obs, replace=T)
boot.fn(Auto, sample1)

set.seed(1)
boot(Auto, boot.fn, R=1000)

lm.fit = lm(mpg~horsepower, data=Auto)
summary(lm.fit)

boot.fn2 = function(data, index) {
  return( coef(lm(mpg~poly(horsepower, 2, raw=T), data=data, subset=index)) )
}
set.seed(1)
boot(Auto, boot.fn2, R=1000)

lm.fit2 = lm(mpg~poly(horsepower,2, raw=T), data=Auto)
summary(lm.fit2)