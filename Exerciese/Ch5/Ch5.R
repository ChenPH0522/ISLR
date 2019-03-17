# ************* Quetion 2 *************
# (g)

x = 1:1e5
y = 1 - (1-1/x)^x
plot(x, y, ylim=c(0, 1))

# (h)
set.seed(1)
rec = rep(NA, 1e4)
for (i in 1:1e4){
  rec[i] = sum(sample(1:100, replace=T) == 4) > 0
}
mean(rec)


# ************* Quetion 5 *************
library(ISLR)
attach(Default)

# (a)
glm.fit = glm(default~income+balance, data=Default, family=binomial)

# (b)
set.seed(1)
n.obs = nrow(Default)
train = sample(n.obs, round(n.obs/2), replace=F)
Default.train = Default[train, ]
Default.test = Default[-train, ]

glm.fit = glm(default~income+balance, data=Default.train, family=binomial)
glm.prob = predict(glm.fit, newdata=Default.test, type='response')
glm.pred = rep('No', nrow(Default.test))
glm.pred[ glm.prob>=0.5 ] = 'Yes'

test.err.1 = mean(glm.pred!=Default.test$default)

# The test error rate is 0.0286

# (c)
b.fn = function(data, index){
  
  data.train = data[index, ]
  data.test = data[-index, ]
  
  glm.fit = glm(default~income+balance, data=data.train, family=binomial)
  glm.prob = predict(glm.fit, newdata=data.test, type='response')
  glm.pred = rep('No', nrow(data.test))
  glm.pred[ glm.prob>=0.5 ] = 'Yes'

  return( mean(glm.pred!=data.test$default) )
}

set.seed(2)
n.obs = nrow(Default)
train.2 = sample(n.obs, round(n.obs/2), replace=F)
test.err.2 = b.fn(Default, train.2)

set.seed(3)
train.3 = sample(n.obs, round(n.obs/2), replace=F)
test.err.3 = b.fn(Default, train.3)

# The test error rates are: 0.0286, 0.0276, 0.0248
# The results are close to each other in terms of their numerical value, but are
# not stable when evalutated in percentage change

# (d)
c.fn = function(data, index){
  
  data.train = data[index, ]
  data.test = data[-index, ]
  
  glm.fit = glm(default~income+balance+student, data=data.train, family=binomial)
  glm.prob = predict(glm.fit, newdata=data.test, type='response')
  glm.pred = rep('No', nrow(data.test))
  glm.pred[ glm.prob>=0.5 ] = 'Yes'
  
  return( mean(glm.pred!=data.test$default) )
}

test.err.1 = c.fn(Default, train)
test.err.2 = c.fn(Default, train.2)
test.err.3 = c.fn(Default, train.3)

# The test error rates are: 0.0288, 0.0286, 0.0248
# Adding dummy variable 'student' does not seem to improve the model


# ************* Quetion 6 *************
library(boot)
library(ISLR)
attach(Default)

# (a)
glm.fit = glm(default~income+balance, data=Default, family=binomial)
summary(glm.fit)

# S.E. for income: 4.985e-06
# S.E. for balance: 2.274e-04

# (b)
boot.fn = function(data, index){
  glm.fit = glm(default~income+balance, data=data, subset=index, family=binomial)
  return( coef(glm.fit)[-1] )
}

# (c)
set.seed(1)
boot(Default, boot.fn, R=1000)

# S.E. for income: 4.582525e-06
# S.E. for balance: 2.267955e-04

# (d)
# The two approaches produce very close results


# ************* Quetion 7 *************
library(ISLR)
attach(Weekly)

# (a)
glm.fit = glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)

# (b)
glm.fit = glm(Direction~Lag1+Lag2, data=Weekly[-1, ], family=binomial)

# (c)
glm.prob = predict(glm.fit, newdata=Weekly[1, ], type='response')
glm.pred = if (glm.prob >= 0.5) 'Up' else 'Down'

# Predicted Direction: Up
# Real Direction: Down
# The observation is wrongly classified

# (d)
n.obs = nrow(Weekly)
store = rep(NA, n.obs)
for (i in 1:n.obs){
  glm.fit = glm(Direction~Lag1+Lag2, data=Weekly[-i, ], family=binomial)
  glm.prob = predict(glm.fit, newdata=Weekly[i, ], type='response')
  glm.pred = if (glm.prob >= 0.5) 'Up' else 'Down'
  store[i] = (glm.pred != Weekly$Direction[i]) * 1
}

# (e)
mean(store)
#LOOCV test error rate is 0.4499541


# ************* Quetion 8 *************

# (a)
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)

# n = 100, p = 2
# the equation is: y = x - 2 * X^2 + eps

# (b)
plot(x, y)

# The plot is downward quadratic

# (c)
df = data.frame(x, y)
n.obs = nrow(df)
cv.err = rep(NA, 5)

set.seed(1)
for (i in 1:5) {
  glm.fit = glm(y~poly(x, i, raw=T), data=df)
  cv.err[i] = cv.glm(df, glm.fit, K=n.obs)$delta[1]
}

# (d)
set.seed(2)
for (i in 1:5) {
  glm.fit = glm(y~poly(x, i, raw=T), data=df)
  cv.err[i] = cv.glm(df, glm.fit, K=n.obs)$delta[1]
}

# The results is the same as LOOCV test error has analytical expression
# - formula (5.2)

# (e)
# when degree = 2, the LOOCV error is the lowest
# This is expected as the true model has degree = 2

# (f)
sig = rep(NA, 5)
for (i in 1:5){
  glm.fit = glm(y~poly(x, i, raw=T), data=df)
  print( paste0('degree = ', i) )
  print( summary(glm.fit)$coefficients[-1, 4] )
}

# degree = 1: not significant
# degree = 2: both are significant
# degree = 3: x^3 is not significant
# degree = 4: x^3 and x^4 are not significant
# degree = 5: x^3, x^4 and x^5 are not significant

# This agrees with conclusion drawn from LOOCV results


# ************* Quetion 9 *************
library(boot)
library(MASS)
attach(Boston)

# (a)
mu = mean(medv)
# mean is 22.53281

# (b)
sigma = sd(medv)/sqrt( length(medv) )
# standard error is 0.4088611

# (c)
boot.fn = function(data, index){
  return( mean( data$medv[index] ) )
}
set.seed(1)
boot.result = boot(Boston, boot.fn, R=1000)

# bootstrap standard error is 0.4119374

# (d)
low = mu - qnorm(0.975) * sd(boot.result$t)
up = mu + qnorm(0.975) * sd(boot.result$t)

t.test(Boston$medv)

# By bootstrap, the 95% confidence interval is (21.72542, 23.34019)
# This is the same as the t.test results up to 2nd decimal place.

# (e)
med = median(medv)

# (f)
boot.fn = function(data, index){
  return( median(data$medv[index]) )
}
set.seed(1)
boot.result = boot(Boston, boot.fn, R=1000)

# standard error of the median is 0.3801002

# (g)
q.10 = quantile(medv, 0.1)

# (h)
boot.fn = function(data, index){
  return( quantile(data$medv[index], 0.1) )
}
set.seed(1)
boot.result = boot(Boston, boot.fn, R=1000)

# standard error of the median is 0.505056