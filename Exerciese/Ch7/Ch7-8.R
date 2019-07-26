library(gam)
library(boot)
library(ISLR)
attach(Auto)
set.seed(1)

# --------- exploratory
# let's use mpg as dependent variable

cols = colnames(Auto)
for(c in cols){
    plot(Auto[[c]], Auto$mpg, col='darkgrey', xlab=c, ylab='mpg')
}

# According to the plots, let's investigate displacement

# Lets use the following models:
# Polynomial
# cubic spline
# natural spline
# smooth spline
# local regression

# Polynomial
# First, lets determine the degree using cross validation

d.max = 10
cv.rss = rep(0, d.max)
for(d in 1:d.max){
    fit = glm(mpg~poly(displacement, d), data=Auto)
    cv.rss[d] = cv.glm(Auto, fit, K=10)$delta[1]
}

d.chosen = which.min(cv.rss)
plot(1:d.max, cv.rss, type='l', xlab='degree', ylab='rss')
points(d.chosen, cv.rss[d.chosen], col='red', cex=2, pch=20)

# Although degree = 10 leads to the lowest rss
# To avoid overfitting, we choose degree = 2

# Fitting
disp.range = min(Auto$displacement):max(Auto$displacement)
fit.poly = glm(mpg~poly(displacement, 2), data=Auto)
pred.poly = predict(fit.poly, newdata=list(displacement=disp.range), type='response')

# Cubit Spline
fit.bs = glm(mpg~bs(displacement), data=Auto)
pred.bs = predict(fit.bs, newdata=list(displacement=disp.range), type='response')

# Natural Spline
fit.ns = glm(mpg~ns(displacement, df=3), data=Auto)
pred.ns = predict(fit.ns, newdata=list(displacement=disp.range), type='response')

# Smoothing Spline
fit.ss = gam(mpg~s(displacement), data=Auto)
pred.ss = predict(fit.ss, newdata=list(displacement=disp.range), type='response')

# Local Regression
fit.lo = loess(mpg~displacement, span=0.7, data=Auto)
pred.lo = predict(fit.lo, newdata=data.frame(displacement=disp.range), type='response')


# Comparing Models by plotting them
plot(Auto$displacement, Auto$mpg, col='darkgrey', xlab='displcement', ylab='mpg')
lines(disp.range, pred.poly, col='red', lwd=2)
lines(disp.range, pred.bs, col='orange', lwd=2)
lines(disp.range, pred.ns, col='green', lwd=2)
lines(disp.range, pred.ss, col='blue', lwd=2)
lines(disp.range, pred.lo, col='purple', lwd=2)
legend('topright', legend=c('polynomial', 'cubic spline', 'natural spline', 'smoothing spline', 'local regression'), 
       col=c('red', 'orange', 'green', 'blue', 'purple'), lwd=2)


# using ANOVA analysis
fit.poly = gam(mpg~poly(displacement, 2), data=Auto)
fit.bs = gam(mpg~bs(displacement), data=Auto)
fit.ns = gam(mpg~ns(displacement, df=3), data=Auto)
fit.ss = gam(mpg~s(displacement), data=Auto)
fit.lo = gam(mpg~lo(displacement, span=0.7), data=Auto)

anova(fit.poly, fit.bs, fit.ns, fit.ss, fit.lo)

# conclusion: the five models are performing really close to each other
# the local regression method might be lightly better than the others, if there's any difference