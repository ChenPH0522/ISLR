library(gam)
library(boot)
library(MASS)
attach(Boston)
set.seed(1)

# --------------- (a) ---------------
dis.range = seq(min(dis), max(dis), length.out=100)
fit.poly = lm(nox~poly(dis, 3))
pred.poly = predict(fit.poly, newdata=list(dis=dis.range), type='response')

summary(fit.poly)
# All factors are statistically significant

plot(dis, nox, col='darkgrey', xlab='dis', ylab='nox')
lines(dis.range, pred.poly, col='blue', lwd=2)


# --------------- (b) ---------------
fit.poly.alt = lm(nox~poly(dis, 5))
pred.poly.alt = predict(fit.poly.alt, newdata=list(dis=dis.range), type='response')

summary(fit.poly.alt)
# beta for degree = 4 is not statistically significant
# beta for degree = 5 is significant at 99% level

lines(dis.range, pred.poly.alt, col='red', lwd=2)
legend('topright', legend=c('degree=3', 'degree=5'), col=c('blue', 'red'), lwd=2)


# --------------- (c) ---------------
d.max = 10
cv.rss = rep(0, d.max)
for(d in 1:d.max){
    fit = glm(nox~poly(dis, d))
    cv.rss[d] = cv.glm(Boston, fit, K=10)$delta[1]
}

d.chosen = which.min(cv.rss)
plot(1:d.max, cv.rss, type='l', xlab='degree', ylab='rss')
points(d.chosen, cv.rss[d.chosen], col='red', cex=2, pch=20)

# The best degree is 4


# --------------- (d) ---------------
fit.bs = glm(nox~bs(dis, df=4))
pred.bs = predict(fit.bs, newdata=list(dis=dis.range), type='response')

plot(dis, nox, col='darkgrey', xlab='dis', ylab='nox')
lines(dis.range, pred.bs, col='blue', lwd=2)

# get the knots
attr(fit.bs$terms, 'predvars')

# The knots are automatically selected by the bs() function: 1.1296, 12.1265


# --------------- (e) ---------------
d.min = 3
d.max = 7
od = order(dis)
pred.lst = list()
fit.lst = list()
for(d in d.min:d.max){
    fit = glm(nox~bs(dis, df=d))
    pred.lst[[d]] = fit$fitted.values[od]
    fit.lst[[d]] = fit
}


fit.col = c('red', 'orange', 'green', 'blue', 'purple')
fit.name = paste0('degree=', d.min:d.max)
plot(dis, nox, col='darkgrey', xlab='dis', ylab='nox')
for(d in d.min:d.max){
    lines(sort(dis), pred.lst[[d]], col=fit.col[d+1-d.min], lwd=2)
}
legend('topright', legend=fit.name, col=fit.col, lwd=1)


rss.vec = rep(0, d.max)
for(d in d.min:d.max){
    rss.vec[d] = sum(fit.lst[[d]]$residuals^2)
}

plot(d.min:d.max, rss.vec[-(1:(d.min-1))], type='l', xlab='df', ylab='rss')


# --------------- (f) ---------------
# let's use the anova approach

fit.1 = gam(nox~bs(dis, 3))
fit.2 = gam(nox~bs(dis, 4))
fit.3 = gam(nox~bs(dis, 5))
fit.4 = gam(nox~bs(dis, 6))
fit.5 = gam(nox~bs(dis, 7))
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
