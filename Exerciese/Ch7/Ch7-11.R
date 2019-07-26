library(MASS)
set.seed(1)

# ------------------------ (a) ------------------------
n = 100
k = 2
X = mvrnorm(n, mu=integer(k+1), Sigma=diag(k+1))
colnames(X) = c(paste0('x', 1:k), 'eps')
x1 = X[, 'x1']
x2 = X[, 'x2']
Y = 3*x1 + 5*x2


# ------------------------ (b) ------------------------
b1 = 0.5


# ------------------------ (c) ------------------------
y.1 = Y-b1*x1
fit.1 = lm(y.1~x2)
summary(fit.1)


# ------------------------ (d) ------------------------
b2 = coef(fit.1)[2]
y.2 = Y - b2 * x2
fit.2 = lm(y.2~x1)
summary(fit.2)


# ------------------------ (e) ------------------------
n.loop = 1000
b0.vec = rep(0, n.loop)
b1.vec = rep(0, n.loop)
b2.vec = rep(0, n.loop)
# initialize
b1 = 0
y.1 = Y - b1 * x1
fit.1 = lm(y.1~x2)
b2.vec[1] = fit.1$coefficients[2]
b0.vec[1] = fit.1$coefficients[1]

for(i in 2:n.loop){
    y.2 = Y - b2.vec[i-1]*x2
    fit.2 = lm(y.2~x1)
    b1.vec[i] = fit.2$coefficients[2]
    
    y.1 = Y - b1.vec[i] * x1
    fit.1 = lm(y.1~x2)
    b2.vec[i] = fit.1$coefficients[2]
    
    b0.vec[i] = fit.1$coefficients[1]
}


plot(1:n.loop, b0.vec, type='l', col='grey', lwd=2,
     xlim=c(1, n.loop), ylim=c(-1, 10), xlab='beta', ylab='value')
lines(1:n.loop, b1.vec, col='blue', lwd=2)
lines(1:n.loop, b2.vec, col='red', lwd=2)
legend('topright', legend=c('beta0', 'beta1', 'beta2'), col=c('grey', 'blue', 'red'), lwd=1)


# ------------------------ (f) ------------------------
fit = lm(Y~x1+x2)
abline(h=fit$coefficients[1], lty=2, col='grey')
abline(h=fit$coefficients[2], lty=2, col='blue')
abline(h=fit$coefficients[3], lty=2, col='red')

# The dashed lines are coefficients obtained from directly regressing
# Y on x1 and x2
# They are overlapping with previous results


# ------------------------ (g) ------------------------
sum.fit = summary(fit)
b0 = fit$coefficients[1]
b1 = fit$coefficients[2]
b2 = fit$coefficients[3]

fit.se = coef(sum.fit)[, 2]
b0.se = fit.se[1]
b1.se = fit.se[2]
b2.se = fit.se[3]

idx.b0 = min(which(abs(b0.vec - b0) < 2*b0.se))

# it requires ~8 iterations to get the coefficients within 2 s.e. to the true value