library(boot)
library(ISLR)
attach(Wage)
set.seed(1)

# ---------------- (a) ----------------
k.max = 10
rss.cv = rep(0, k.max)
for(k in 1:k.max){
  fit = glm(wage~poly(age, k), data=Wage)
  rss.cv[k] = cv.glm(Wage, fit, K=10)$delta[1]
}

# cross validation RSS comparison
plot(1:k.max, rss.cv, type='l', xlab='', ylab='')
title(main='Cross Validation RSS', xlab='Polynomial Degree', ylab='RSS')
degree = which.min(rss.cv)
points(degree, rss.cv[degree], col='red', cex=2, pch=20)

msg = paste0( 'The polynomial degree chosen using cross-validation is: ',  degree)
print(msg)


# ANOVA comparison
fit.anova = list()
for(k in 1:k.max){
  fit.anova[[k]] = lm(wage~poly(age, k), data=Wage)
}
arg = paste0( paste0('fit.anova[[', 1:k.max, ']]'), collapse=', ')
cmd = paste0('anova(', arg, ')')
eval(parse(text=cmd))



# The cross-validation method chooses degree=4
# The ANOVA method chooses degree = 3 or 4
# Let's assume the degree = 3

fit.4 = lm(wage~poly(age, 3), data=Wage)
age.range = min(Wage$age):max(Wage$age)
pred.4 = predict(fit.4, newdata=list(age=age.range), type='response')

plot(Wage$age, Wage$wage, xlab='Age', ylab='Wage', col='darkgrey')
lines(age.range, pred.4, type='l', col='blue', lwd=2)
title('Actual vs Prediction')



# ---------------- (b) ----------------

#k.max = length(unique(Wage$age)) %/% 2
k.max=10
rss.cv = rep(0, k.max)
for (k in 2:k.max){
  Wage$age.cut = cut(Wage$age, k)
  fit = glm(wage~age.cut, data=Wage)
  rss.cv[k] = cv.glm(Wage, fit, K=10)$delta[1]
}

n.cuts = which.min(rss.cv[-1])+1
plot(2:k.max, rss.cv[-1], type='l', xlab='Number of cuts', ylab='RSS')
points(n.cuts, rss.cv[n.cuts], col='red', cex=2, pch=20)


# The cross-validation method chooses number of cuts = 8
# let's assume number of cuts = 8 going forward

age.range = min(Wage$age):max(Wage$age)
fit = lm(wage~cut(age, 8), data=Wage)
pred = predict(fit, newdata=list(age=age.range), type='response')

plot(Wage$age, Wage$wage, col='darkgrey', xlab='Age', ylab='Wage')
lines(age.range, pred, type='l', col='red', lwd=2)
title('Actual vs Prediction: Step Function')