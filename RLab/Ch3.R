# 3.6.1
install.packages('ISLR')
library(ISLR)

# 3.6.2
library(MASS)
fix(Boston)
names(Boston)
attach(Boston)

lm.fit = lm(medv~lstat)
summary(lm.fit)

names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval='confidence')
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval='prediction')

plot(lstat, medv, pch='+')
abline(lm.fit, lwd=3, col='red')

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,2))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

par(mfrow=c(1,1))
h = hatvalues(lm.fit)
plot(h)
which.max(h)

# 3.6.3
lm.fit = lm(medv~lstat+age, Boston)
summary(lm.fit)

lm.fit = lm(medv~., Boston)
summary(lm.fit)

?summary.lm
summary(lm.fit)$r.squared
summary(lm.fit)$sigma

install.packages('car')
library(car)
vif(lm.fit)

lm.fit1 = lm(medv~.-age, Boston)
summary(lm.fit1)
lm.fit1 = update(lm.fit, ~.-age)

# 3.6.4
lm.fit = lm(medv~lstat*age, Boston)
summary(lm.fit)

# 3.6.5
lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

lm.fit = lm(medv~lstat)
anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5 = lm(medv~poly(lstat, 5))
summary(lm.fit5)

lm.fit = lm(medv~log(rm), Boston)
summary(lm.fit)

# 3.6.6
fix(Carseats)
names(Carseats)

lm.fit = lm(Sales~.+Income:Advertising + Price:Age, Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)

# 3.6.7
LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print('The libraries have been loaded')
}

LoadLibraries()