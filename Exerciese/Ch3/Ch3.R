# -------------------- Problem 8 --------------------
library(ISLR)
attach(Auto)

# ---- (a) ----
lm.fit = lm(mpg~horsepower)
summary(lm.fit)

# i. There is a relationship between the predictor and the response since 
# the coefficient is statistically significant

# ii. The relationship is of 99% confidence.

# iii. The relationship is negative

# iv.
predict(lm.fit, data.frame(horsepower=98), interval='confidence', level=0.95)
predict(lm.fit, data.frame(horsepower=98), interval='prediction', level=0.95)

# ---- (b) ----
plot(horsepower, mpg)
abline(lm.fit)

# ---- (c) ----
par(mfrow=c(2,2))
plot(lm.fit)

# There is some non-linearity relationship as well as heteroskedasticity
# between the response and the predictor according to the residual-fitted values plot. 
# Most standardized residuals are within 2 standard deviations, so there is no
# unusual outliers in this dataset.

# -------------------- Problem 9 --------------------
library(ISLR)
attach(Auto)

# ---- (a) ----
pairs(Auto)

# ---- (b) ----
names(Auto)
cor(Auto[-9])

# ---- (c) ----
lm.fit = lm(mpg~.-name, Auto)
summary(lm.fit)

# i. For displacement, weight, year and origin, it is likely that there is relationship
# between the response and these variables. For other variables, it is not likely
# that there is a relationship
# 
# ii. displacement, horsepower, year and origin
# 
# iii. Younger cars tend to have higher mpg thant more aged cars, which makes sense.

# ---- (d) ----
par(mfrow=c(2,2))
plot(lm.fit)

# Most standardized residuals are within 2 standard deviations, so there is no
# unusual outliers in this dataset.

# There are a few points with unusually high leverage, especially observation-14.

# ---- (e) ----
lm.fit = lm(mpg~(.-name)^2, Auto)
summary(lm.fit)

# the most statistically significant interaction term is: acceleration vs origin
# the terms cylinders, horsepower, weight and year are not statistically significant
# the next explores different syntax for this interaction term:

lm.fit = lm(mpg~displacement + acceleration + origin + acceleration:origin, Auto)
summary(lm.fit)

lm.fit = lm(mpg~displacement + acceleration*origin, Auto)
summary(lm.fit)

# ---- (f) ----
lm.fit.log = lm(log(mpg)~.-name, Auto)
summary(lm.fit.log)

lm.fit.sqrt = lm(sqrt(mpg)~.-name, Auto)
summary(lm.fit.sqrt)

lm.fit.sq = lm(mpg^2~.-name, Auto)
summary(lm.fit.sq)

# log transformation seems the best choice, followed by square root transformation
# since they reduces the non-linearity in the data
# square transformation is the worse since it increases the non-linearity in the
# data

# -------------------- Problem 10 --------------------
library(ISLR)
attach(Carseats)

# ---- (a) ----
lm.fit = lm(Sales ~ Price+Urban+US, Carseats)
summary(lm.fit)

# ---- (b) ----

# Price: Raising price by $1 would decrease sales of Carseats by roughly 54 units
# UrbanYes: Puting stores in urban area would decrease sales of Carseats by roughly 22 units
# USYes: Putting stores in US would increase sales of Carseats by 1200 roughly units.

# ---- (c) ----
# Price = beta_0 + beta_1*Price + beta_2*UrbanYes + beta_2*USYes

# ---- (d) ----
# For Price and USYes, we can reject the null hypothesis

# ---- (e) ----
lm.fit2 = lm(Sales ~ Price+US, Carseats)
summary(lm.fit2)

# ---- (f) ----
par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit2)

# They both fit the data quite well. The R^2 for the two models are all roughly 0.2393
# And from the diagnostic graphs, the residuals do not show non-linearity
# or heteroskedasticity.

# ---- (g) ----
confint(lm.fit2, level=0.95)

# ---- (h) ----
# Yes. From the diagnostic plot, there are few points with unusually high leverage.
# Their leverage exceeds (p+1)/n = (2+1)/400 = 0.0075

# -------------------- Problem 11 --------------------
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)

# ---- (a) ----
lm.fit = lm(y~x+0)
summary(lm.fit)

# The estimated coefficient is very close to the true parameter, 2.
# The t-statistic for its standard error is very high (>18), and the
# corresponding p-value is very small (<2e-16).
# Hence, the result is statistically significant.

# ---- (b) ----
lm.fit2 = lm(x~y+0)
summary(lm.fit2)

# The t-statistic for its standard error is very high (>18), and the
# corresponding p-value is very small (<2e-16).
# Hence, the result is statistically significant.

# ---- (c) ----

# The relationship is: the product of the two betas obtained in the two regressions
# equals to the squared correlation between x and y, which is 0.8.

# Or, based on the same t-statistics and p-value, 
# we can guess that the equation can be written in either way:
# y = 2x + eps or x = 0.5(y - eps)

# ---- (d) ----
# numerical
n = 100
tstat = sqrt(n-1) * t(x)%*%y / sqrt(sum(x^2)*sum(y^2) - (t(x)%*%y)^2 )
print(tstat)

# ---- (e) ----
# This is obvious, since they both use the same formula to calculate the t-statistics.
# Switching the order of x and y does not change anything.

# ---- (f) ----
lm.fit1 = lm(y~x)
summary(lm.fit1)

lm.fit2 = lm(x~y)
summary(lm.fit2)

# It is easy to see that the coefficients for both beta_1's are equal

# -------------------- Problem 12 --------------------

# ---- (b) ----
n = 100
x = rnorm(n)
y = x + rnorm(n)

lm.fit1 = lm(y~x+0)
summary(lm.fit1)

lm.fit2 = lm(x~y+0)
summary(lm.fit2)

# ---- (c) ----
x = 1:100
y = 100:1

lm.fit1 = lm(y~x+0)
summary(lm.fit1)

lm.fit2 = lm(x~y+0)
summary(lm.fit2)

# -------------------- Problem 13 --------------------
set.seed(1)

# ---- (a) ----
x = rnorm(n, 0, 1)

# ---- (b) ----
eps = rnorm(n, 0, 0.25)

# ---- (c) ----
y = -1 + 0.5 * x + eps

# length of y: 100
# beta_0 = -1
# beta_1 = 0.5

# ---- (d) ----
plot(x, y)

# ---- (e) ----
lm.fit = lm(y~x)
summary(lm.fit)

# the estimates of beta_0 and beta_1 are close to the true value

# ---- (f) ----
abline(lm.fit, col='red')
legend(-2, 0, legend='fitted line', col='red', lty=1)

# ---- (g) ----
lm.fit2 = lm(y~poly(x, 2))
summary(lm.fit2)

# The quadratic term improves the r-squared only marginally, but the coefficient
# for the quadratic term is not statistically significant

# ---- (h) ----
set.seed(1)
x = rnorm(n, 0, 1)
eps = rnorm(n, 0, 0.04)
y = -1 + 0.5 * x + eps
plot(x, y)

lm.fit2 = lm(y~x)
summary(lm.fit)

abline(lm.fit, col='red')
legend(-2, 0, legend='fitted line', col='red', lty=1)

# With less noise, the model fits better as the r-squared is drastically
# improved.

# ---- (i) ----
set.seed(1)
x = rnorm(n, 0, 1)
eps = rnorm(n, 0, 0.64)
y = -1 + 0.5 * x + eps
plot(x, y)

lm.fit3 = lm(y~x)
summary(lm.fit)

abline(lm.fit, col='red')
legend(-2, 0, legend='fitted line', col='red', lty=1)

# With more noise, the model fits worse as the r-squared is drastically
# decreased.

# ---- (j) ----
confint(lm.fit)
confint(lm.fit2)
confint(lm.fit3)

# With less noise, the confidence interval is narrower.
# With more noise, the confidence interval is wider.

# -------------------- Problem 14 --------------------

# ---- (a) ----
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

# y = beta0 + beta1*x1 + beta2*x2 + eps
# beta0 = 2, beta1 = 2, beta2 = 0.3

# ---- (b) ----
cor(x1, x2)
plot(x1, x2)

# correlation between x1 and x2 is about 0.83

# ---- (c) ----
lm.fit = lm(y~x1+x2)
summary(lm.fit)

# 1) beta0_hat = 2.13, beta1_hat = 1.44, beta2_hat = 1
# 2) beta0 is relatively close to the true beta0. beta1_hat and beta2_hat is 
# quite different from their true values
# 3) At 95% confidence leve, we cannot reject that beta1 and beta2 are 0

# ---- (d) ----
lm.fit2 = lm(y~x1)
summary(lm.fit2)

# We can reject the null hypothesis because the p-value is very small

# ---- (e) ----
lm.fit3 = lm(y~x2)
summary(lm.fit3)

# We can reject the null hypothesis because the p-value is very small

# ---- (f) ----

# No, the do not contradict each other. Because there is collinearity problem between 
# x1 and x2, it is difficult to separate their effects in the full regression
# model. Since collinearity increases the variance (s.e.) of the estimated parameters,
# they become statistically insignificant.

# Without collinearity problem, they become statistically significant in the 
# single regression model.

# ---- (g) ----
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

lm.fit = lm(y~x1+x2)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

# In this model, the new observation is not an outlier.
# However, it is a high-leverage point.

lm.fit2 = lm(y~x1)
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)

# In this model, the new observation is an outlier but still within 2-sigma.
# It is a high-leverage point.

lm.fit3 = lm(y~x2)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)

# In this model, the new observation is not an outlier.
# However, it is a high-leverage point.

# -------------------- Problem 15 --------------------
library(MASS)
attach(Boston)

# ---- (a) ----
names(Boston)
for (name in names(Boston)){
  if (name == 'crim') {next}
  lm.fit = lm(crim~Boston[[name]], Boston)
  print(name)
  print(summary(lm.fit))
}

# For all variables except chas, there is a statistically significant relationship.

# ---- (b) ----
lm.fit = lm(crim~., Boston)
summary(lm.fit)

# At 95% confidence level, we can reject the null hypothesis for
# zn, dis, rad, black, medv

# ---- (c) ----
x = vector()
for (name in names(Boston)[-1]){
  lm.fit = lm(crim~Boston[[name]], Boston)
  x = c(x, coef(lm.fit)[[2]])
}

lm.fit = lm(crim~., Boston)
y = unname(coef(lm.fit)[-1])

par(mfrow=c(1,1))
plot(x, y)

# ---- (d) ----
for (name in names(Boston)){
  if (name=='crim' | name=='chas') {next}
  print(name)
  lm.fit = lm(crim~poly(Boston[[name]], 3), Boston)
  print(summary(lm.fit))
}

# At 95% confidence level:
# black has no nonlinear relationship
# zn, rm, rad, tax, lstat has nonlinear relationship up to the 2nd degree
# indus, nox, age, dis, ptratio, medv has nonlinear relationship up to the 3rd degree