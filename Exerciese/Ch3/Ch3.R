# -------------------- Problem 8 --------------------

library(ISLR)
attach(Auto)


# (a)
lm.fit = lm(mpg~horsepower)
summary(lm.fit)

# i. There is a relationship between the predictor and the response since 
# the coefficients are statistically significant