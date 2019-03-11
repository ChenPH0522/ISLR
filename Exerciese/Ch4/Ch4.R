# **************** Question 10 ****************
library(ISLR)
attach(Weekly)

# ---- (a) ----
summary(Weekly)
cor(Weekly[, -9])
plot(Year, Today)
plot(Year, Volume)

# From the correlation matrix, we can see that there is a strong positive correlation 
# between year and volume.
# 
# Graphically, the positive correlation between year and volume is confirmed.
# 
# For the return series, there is no obvious trend. However, for some years (~2000, 2008), the returns
# are more volatile than others (1995, 2005). And the fluctuation seems periodic.

# ---- (b) ----

glm.fit = glm(Direction~.-Year-Today, data=Weekly, family=binomial)
summary(glm.fit)

# Lag2 is statistically significant at 99% confidence level

# ---- (c) ----
glm.prob = predict(glm.fit, type='response')
glm.pred = rep('Up', length(glm.prob))
glm.pred[glm.prob < 0.5] = 'Down'

table(glm.pred, Direction)
mean(glm.pred==Direction)

# Overall fraction of correct predictions is ~56.1%. However, in the original dataset,
# there are ~55.56% of 'Up'. That means, a naive classifier that always predicts 'Up'
# would achieve similar results as our trained classifier.
# 
# From the confusion matrix, the trained classifier tends to predict 'Up' when it is
# actually 'Down'. There is heavy bias towards 'Up'.

# ---- (d) ----
train = (Year <= 2008)
Weekly.train = Weekly[train, ]
Weekly.test = Weekly[!train, ]
glm.fit = glm(Direction~Lag2, data=Weekly.train, family=binomial )
glm.prob = predict(glm.fit, newdata=Weekly.test, type='response')
glm.pred = rep('Up', nrow(Weekly.test))
glm.pred[ glm.prob < 0.5 ] = 'Down'

table(glm.pred, Weekly.test$Direction)
mean(glm.pred == Weekly.test$Direction)

# Overall fraction of correct predictions is ~62.5%

# ---- (e) ----
library(MASS)

lda.fit = lda(Direction~Lag2, data=Weekly.train, family=binomial)
lda.pred = predict(lda.fit, newdata=Weekly.test)$class
table(lda.pred, Weekly.test$Direction)
mean(lda.pred==Weekly.test$Direction)

# ---- (f) ----
qda.fit = qda(Direction~Lag2, data=Weekly.train)
qda.pred = predict(qda.fit, newdata=Weekly.test)$class
table(qda.pred, Weekly.test$Direction)
mean(qda.pred == Weekly.test$Direction)

# ---- (g) ----
library(class)

set.seed(1)
train.X = cbind(Weekly.train$Lag2)
test.X = cbind(Weekly.test$Lag2)
knn.pred = knn(train.X, test.X, Weekly.train$Direction, k=1)
table(knn.pred, Weekly.test$Direction)
mean(knn.pred==Weekly.test$Direction)

# ---- (h) ----
# The logistic regression classifier and the LDA classifier provide the best results

# ---- (i) ----
set.seed(1)
train.X = cbind(Weekly.train$Lag2)
test.X = cbind(Weekly.test$Lag2)
knn.pred = knn(train.X, test.X, Weekly.train$Direction, k=3)
table(knn.pred, Weekly.test$Direction)
mean(knn.pred==Weekly.test$Direction)

# **************** Question 11 ****************

# ---- (a) ----
attach(Auto)

mpg.med = median(mpg)
mpg01 = rep(0, length(mpg))
mpg01[mpg>=mpg.med] = 1
df.Auto = data.frame(Auto, mpg01)

# ---- (b) ----
cor(df.Auto[,-9])
pairs(df.Auto[,-9])

boxplot(cylinders~mpg01)
boxplot(displacement~mpg01)
boxplot(horsepower~mpg01)
boxplot(weight~mpg01)
boxplot(acceleration~mpg01)
boxplot(year~mpg01)
boxplot(origin~mpg01)

# ---- (c) ----
n.obs = nrow(df.Auto)
train = sample( n.obs, round(n.obs/2) )
Auto.train = df.Auto[train, ]
Auto.test = df.Auto[-train, ]

# ---- (d) ----
library(MASS)

lda.fit = lda(mpg01~cylinders+displacement+horsepower+weight, data=Auto.train)
lda.pred = predict(lda.fit, newdata=Auto.test)$class
table(lda.pred, Auto.test$mpg01)
mean(lda.pred!=Auto.test$mpg01)

# test error rate is 11.7%

# ---- (e) ----
qda.fit = qda(mpg01~cylinders+displacement+horsepower+weight, data=Auto.train)
qda.pred = predict(qda.fit, newdata=Auto.test)$class
table(qda.pred, Auto.test$mpg01)
mean(qda.pred!=Auto.test$mpg01)

# test error rate is 11.7%

# ---- (f) ----
glm.fit = glm(mpg01~cylinders+displacement+horsepower+weight, data=Auto.train, family=binomial)
glm.prob = predict(glm.fit, newdata=Auto.test, type='response')
glm.pred = rep(0, nrow(Auto.test))
glm.pred[ glm.prob >= 0.5 ] = 1
table(glm.pred, Auto.test$mpg01)
mean(glm.pred!=Auto.test$mpg01)

# test error rate is 11.7%

# ---- (g) ----
library(class)

train.X = cbind(Auto.train$cylinders, Auto.train$displacement, Auto.train$horsepower, Auto.train$weight)
test.X = cbind(Auto.test$cylinders, Auto.test$displacement, Auto.test$horsepower, Auto.test$weight)

set.seed(1)
knn.pred = knn(train.X, test.X, Auto.train$mpg01, k=1)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred!=Auto.test$mpg01)

# test error rate is 14.8%

set.seed(1)
knn.pred = knn(train.X, test.X, Auto.train$mpg01, k=3)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred!=Auto.test$mpg01)

# test error rate is 15.3%

set.seed(1)
knn.pred = knn(train.X, test.X, Auto.train$mpg01, k=5)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred!=Auto.test$mpg01)

# test error rate is 16.3%

# K = 1 seems performing best on this dataset

# **************** Question 12 ****************

# ---- (a) ----
Power <- function(){  print(2^3) }
Power()

# ---- (b) ----
Power2 <- function(x, a){  print(x^a) }
Power2(3, 8)

# ---- (c) ----
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

# ---- (d) ----
Power3 <- function(x, a){ return( x^a ) }

# ---- (e) ----
x = 1:10
y = Power3(x, 2)
plot(x, y, log='y')

# ---- (f) ----
PlotPower <- function(x, a){
  y = Power2(x, a)
  plot(x, y)
}

PlotPower(1:10, 3)

# **************** Question 13 ****************
library(MASS)
attach(Boston)

crim.med = median(crim)
crim01 = rep(0, nrow(Boston))
crim01[ crim >= crim.med ] = 1
df.Boston = data.frame(Boston, crim01)

# ---- exploratory ----
cor(df.Boston)
pairs(df.Boston)
boxplot(zn~crim01, main='zn')
boxplot(indus~crim01, main='indus')
boxplot(chas~crim01, main='chas')
boxplot(nox~crim01, main='nox')
boxplot(rm~crim01, main='rm')
boxplot(age~crim01, main='age')
boxplot(dis~crim01, main='dis')
boxplot(rad~crim01, main='rad')
boxplot(tax~crim01, main='tax')
boxplot(ptratio~crim01, main='ptratio')
boxplot(black~crim01, main='black')
boxplot(lstat~crim01, main='lstat')
boxplot(medv~crim01, main='medv')

# ---- split ----
n.obs = nrow(df.Boston)
train = sample(n.obs, round(n.obs*2/3))
Boston.train = df.Boston[train, ]
Boston.test = df.Boston[-train, ]

# ---- logistic ----
glm.fit = glm(crim01~.-crim, data=Boston.train, family=binomial)
glm.prob = predict(glm.fit, newdata=Boston.test, type='response')
glm.pred = rep(0, nrow(Boston.test))
glm.pred[ glm.prob >= 0.5 ] = 1
table(glm.pred, Boston.test$crim01)
mean(glm.pred!=Boston.test$crim01)

# test error rate is 10.1%

# ---- lda ----
lda.fit = lda(crim01~.-crim, data=Boston.train)
lda.pred = predict(lda.fit, newdata=Boston.test)$class
table(lda.pred, Boston.test$crim01)
mean(lda.pred!=Boston.test$crim01)

# test error rate is 13.6%

# ---- qda ----
qda.fit = qda(crim01~.-crim, data=Boston.train)
qda.pred = predict(qda.fit, newdata=Boston.test)$class
table(qda.pred, Boston.test$crim01)
mean(qda.pred!=Boston.test$crim01)

# test error rate is 12.4%

# ---- knn ----
train.X = data.matrix(Boston[, -1][train, ])
test.X = data.matrix(Boston[, -1][-train, ])

set.seed(1)
knn.pred1 = knn(train.X, test.X, Boston.train$crim01, k=1)
table(knn.pred1, Boston.test$crim01)
mean(knn.pred1 != Boston.test$crim01)

# test error rate is 7.7%

set.seed(1)
knn.pred3 = knn(train.X, test.X, Boston.train$crim01, k=3)
table(knn.pred3, Boston.test$crim01)
mean(knn.pred3 != Boston.test$crim01)

# test error rate is 10.1%

set.seed(1)
knn.pred5 = knn(train.X, test.X, Boston.train$crim01, k=5)
table(knn.pred5, Boston.test$crim01)
mean(knn.pred5 != Boston.test$crim01)

# test error rate is 11.2%

set.seed(1)
knn.pred10 = knn(train.X, test.X, Boston.train$crim01, k=10)
table(knn.pred10, Boston.test$crim01)
mean(knn.pred10 != Boston.test$crim01)

# test error rate is 16.0%