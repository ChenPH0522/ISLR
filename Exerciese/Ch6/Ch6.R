# ------------------ Exercise 6 ------------------

# (a)
y = 5
lambda = 1
F.ridge <- function(beta, y, lambda){
  return( (y - beta)^2 + lambda * beta^2 )
}
beta = seq(y/(1+lambda)*0.5, y/(1+lambda)*1.5, by=y/(1+lambda)/100)
plot(beta, F.ridge(beta, y, lambda), type='l')


# (b)
F.lasso <- function(beta, y, lambda){
  return( (y - beta)^2 + lambda * abs(beta) )
}

# case 1: y > 0.5*lambda
y = 5
lambda = 1
beta = seq((y-0.5*lambda)*0.5, (y-0.5*lambda)*1.5, by=(y-0.5*lambda)/100)
plot(beta, F.lasso(beta, y, lambda), type='l')


# case 2: y < -0.5*lambda
y = -5
lambda = 1
beta = seq((y+0.5*lambda)*0.5, (y+0.5*lambda)*1.5, by=(y+0.5*lambda)/100)
plot(beta, F.lasso(beta, y, lambda), type='l')


# case 3: |y| < 0.5*lambda
y = 1
lambda = 5
beta = seq(-2.5, 2.5, by=0.01)
plot(beta, F.lasso(beta, y, lambda), type='l')


# ------------------ Exercise 8 ------------------
set.seed(1)

# (a)
X = rnorm(100)
eps = rnorm(100)

# (b)
b0 = 0
b1 = 1
b2 = 2
b3 = 3
Y = b0 + b1*X + b2*X^2 + b3*X^3 + eps

# (c)
library(leaps)
data.full <- data.frame(y=Y, x=X)
model.full <- regsubsets(y~poly(x, 10, raw=T), data=data.full, nvmax=10, method='exhaustive')
model.summary <- summary(model.full)

which.min(model.summary$cp)         # selected model: max degree = 4
which.min(model.summary$bic)        # selected model: max degree = 3
which.max(model.summary$adjr2)      # selected model: max degree = 4

plot(model.summary$cp, xlab='highest degree', ylab='Cp', type='l')
points(4, model.summary$cp[4], pch=4, col='red', lwd=7)

plot(model.summary$bic, xlab='highest degree', ylab='BIC', type='l')
points(3, model.summary$bic[3], pch=4, col='red', lwd=7)

plot(model.summary$adjr2, xlab='highest degree', ylab='Adj-R2', type='l')
points(4, model.summary$adjr2[4], pch=4, col='red', lwd=7)

# we report coefficients of degree=3
coefficients(model.full, id=3)

# (Intercept) poly(x, 10, raw = T)1 poly(x, 10, raw = T)2 poly(x, 10, raw = T)3 
# 0.06150718            0.97528027            1.87620901            3.01763858 

# (d)
# FORWARD SELECTION:
model.forward <- regsubsets(y~poly(x, 10, raw=T), data=data.full, nvmax=10, method='forward')
model.for.summary <- summary(model.forward)

which.min(model.for.summary$cp)       # selected highest degree = 4
which.min(model.for.summary$bic)      # selected highest degree = 3
which.max(model.for.summary$adjr2)    # selected highest degree = 4

plot(model.for.summary$cp, xlab='highest degree', ylab='Cp', type='l')
points(4, model.for.summary$cp[4], pch=4, col='red', lwd=7)

plot(model.for.summary$bic, xlab='highest degree', ylab='BIC', type='l')
points(3, model.for.summary$bic[3], pch=4, col='red', lwd=7)

plot(model.for.summary$adjr2, xlab='highest degree', ylab='Adj-R2', type='l')
points(4, model.for.summary$adjr2[4], pch=4, col='red', lwd=7)

# we report coefficients of degree=4
coefficients(model.forward, id=3)

# (Intercept) poly(x, 10, raw = T)1 poly(x, 10, raw = T)2 poly(x, 10, raw = T)3 
# 0.06150718            0.97528027            1.87620901            3.01763858 

# BACKWARD SELECTION:
model.back <- regsubsets(y~poly(x, 10, raw=T), data=data.full, nvmax=10, method='backward')
model.back.summary <- summary(model.back)

which.min(model.back.summary$cp)       # selected highest degree = 7
which.min(model.back.summary$bic)      # selected highest degree = 5
which.max(model.back.summary$adjr2)    # selected highest degree = 7

plot(model.back.summary$cp, xlab='highest degree', ylab='Cp', type='l')
points(7, model.back.summary$cp[7], pch=4, col='red', lwd=7)

plot(model.back.summary$bic, xlab='highest degree', ylab='BIC', type='l')
points(5, model.back.summary$bic[5], pch=4, col='red', lwd=7)

plot(model.back.summary$adjr2, xlab='highest degree', ylab='Adj-R2', type='l')
points(7, model.back.summary$adjr2[7], pch=4, col='red', lwd=7)

# we report coefficients of degree=7
coefficients(model.back, id=5)

# (Intercept) poly(x, 10, raw = T)1 poly(x, 10, raw = T)3 poly(x, 10, raw = T)4 poly(x, 10, raw = T)6 
# 0.41539927            1.11046541            2.94763385            1.59460157           -0.46659252 
# poly(x, 10, raw = T)8 
# 0.04405208 

# The result does not change for forward selection, but changes for backward selection

# (e)
library(glmnet)
Xmat <- model.matrix(y~poly(x, 10, raw=T), data=data.full)[, -1]
cv.lasso <- cv.glmnet(Xmat, Y, alpha=1)
best.lambda <- cv.lasso$lambda.min       # min = 0.05001905
plot(cv.lasso)

lasso <- glmnet(Xmat, Y, alpha=1)
lasso.pred <- predict(lasso, s=best.lambda, type='coefficients')
# (Intercept)            0.1787422534
# poly(x, 10, raw = T)1  1.1326679564
# poly(x, 10, raw = T)2  1.6305908848
# poly(x, 10, raw = T)3  2.8360251216
# poly(x, 10, raw = T)4  0.0408558288
# poly(x, 10, raw = T)5  0.0003580264
# poly(x, 10, raw = T)6  .           
# poly(x, 10, raw = T)7  0.0054208543
# poly(x, 10, raw = T)8  .           
# poly(x, 10, raw = T)9  .           
# poly(x, 10, raw = T)10 .           

# (f)

# FULL SUBSET
b0 <- 0
b7 <- 7
Y <- b0 + b7*X^7 + eps
data.full <- data.frame(y=Y, x=X)

model.full <- regsubsets(y~poly(x, 10, raw=T), data=data.full, nvmax=10, method='exhaustive')
model.full.summary <- summary(model.full)

which.min( model.full.summary$cp )             # select highest degree = 2
which.min( model.full.summary$bic )            # select highest degree = 1
which.max( model.full.summary$adjr2 )          # select highest degree = 4

plot(model.full.summary$cp, xlab='highest degree', ylab='Cp', type='l')
points(2, model.full.summary$cp[2], pch=4, col='red', lwd=7)

plot(model.full.summary$bic, xlab='highest degree', ylab='BIC', type='l')
points(1, model.full.summary$bic[1], pch=4, col='red', lwd=7)

plot(model.full.summary$adjr2, xlab='highest degree', ylab='Adj-R2', type='l')
points(4, model.full.summary$adjr2[4], pch=4, col='red', lwd=7)

# show coefficients for degree=2
coefficients(model.full, id=2)
# (Intercept) poly(x, 10, raw = T)2 poly(x, 10, raw = T)7 
# 0.07049037           -0.14170843            7.00155519

# LASSO
Xmat <- model.matrix(y~poly(x, 10, raw=T), data=data.full)[, -1]

lasso.model <- cv.glmnet(Xmat, Y, alpha=1)
best.lambda <- lasso.model$lambda.min         # 13.57478
plot(lasso.model)

lasso.fit <- glmnet(Xmat, Y, alpha=1)
lasso.pred <- predict(lasso.model, s=best.lambda, type='coefficients')
# (Intercept)            0.9041878
# poly(x, 10, raw = T)1  .        
# poly(x, 10, raw = T)2  .        
# poly(x, 10, raw = T)3  .        
# poly(x, 10, raw = T)4  .        
# poly(x, 10, raw = T)5  .        
# poly(x, 10, raw = T)6  .        
# poly(x, 10, raw = T)7  6.7767971
# poly(x, 10, raw = T)8  .        
# poly(x, 10, raw = T)9  .        
# poly(x, 10, raw = T)10 .      

# Best subset method selects degree = 2 & 7, which is different from the true model.
# Lasso method selects degree = 7, which is the true model.
# Empirically, lasso method seems to have better performance


# ------------------ Exercise 9 ------------------
library(ISLR)
attach(College)
names(College)
College <- na.omit(College)

# (a)
set.seed(1)
train <- sample(nrow(College), nrow(College)/2)
test <- -(train)
X <- College[, -2]
Y <- College$Apps
X.train <- X[train, ]
Y.train <- Y[train]
X.test <- X[test, ]
Y.test <- Y[test]

# (b)
set.seed(1)
ols.fit <- lm(Apps~., data=College, subset=train)
ols.pred <- predict(ols.fit, newdata=X.test, type='response')
mean( (Y.test - ols.pred)^2 )        # 1108531

# (c)
library(glmnet)
set.seed(1)
cv.ridge <- cv.glmnet(data.matrix(X.train), Y.train, alpha=0)
best.lambda <- cv.ridge$lambda.min

ridge.pred <- predict(cv.ridge, newx=data.matrix(X.test), s=best.lambda, alpha=0, type='response')
mean( (Y.test - ridge.pred)^2 )         # 1037616

# (d)
cv.lasso <- cv.glmnet(data.matrix(X.train), Y.train, alpha=1)
best.lambda <- cv.lasso$lambda.min

lasso.pred <- predict(cv.lasso, newx=data.matrix(X.test), s=best.lambda, type='response')
mean( (Y.test - lasso.pred)^2 )         # 1107927

lasso.model <- predict(cv.lasso, newx=data.matrix(X.test), s=best.lambda, type='coefficients')
# (Intercept) -7.958176e+02
# Private      .           
# Accept       1.403062e+00
# Enroll       .           
# Top10perc    3.099707e+01
# Top25perc    .           
# F.Undergrad  .           
# P.Undergrad  .           
# Outstate     .           
# Room.Board   1.510775e-02
# Books        .           
# Personal     .           
# PhD          .           
# Terminal     .           
# S.F.Ratio    .           
# perc.alumni  .           
# Expend       6.797858e-03
# Grad.Rate    .        

# There are 4 non-zero predictors

# (e)
library(pls)

set.seed(1)
pcr.model <- pcr(Apps~., data=College, subset=train, scale=T, validation='CV')
validationplot(pcr.model)
summary(pcr.model)  # select ncomp=16

pcr.pred <- predict(pcr.model, newdata=X.test, ncomp=16)
mean( (pcr.pred - Y.test)^2 )      # 1166897

# (f)
set.seed(1)
pls.model <- plsr(Apps~., data=College, subset=train, scale=T, validation='CV')
validationplot(pls.model)
summary(pls.model)  # select ncomp=5

pls.pred <- predict(pls.model, newdata=X.test, ncomp=5)
mean( (pls.pred - Y.test)^2 )      # 1158597

# (g)
# The Test error are:
#   ridge: 1037616
#   OLS: 1108531
#   lasso: 1107927
#   pls: 1158597
#   pcr: 1166897
# They are not materially different


# ------------------ Exercise 10 ------------------
n <- 1000
p <- 20

# (a)
library(MASS)
set.seed(1)
X = mvrnorm(n, mu=integer(p), Sigma=diag(p))
beta = rnorm(p) * sample(0:1, p, replace=T)
eps = rnorm(n)
Y = X %*% beta + eps

# (b)
n.train <- 100
train = sample(n, n.train, replace=F)
test = -(train)

X.train = X[train, ]
Y.train = Y[train]
X.test = X[test, ]
Y.test = Y[test]

df.train = data.frame(y=Y.train, x=X.train)
df.test = data.frame(y=Y.test, x=X.test)
df.full = data.frame(y=Y, x=X)

# (c)
library(leaps)
bestsub.model = regsubsets( y~., data=df.train, nvmax=p )
mse.train = rep(NA, p)
colnames(df.train) = colnames(df.train, prefix='x.')

for (i in 1:p){
  coefi = coefficients(bestsub.model, id=i)
  x_cols = names(coefi)[-1]
  Y.pred = coefi[1] + data.matrix( df.train[, x_cols] ) %*% coefi[-1]
  mse.train[i] = mean( (Y.train - Y.pred)^2 )
}

plot(mse.train, type='l', xlab='Model Size', ylab='Training MSE')

# (d)
colnames(df.test) = colnames(df.test, prefix='x.')

mse.test = rep(NA, p)
for ( i in 1:p){
  coefi = coefficients(bestsub.model, id=i)
  x_cols = names(coefi)[-1]
  Y.pred = coefi[1] + data.matrix(df.test[, x_cols]) %*% coefi[-1]
  mse.test[i] = mean( (Y.pred - Y.test)^2 )
}

plot(mse.test, type='l', xlab='Model Size', ylab='Test MSE')

# (e)
id.best = which.min(mse.test)       # 11

# (f)
cofi.best = coefficients(bestsub.model, id=id.best)
# (Intercept)          x.1          x.3          x.5          x.7         x.10         x.11 
# -0.007041361  0.211638040 -0.604190458  1.165495981 -1.684541803  0.805520303  0.761756232 
# x.12         x.13         x.14         x.18         x.19 
# 0.320778629 -0.187200633 -0.562404336  1.739617667  0.879811945 

# true beta:
# Intercept = 0
# [1]  0.2353485  0.0000000 -0.6421869  0.0000000  1.0386957 -0.2835501 -1.4097291  0.0000000
# [9]  0.0000000  0.7304903  0.8791534  0.5545564 -0.2845811 -0.6746580  0.0000000  0.0000000
# [17]  0.0000000  1.6698068  0.8922593  0.0000000

# The results are fairly close.

# (g)
names(beta) = colnames(df.full, prefix='x.')[-1]
reg = rep(NA, p)

for ( i in 1:p ){
  coefi = coefficients(bestsub.model, id=i)
  selected.name = names(coefi)[-1]
  selected.coefi = coefi[-1]
  reg[i] = sqrt( sum( (beta[selected.name] - selected.coefi)^2 ) + sum( (beta[-(names(beta) %in% selected.name)])^2 ) )
}

plot(reg, type='l', xlab='Model Size')
which.min(reg)        # 6


# The best model has paramester size = 11
# The best estimates of the model parameter has parameter size = 6
# A better fit of true coefficients as measured here
# doesn't mean the model will have a lower test MSE.


# ------------------ Exercise 11 ------------------
library(MASS)
attach(Boston)
Boston = na.omit(Boston)

# (a)
set.seed(1)
train = sample(nrow(Boston), nrow(Boston)/2)
test = -(train)

# I.1 best subset
library(leaps)
bs.model = regsubsets(crim~., data=Boston, subset=train, nvmax=14, method='exhaustive')

bs.summary = summary(bs.model)
which.min(bs.summary$cp)        # 8
which.min(bs.summary$bic)       # 2
which.max(bs.summary$adjr2)     # 8

# hard to decide, use graphs
plot(bs.summary$cp, xlab='model size', ylab='Cp', type='l')
plot(bs.summary$bic, xlab='model size', ylab='BIC', type='l')
plot(bs.summary$adjr2, xlab='model size', ylab='Adj-R2', type='l')

# use 8
coefi = coefficients(bs.model, id=8)
# (Intercept)           zn          nox           rm          dis          rad      ptratio 
# 13.49137141   0.04686838 -17.03403009   1.57135072  -1.29059959   0.55947948  -0.41356877 
# lstat         medv 
# 0.17036943  -0.26409286 

# test error
x_cols = names(coefi)[-1]
X.test = Boston[test, x_cols]
Y.test = Boston$crim[test]
Y.pred = coefi[1] + data.matrix(X.test) %*% coefi[-1]
bs.test.mse = mean( (Y.pred - Y.test)^2 )    # 39.56733


# I.2 forward selection
fs.model = regsubsets(crim~., data=Boston, subset=train, nvmax=14, method='forward')
fs.summary = summary(fs.model)

which.min(fs.summary$cp)
which.min(fs.summary$bic)
which.max(fs.summary$adjr2)

# same situation, use 8
coefi = coefficients(fs.model, id=8)
# (Intercept)           zn          nox           rm          dis          rad      ptratio 
# 13.49137141   0.04686838 -17.03403009   1.57135072  -1.29059959   0.55947948  -0.41356877 
# lstat         medv 
# 0.17036943  -0.26409286

x_cols = names(coefi)[-1]
X.test = Boston[test, x_cols]
Y.test = Boston$crim[test]
Y.pred = coefi[1] + data.matrix(X.test) %*% coefi[-1]
fs.test.mse = mean( (Y.test - Y.pred)^2 )      # 39.56733


# I.3 backward selection
bks.model = regsubsets(crim~., data=Boston, subset=train, method='backward')
bks.summary = summary(bks.model)

which.min(bks.summary$cp)
which.min(bks.summary$bic)
which.max(bks.summary$adjr2)

# the same, use 8
coefi = coefficients(bks.model, id=8)
# (Intercept)           zn          nox           rm          dis          rad      ptratio 
# 13.49137141   0.04686838 -17.03403009   1.57135072  -1.29059959   0.55947948  -0.41356877 
# lstat         medv 
# 0.17036943  -0.26409286 

x_cols = names(coefi)[-1]
X.test = Boston[test, x_cols]
Y.test = Boston$crim[test]
Y.pred = coefi[1] + data.matrix(X.test) %*% coefi[-1]
bks.test.mse = mean( (Y.pred - Y.test)^2 )       # 39.56733

## The 3 methods produce exactly the same result

# II.1 Lasso
library(glmnet)
X.train = Boston[train, -1]
Y.train = Boston$crim[train]
X.test = Boston[test, -1]
Y.test = Boston$crim[test]

cv.lasso = cv.glmnet(data.matrix(X.train), Y.train, alpha=1)
best.lambda = cv.lasso$lambda.min      # 0.082852

lasso.pred = predict(cv.lasso, s=best.lambda, newx=data.matrix(X.test), type='response')
lasso.mse = mean( (Y.test - lasso.pred)^2 )     # 38.37664

lasso.model = predict(cv.lasso, s=best.lambda, newx=data.matrix(X.test), type='coefficients')
# (Intercept)  7.204613949
# zn           0.037084733
# indus       -0.034037718
# chas        -0.518884600
# nox         -9.663821264
# rm           1.187198534
# age          .          
# dis         -0.971658062
# rad          0.517060059
# tax          .          
# ptratio     -0.220503429
# black       -0.002432899
# lstat        0.176698759
# medv        -0.193172676


# II.2 Ridge
cv.ridge = cv.glmnet(data.matrix(X.train), Y.train, alpha=0)
best.lambda = cv.ridge$lambda.min     # 0.8679707

ridge.pred = predict(cv.ridge, newx=data.matrix(X.test), s=best.lambda, type='response')
ridge.mse = mean( (Y.test - ridge.pred)^2 )    # 38.37876

ridge.model = predict(cv.ridge, newx=data.matrix(X.test), s=best.lambda, type='coefficients')
# (Intercept)  2.345518586
# zn           0.033709412
# indus       -0.057715341
# chas        -0.790088255
# nox         -6.303985099
# rm           1.081377953
# age          0.006069496
# dis         -0.784154381
# rad          0.388516755
# tax          0.004574584
# ptratio     -0.118258105
# black       -0.004400293
# lstat        0.189925722
# medv        -0.152802441

# Ridge & Lasso produces similar results

# III. PCR
library(pls)
pcr.model = pcr(crim~., data=Boston, subset=train, scale=T, validation='CV')
validationplot(pls.model)

summary(pls.model)     # choose ncomp=13

pcr.pred = predict(pls.model, newdata=Boston[test, -1], ncomp=13, type='response')
pcr.mse = mean( (Y.test - pcr.pred)^2 )    # 39.27592


# (b)
# Overall, the Lasso regression seems performing better than other methods, since it has
# the lowest test error.

# Re-fit the model on the full dataset:
best.lambda = cv.lasso$lambda.min
lasso.fit = glmnet(data.matrix(Boston[, -1]), lambda=best.lambda, Boston$crim, alpha=1)
coefi = coefficients(lasso.fit)
# (Intercept) 10.518219253
# zn           0.033111052
# indus       -0.057700722
# chas        -0.537399563
# nox         -4.860715800
# rm           0.096662985
# age          .          
# dis         -0.666475145
# rad          0.501555245
# tax          .          
# ptratio     -0.135568989
# black       -0.007568141
# lstat        0.119896747
# medv        -0.137063235


# (c)
# not all features -- over fitting problem