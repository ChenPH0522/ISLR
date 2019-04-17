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
