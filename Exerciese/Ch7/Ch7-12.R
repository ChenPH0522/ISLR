library(MASS)
set.seed(1)

# create variables
n = 1e4
p = 100
X = mvrnorm(n, mu=integer(p), Sigma=diag(p))
Y = X %*% c(1:p)

n.loop = 10
b.mat = matrix(0, nrow=p, ncol=n.loop)
rss.vec = rep(0, n.loop)
for(c in 2:n.loop){
    for(r in 1:p){
        y = Y - X[, -r] %*% c(1:p)[-r]
        fit = lm(y~X[, r])
        b.mat[r, c] = fit$coefficients[2]
    }
    res = Y - X %*% b.mat[, c]
    rss.vec[c] = sum(res^2)
}

rss.vec[1] = sum(Y^2)
plot(1:n.loop, rss.vec, type='l', xlab='iteration', ylab='rss')


# We determine whether regression is good enough using RSS
# By looking at the plot, 2 iterations are enough to obtain a good approximation
# to the true regression coefficients