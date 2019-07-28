err.rate <- function(pm1){
    pm2 = 1 - pm1
    err = 1 - pmax(pm1, pm2)
    return( err )
}


gini <- function(pm1){
    pm2 = 1 - pm1
    gini = pm1*(1-pm1) + pm2*(1-pm2)
    return( gini )
}


cross.entropy <- function(pm1){
    pm2 = 1 - pm1
    c.e = -1*( pm1*log(pm1) + pm2*log(pm2) )
    return( c.e )
}


pm1 = seq(0, 1, length.out=100)
e.r = err.rate(pm1)
g = gini(pm1)
c.e = cross.entropy(pm1)
x.lim = c(min(pm1), max(pm1))
y.lim = c(min(e.r, g, c.e, na.rm=TRUE), max(e.r, g, c.e, na.rm=TRUE))

plot(pm1, e.r, type='l', col='green', xlim=x.lim, ylim=y.lim, xlab='pm1', ylab='index value')
lines(pm1, g, col='red')
lines(pm1, c.e, col='blue')
legend('topright', legend=c('Classification Error', 'Gini', 'Cross Entropy'), col=c('green', 'red', 'blue'), lwd=1)