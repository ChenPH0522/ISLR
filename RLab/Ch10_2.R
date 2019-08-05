
par(mar=c(1,1,1,1))

# ------------------- 10.5.1 -------------------
set.seed(1)
x = matrix(rnorm(50*2), ncol=2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4

km.out = kmeans(x, centers=2, nstart=20)
plot(x, col=km.out$cluster+1, xlab='x1', ylab='x2', pch=20, cex=2)


set.seed(4)
km.out = kmeans(x, centers=3, nstart=20)
plot(x, col=km.out$cluster+1, xlab='x1', ylab='x2', pch=20, cex=2)

set.seed(3)
km.out = kmeans(x, centers=3, nstart=1)
km.out$tot.withinss
# [1] 60.17588

km.out = kmeans(x, centers=3, nstart=20)
km.out$tot.withinss
# [1] 60.17588


# ------------------- 10.5.2 -------------------
hc.complete = hclust(dist(x), method='complete')
hc.average = hclust(dist(x), method='average')
hc.single = hclust(dist(x), method='single')

par(mfrow=c(1, 3))
plot(hc.complete, main='Complete Linkage', xlab='', ylab='', cex=0.9)
plot(hc.average, main='Average Linkage', xlab='', ylab='', cex=0.9)
plot(hc.single, main='Single Linkage', xlab='', ylab='', cex=0.9)

xsc = scale(x)
hc.scale.complete = hclust(dist(xsc), method='complete')
par(mfrow=c(1, 1))
plot(hc.scale.complete, main='Scaled Complete Linkage', xlab='', ylab='', cex=0.9)

x = matrix(rnorm(30*3), ncol=3)
dd = as.dist(1 - cor(t(x)))
hc.dd.complete = hclust(dd, method='complete')
plot(hc.dd.complete, main='Correlation Distance - Complete Linkage', xlab='', ylab='', cex=0.9)
