
# ------------------------------ (a) ------------------------------
gene = read.csv('./Data/Ch10Ex11.csv', header=F)


# ------------------------------ (b) ------------------------------
diss = as.dist(1-cor(gene))

hc.cmp = hclust(diss, method='complete')
plot(hc.cmp, xlab='', main='Complete Linkage')

hc.single = hclust(diss, method='single')
plot(hc.single, xlab='', main='Single Linkage')

hc.avg = hclust(diss, method='average')
plot(hc.avg, xlab='', main='Average Linkage')

hc.cent = hclust(diss, method='centroid')
plot(hc.cent, xlab='', main='Centroid Linkage')

# The grouping does differ by different linkage at choice
# By far the most reasonable result is obtained using complete linkage
# Using this linkage, the samples are separated into two groups


# ------------------------------ (c) ------------------------------
# one method is to compare the distribution of each gene given the two groups
# but for this problem, since there are too many genes, we can check the 
# loading factor of the first PC
# then cross-check by plotting its distribution

pr = prcomp(gene)
gene.max = which.max( abs(pr$rotation[, 1]) )     # gene #22

clust = cutree(hc.cmp, 2)
dist.1 = density( as.numeric( gene[gene.max, which(clust==1) ] ) )
dist.2 = density( as.numeric( gene[gene.max, which(clust==2) ] ) )

x.lim = c( min(dist.1$x, dist.2$x), max(dist.1$x, dist.2$x) )
y.lim = c( min(dist.1$y, dist.2$y), max(dist.1$y, dist.2$y) )
plot(dist.1, col='red', xlim=x.lim, ylim=y.lim, xlab='Gene 22', ylab='Density', main='Gene 22 Density Plot')
lines(dist.2, col='blue')
legend('topright', legend=c('Cluster 1', 'Cluster 2'), col=c('red', 'blue'), lwd=1)
