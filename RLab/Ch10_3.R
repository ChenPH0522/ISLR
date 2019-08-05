library(ISLR)
nci.labs = NCI60$labs
nci.data = NCI60$data

dim(nci.data)
# [1]   64 6830

table(nci.labs)
# nci.labs
# BREAST         CNS       COLON K562A-repro K562B-repro    LEUKEMIA 
# 7             5           7           1           1           6 
# MCF7A-repro MCF7D-repro    MELANOMA       NSCLC     OVARIAN    PROSTATE 
# 1           1             8               9           6           2 
# RENAL     UNKNOWN 
# 9           1 


# ------------------------------ 10.6.1 ------------------------------
pr.out = prcomp(nci.data, scale=TRUE)

cols <- function(vec){
    cols = rainbow(length(unique(vec)))
    return( cols[as.numeric(as.factor(vec))] )
}

plot(pr.out$x[, 1:2], col=cols(nci.labs), xlab='Z1', ylab='Z2', pch=19)
plot(pr.out$x[, c(1, 3)], col=cols(nci.labs), xlab='Z1', ylab='Z2', pch=19)

summary(pr.out)
plot(pr.out)

pve = (pr.out$sdev)^2 / sum((pr.out$sdev)^2)
par(mfrow=c(1, 2))
plot(pve, ylim=c(0, 1), type='b', xlab='Principal Component', ylab='Proportion of Variance Explained')
plot(cumsum(pve), ylim=c(0, 1), type='b', xlab='Principal Component', ylab='Cumulative Proportion of Variance Explained')


# ------------------------------ 10.6.2 ------------------------------
sd.data = scale(nci.data)
hc.complete = hclust(dist(sd.data), method='complete')
hc.single = hclust(dist(sd.data), method='single')
hc.average = hclust(dist(sd.data), method='average')

par(mfrow=c(1, 3))
plot(hc.complete, labels=nci.labs, xlab='', ylab='', main='Complete Linkage')
plot(hc.single, labels=nci.labs, xlab='', ylab='', main='Single Linkage')
plot(hc.average, labels=nci.labs, xlab='', ylab='', main='Average Linkage')

hc.complete.cut = cutree(hc.complete, 4)
table(hc.complete.cut, nci.labs)

par(mfrow=c(1, 1))
plot(hc.complete, labels=nci.labs, xlab='', ylab='', main='Complete Linkage')
abline(h=139, col='red')

set.seed(2)
km.out = kmeans(sd.data, centers=4, nstart=20)
km.cluster = km.out$cluster
table(km.cluster, hc.complete.cut)

hc.score.cp = hclust(dist(pr.out$x[, 1:5]), method='complete')
plot(hc.score.cp, labels=nci.labs, xlab='', ylab='', main='Principal Component Score - Complete Linkage')
table(cutree(hc.score.cp, 4), nci.labs)