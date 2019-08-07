library(ISLR)

X = scale(USArrests)

# Let's use the complete linkage method

# Euclidean Complete Linkage
diss.eu = dist(X)
hc.eu = hclust(diss.eu, method='complete')
plot(hc.eu, xlab='', ylab='', main='Euclidean Complete Linkage')

# Correlation Complete Linkage
diss.cor = as.dist(1-cor(t(X)))
hc.cor = hclust(diss.cor, method='complete')
plot(hc.cor, xlab='', ylab='', main='Correlation Complete Linkage')
# Unfortunately, the two plots are not equivalent


plot(diss.eu, diss.cor)
# from the scatter plot:
# 1. there is some positive relationship between the two dissimilarity measurements
# 2. the relationship is not significant