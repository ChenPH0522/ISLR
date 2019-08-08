set.seed(1)


# --------------------------------- (a) ---------------------------------
n = 60
p = 50
X = matrix( rnorm(n*p), ncol=p )
y = c(rep(0, n/3), rep(1, n/3), rep(2, n/3))
X[y==1, ] = X[y==1, ] + 1
X[y==2, ] = X[y==2, ] + 2


# --------------------------------- (b) ---------------------------------
pr = prcomp(X)
pr.score = X %*% pr$rotation
plot(pr.score[, 1:2], col=(4-y), xlab='PC1', ylab='PC2')


# --------------------------------- (c) ---------------------------------
km.3 = kmeans(X, centers=3, nstart=20)
table(pred=km.3$cluster, actual=y)
#       actual
# pred  0   1   2
# 1     0   20  0
# 2     0   0   20
# 3     20  0   0
# The Kmeans algorithm seems correctly cluster the groups


# --------------------------------- (d) ---------------------------------
km.2 = kmeans(X, centers=2, nstart=20)
table(pred=km.2$cluster, actual=y)
#       actual
# pred  0   1   2
# 1     0   3   20
# 2     20  17  0


# --------------------------------- (e) ---------------------------------
km.4 = kmeans(X, centers=4, nstart=20)
table(pred=km.4$cluster, actual=y)
#       actual
# pred  0   1   2
# 1     0   20  0
# 2     10  0   0
# 3     10  0   0
# 4     0   0   20


# --------------------------------- (f) ---------------------------------
km.3.pr = kmeans(pr.score[, 1:2], centers=3, nstart=20)
table(pred=km.3.pr$cluster, actual=y)
#       actual
# pred  0   1   2
# 1     0   20  0
# 2     20  0   0
# 3     0   0   20

# Clustering using the first 2 PC achieves the same clustering results as
# clustering using all dimensions


# --------------------------------- (g) ---------------------------------
X = scale(X)
km.3.std = kmeans(X, centers=3, nstart=20)
table(pred=km.3.std$cluster, actual=y)
#       actual
# pred  0   1   2
# 1     0   20  0
# 2     20  0   0
# 3     0   0   20

# For this particular example, the result is the same
# since the oberservations are generated from standard normal distribution
# if the observations' different dimensions are measured in different units
# then the result might be different.