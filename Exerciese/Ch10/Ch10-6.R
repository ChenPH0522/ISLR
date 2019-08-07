set.seed(1)

control = matrix(rnorm(1000*50), ncol=50)
treatment = matrix(rnorm(1000*50), ncol=50)
x = cbind(control, treatment)

# ** the -18 to 18 is hard-coded. This value ensures the first principal component
# explains ~10% of the total variance
x[1, ] = seq(-18, 18-0.36, 0.36)
# suppose one gene is activated/de-activated by one type of machine
# and also the gene is related to time of process


pr.out = prcomp(scale(x))
summary(pr.out)$importance[, 1]
# the first principal component explains 9.911% of total variance

# ** the choice of 10 vs 0 encoding is hard-coded
# it ensures the first principal component improves PVE by a significant amount
new.feature = c(rep(10, 50), rep(0, 50))
x = rbind(x, new.feature)
pr.out = prcomp(scale(x))
summary(pr.out)$importance[, 1]
# the first principal component now explains 11.55% of total variance
# the explanating power of the first principal component is improved