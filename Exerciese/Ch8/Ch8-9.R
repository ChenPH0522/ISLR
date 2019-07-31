library(tree)
library(ISLR)
attach(OJ)
set.seed(1)

# ------------------------ (a) ------------------------
n.train = 800
idx.train = sample(nrow(OJ), n.train)
train = OJ[idx.train, ]
test = OJ[-idx.train, ]


# ------------------------ (b) ------------------------
tree.fit = tree(Purchase~., data=train)
summary(tree.fit)

# Training error rate: 0.1588
# Terminal nodes: 9


# ------------------------ (c) ------------------------
# Taking node (8) as an example:
# The split criterion is:
#   LoyalCH < 0.5036 --> LoyalCH < 0.280875 --> LoyalCH < 0.0356415
# There are 59 obervations in this terminal node
# The node deviance is 10.14
# The predicted class is MM
# Within this node, 1.695% of oberservations belong to CH
# 98.305% of oberservations belong to MM


# ------------------------ (d) ------------------------
plot(tree.fit)
text(tree.fit, pretty=0)


# ------------------------ (e) ------------------------
tree.pred = predict(tree.fit, newdata=test, type='class')
table(tree.pred, actual=test$Purchase)

# The test error rate is: (8+38)/270 = 0.1703704


# ------------------------ (f) ------------------------
cv.fit = cv.tree(tree.fit, FUN=prune.misclass)

# The optimal tree size is 7


# ------------------------ (g) ------------------------
plot(cv.fit$size, cv.fit$dev/n.train, type='b', xlab='size', ylab='classification error')


# ------------------------ (h) ------------------------
# The optimal tree size is 7


# ------------------------ (i) ------------------------
prune.fit = prune.misclass(tree.fit, best=7)


# ------------------------ (j) ------------------------
summary(prune.fit)
# The pruned tree has training classification error of 0.1625
# Higher than the unpruned tree


# ------------------------ (k) ------------------------
prune.pred = predict(prune.fit, newdata=test, type='class')
table(pred=prune.pred, actual=test$Purchase)

# The pruned tree has test error rate of  (8+36)/270 = 0.162963
# Lower than the unpruned tree
