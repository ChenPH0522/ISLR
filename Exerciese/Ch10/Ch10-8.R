library(ISLR)
X = scale(USArrests)

# ----------------------- (a) -----------------------
pr = prcomp(X)
pr.pve1 = pr$sdev^2 / sum(pr$sdev^2)
# [1] 0.62006039 0.24744129 0.08914080 0.04335752

# ----------------------- (b) -----------------------
pr.scores = X %*% pr$rotation
pr.pve2 = colSums(pr.scores^2) / sum(pr.scores^2)
# PC1        PC2        PC3        PC4 
# 0.62006039 0.24744129 0.08914080 0.04335752 


# The two methods are equivalent