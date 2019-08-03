library(e1071)
library(ISLR)
attach(Auto)
set.seed(1)


# --------------------------- (a) ---------------------------
med = median(mpg)
mpgMed = ifelse(mpg>med, 1, 0)


# --------------------------- (b) ---------------------------
Auto = cbind(Auto, mpgMed)
costs = c(0.01, 0.1, 1, 5, 10, 15, 20, 30, 50)
svm.linear.cv = tune(svm, mpgMed~.-mpg, data=Auto, kernel='linear', ranges=list(cost=costs))
err.cv = svm.linear.cv$performance$error

plot(costs, err.cv, type='b', xlab='cost', ylab='cross-validation error')

# cost = 1 leads to the lowest cross-validation error


# --------------------------- (c) ---------------------------

# SVM: polynomial kernel
dgs = 1:5
svm.poly.cv = tune(svm, mpgMed~.-mpg, data=Auto, kernel='polynomial', ranges=list(cost=costs, degree=dgs))

# best parameters are: cost=50, degree=1
# best cross-validation error is: 0.09921599


# SVM: radial kernel
gms = costs
svm.radial.cv = tune(svm, mpgMed~.-mpg, data=Auto, kernel='radial', ranges=list(cost=costs, gamma=gms))

# best parameters are: cost=5, gamma=0.1
# best cross-validation error is: 0.06294489
