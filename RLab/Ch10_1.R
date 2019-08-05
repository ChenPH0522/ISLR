states = rownames(USArrests)
vars = colnames(USArrests)

apply(USArrests, 2, mean)
# Murder  Assault UrbanPop     Rape 
# 7.788  170.760   65.540   21.232 

apply(USArrests, 2, var)
# Murder    Assault   UrbanPop       Rape
# 18.97047 6945.16571  209.51878   87.72916

pr.out = prcomp(USArrests, scale=TRUE)
names(pr.out)
# "sdev"     "rotation" "center"   "scale"    "x"  

pr.out$center
# Murder  Assault UrbanPop     Rape 
# 7.788  170.760   65.540   21.232 

(pr.out$scale)^2
# Murder    Assault   UrbanPop       Rape 
# 18.97047 6945.16571  209.51878   87.72916

pr.out$rotation
#           PC1        PC2        PC3         PC4
# Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
# Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
# UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
# Rape     -0.5434321 -0.1673186  0.8177779  0.08902432

dim(pr.out$x)
# [1] 50  4

biplot(pr.out, scale=0)

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale=0)

pr.out$sdev
# [1] 1.5748783 0.9948694 0.5971291 0.4164494

pr.var = (pr.out$sdev)^2
pr.var
# [1] 2.4802416 0.9897652 0.3565632 0.1734301

pve = pr.var/sum(pr.var)
pve
# [1] 0.62006039 0.24744129 0.08914080 0.04335752

plot(pve, type='b', ylim=c(0, 1), xlab='Principal Component', ylab='Proportion of Variance Explained')
plot(cumsum(pve), type='b', ylim=c(0, 1), xlab='Principal Component', ylab='Cumulative Proportion of Variance Explained')

