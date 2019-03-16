# ************* Quetion 2 *************
# (g)

x = 1:1e5
y = 1 - (1-1/x)^x
plot(x, y, ylim=c(0, 1))

# (h)
set.seed(1)
rec = rep(NA, 1e4)
for (i in 1:1e4){
  rec[i] = sum(sample(1:100, replace=T) == 4) > 0
}
mean(rec)