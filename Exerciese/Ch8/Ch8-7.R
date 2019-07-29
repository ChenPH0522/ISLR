library(randomForest)
library(MASS)
attach(Boston)
set.seed(1)

idx.train = sample(nrow(Boston), nrow(Boston)%/%2)
train = Boston[idx.train, ]
test = Boston[-idx.train, ]

mtry.range = 1:14
ntree.range = 1:500

mse.mat = matrix(0, nrow=max(ntree.range), ncol=max(mtry.range))
for(m in mtry.range){
    for(t in ntree.range){
        cat('\nTraining', m, t, 'Trees...')
        rf.fit = randomForest(medv~., data=train, mtry=m, ntree=t, importance=TRUE)
        pred = predict(rf.fit, newdata=test)
        mse.mat[t, m] = mean( (pred-test$medv)^2 )
    }
}


lab = paste('m =', mtry.range)
cols = rainbow(max(mtry.range))
x.lim = c(min(mtry.range), max(mtry.range))
y.lim = c(min(mse.mat), max(mse.mat))
plot(ntree.range, mse.mat[, 1], type='l', col=cols[1], xlim=x.lim, ylim=y.lim, xlab='number of trees', ylab='mse')
for(m in mtry.range[-1]){
    lines(mse.mat[, m], col=cols[m])
}
legend('topright', legend=lab, col=cols, lwd=1, x.intersp=0.5, y.intersp=0.75)