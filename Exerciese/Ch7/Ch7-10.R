library(gam)
library(leaps)
library(ISLR)
attach(College)
set.seed(1)

# ------------------------ (a) ------------------------
College = College[Grad.Rate<=100, ]
n.test = nrow(College) %/% 7
idx.test = sample(1:nrow(College), n.test)
train = College[-idx.test, ]
test = College[idx.test, ]


regfit.fwd = regsubsets(Outstate~., data=train, method='forward')
fwd.summary = summary(regfit.fwd)

d.1 = which.min(fwd.summary$cp)         # model = 8
d.2 = which.min(fwd.summary$bic)        # model = 7
d.3 = which.max(fwd.summary$adjr2)      # model = 8

par(mfrow = c(1, 3))
plot(fwd.summary$cp, type='l')
plot(fwd.summary$bic, type='l')
plot(fwd.summary$adjr2, type='l')


coeffs = coef(regfit.fwd, id=6)
names(coeffs)
# by examine the graph, we choose model 6:
# Outstate ~ Private + Room.Board + Terminal + perc.alumni + Expend + Grad.Rate


# ------------------------ (b) ------------------------
fit = gam(Outstate~Private+ns(Room.Board, df=3)+ns(Terminal, df=2)+ns(perc.alumni, 3)+ns(Expend, 2), data=train)
plot(fit, se=TRUE, col='blue')



# ------------------------ (c) ------------------------
preds = predict(fit, newdata=test, type='response')
plot(test$Outstate, preds)

res = test$Outstate - preds
plot(res)
plot(density(res))

qqplot(test$Outstate, preds)

# From QQ plot, the result seems good
# Let's evalutate the R^2:

TSS = sum( (test$Outstate - mean(test$Outstate))^2 )
RSS = sum( res^2 )
r2 = 1 - RSS/TSS

# R^2 = 0.7809275, quite good


# ------------------------ (d) ------------------------
summary(fit)

# The summary results indicate that all of the variables are likely to have non-linear relationship
# with the respond variable, Outstate