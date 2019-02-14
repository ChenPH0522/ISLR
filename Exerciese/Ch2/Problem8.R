
# ------------------- (a) ---------------------

college = read.csv('./Data/College.csv')

# ------------------- (b) ---------------------
fix(college)

rownames(college) = college[, 1]
fix(college)

college = college[, -1]
fix(college)

# ------------------- (c) ---------------------
# i.
summary(college)

# ii.
pairs(college[, 1:10])

# iii.
plot(college$Private, college$Outstate, xlab='Private', ylab='Outstate')

# iv.
Elite = rep('No', nrow(college))
Elite[college$Top10perc > 50] = 'Yes'
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(Elite)
plot(college$Elite, college$Outstate, xlab='Elite', ylab='Outstate')

# v.
par(mfrow=c(2,2))
hist(college$Books, xlab='Books', ylab='Count', col=2)
hist(college$Personal, xlab='Personal', ylab='Count', col=3)
hist(college$PhD, xlab='PhD', ylab='Count', col=4)
hist(college$Grad.Rate, xlab='Grad.Rate', ylab='Count', col=5)

# vi.
summary(college)