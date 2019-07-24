library(gam)
library(ISLR)
attach(Wage)
set.seed(1)

# factor selection step omitted
# use maritl, jobclass

plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

# include maritl and jobclass in GAM regression

fit.1 = gam(wage~lo(year, span=0.7)+s(age, 5)+education, data=Wage)
fit.2 = gam(wage~lo(year, span=0.7)+s(age, 5)+education+maritl, data=Wage)
fit.3 = gam(wage~lo(year, span=0.7)+s(age, 5)+education+jobclass, data=Wage)
fit.4 = gam(wage~lo(year, span=0.7)+s(age, 5)+education+maritl+jobclass, data=Wage)
anova(fit.1, fit.2, fit.3, fit.4)
# conclusion: all factors (year, age, education, maritl, jobclass) are statistically
# significant. We choose the full model, fit.4

plot(fit.4, se=TRUE, col='blue')
