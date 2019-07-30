library(data.table)
library(testthat)

## generate some test data
ad <- simGLM(n = 20, t = 15,
             seed = 123, model = "poisson")
setDT(ad)
ad[, i_f := factor(i)]
ad[, t_f := factor(t)]
indep.var <- c("x1", "x2", "x3")


##------------------------------------------------------------
## MANUALLY centered dependent variable
base.form <- as.formula("y ~ x1 + x2 + x3 + i_f + t_f")
fe.form <- as.formula("y ~ x1 + x2 + x3 | i + t")

res.fe <- feglm(fe.form, data = ad, family = poisson())
res.base <- glm(base.form, data = ad, family = poisson())

## compare the independent variables
x.fe <- coef(res.fe)[indep.var]
x.base <- coef(res.base)[indep.var]
expect_equal(x.fe, x.base, tolerance = 1e-6)

## compare the fixed effects

## compare standard errors


##------------------------------------------------------------
## FEGLM centered dependent variable
