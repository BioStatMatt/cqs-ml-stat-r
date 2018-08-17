#############################
## Inferences vs. Predictions
#############################

####################
## Model assumptions
####################

## In linear regression we assume normal errors and have identical
## variance across all values of 'x' (i.e., homoscedasticity)
## Suppose in truth, the errors are heteroscedastic, but that we use
## ordinary linear regression anyway.
## How does this affect nominal vs actual confidence interval coverage
## for effect of 'x' on 'y'?
## How does this affect nominal vs actual test error of prediction model?

library('caret')

## simulate data from "true" model
## let errors be related to x (heteroscedasticity)
sim <- function(n=5000, int=0, beta=1) {
  x <- rnorm(n)
  y <- int + beta*x + rnorm(n)*x
  data.frame(x, y)
}

## generate Wald 95% CI under incorrect model
coi <- function(dat) {
  fit <- lm(y~x, data=dat)
  list(fit=fit, ci=confint(fit)['x',])
}

dat <- sim()
plot(dat$x, dat$y, xlab='x', ylab='y')
abline(a=0, b=1, col='orange', lwd=2)

## compute actual coverage
cis <- replicate(10000, coi(sim()), simplify = FALSE)
mean(sapply(cis, function(x) x$ci[1] < 1 && x$ci[2] > 1))


## create prediction model and estimate test error using 10-fold CV
cv <- function(k=10, pdat) {
  set.seed(42)
  flds <- createFolds(pdat$y, 10)
  errs <- rep(NA, k)
  for(i in 1:k) {
    pdat_trn <- pdat[ flds[[i]],]
    pdat_tst <- pdat[-flds[[i]],]
    pfit <- lm(y~x, pdat_trn)
    errs[i] <- mean((pdat_tst$y - predict(pfit, pdat_tst))^2)  
  }
  return(errs)
}

## 10-fold CV test error for predictions
pdat <- sim()
mean(cv(10, pdat))
## compute actual test error
pfit <- lm(y~x, pdat)
ptst <- sim(10000)
mean((ptst$y - predict(pfit, ptst))^2)

########################
## Model assumptions lab
########################

## 1. Redo the simulations above. Instead of heteroscedastic errors, let the 
##    errors have a skewed normal distribution (use 'fGarch::rsnorm').

library('fGarch')
## view skey normal density
curve(dsnorm(x, xi=3.5), from=-5, to=5)

## simulate data from "true" model
## let errors be skew normal
sim <- function(n=5000, int=0, beta=1) {
  x <- rnorm(n)
  y <- int + beta*x + rsnorm(n, xi=3.5)
  data.frame(x, y)
}

## compute actual coverage
cis <- replicate(10000, coi(sim()), simplify = FALSE)
mean(sapply(cis, function(x) x$ci[1] < 1 && x$ci[2] > 1))

## 10-fold CV test error for predictions
pdat <- sim()
mean(cv(10, pdat))
## compute actual test error
pfit <- lm(y~x, pdat)
ptst <- sim(10000)
mean((ptst$y - predict(pfit, ptst))^2)


## 2. Redo the simulations above. Instead of heteroscedastic errors, let the 
##    errors have a normal distribution. But, assume that 'y' is also related
##    to a second factor 'z' that is negatively correlated with 'x', and that
##    'z' is unmeasured (can't be used for inference/prediction). 'z' is an
##    unmeasured confounder.

## simulate data from "true" model
## let 'y' depend on unmeasured confounder 'z'
sim <- function(n=100, int=0, beta=1, gam=0.5) {
  x <- rnorm(n)
  z <- -x + rnorm(n, sd = 0.2)
  y <- int + beta*x + gam*z + rnorm(n)
  data.frame(x, y)
}

## compute actual coverage
cis <- replicate(10000, coi(sim()), simplify = FALSE)
mean(sapply(cis, function(x) x$ci[1] < 1 && x$ci[2] > 1))

## 10-fold CV test error for predictions
pdat <- sim()
mean(cv(10, pdat))
## compute actual test error
pfit <- lm(y~x, pdat)
ptst <- sim(10000)
mean((ptst$y - predict(pfit, ptst))^2)

############
## Bootstrap
############

## In the scenario above, where we have heteroscedastic errors,
## use a bootstrap confidence interval rather than the Wald interval.
## How does this affect nominal vs actual confidence interval coverage
## for effect of 'x' on 'y'?
library('boot')

## simulate data from "true" model
## let errors be related to x (heteroscedasticity)
sim <- function(n=200, int=0, beta=1) {
  x <- rnorm(n)
  y <- int + beta*x + rnorm(n)*x
  data.frame(x, y)
}

## create Wald 95% CI under incorrect model
coi_Wald <- function(dat) {
  fit <- lm(y~x, data=dat)
  list(fit=fit, ci=confint(fit)['x',])
}

## create a bootstrap 95% CI under incorrect model
coi_Boot <- function(dat) {
  cat(".")
  stat <- function(dat, idx)
    coef(lm(y~x, dat[idx,]))['x']
  bs <- boot(data=dat, statistic=stat, R=500)
  ci <- boot.ci(bs, type='basic') 
  list(boot=bs, ci=ci$basic[,4:5])
}

## compute actual coverage for Wald interval
cis_Wald <- replicate(500, coi_Wald(sim()), simplify = FALSE)
mean(sapply(cis_Wald, function(x) x$ci[1] < 1 && x$ci[2] > 1))

## compute actual coverage for bootstrap interval
## this may take a long time since coi_Boot computes many
## resamples to create just one interval, and the expression
## below creates many intervals in order to approximate coverage
## result: about 0.944
cis_Boot <- replicate(500, coi_Boot(sim()), simplify = FALSE)
mean(sapply(cis_Boot, function(x) x$ci[1] < 1 && x$ci[2] > 1))

################
## Bootstrap lab
################

## 1. Rerun the simulations above under the unmeasured confounding scenario
##    (question 2 in the modeling assumptions lab). Does the bootstrap interval
##    have better coverage than the Wald interval?

## simulate data from "true" model
## let 'y' depend on unmeasured confounder 'z'
sim <- function(n=200, int=0, beta=1, gam=0.5) {
  x <- rnorm(n)
  z <- -x + rnorm(n, sd = 0.2)
  y <- int + beta*x + gam*z + rnorm(n)
  data.frame(x, y)
}

## compute actual coverage for Wald interval
cis_Wald <- replicate(500, coi_Wald(sim()), simplify = FALSE)
mean(sapply(cis_Wald, function(x) x$ci[1] < 1 && x$ci[2] > 1))

## compute actual coverage for bootstrap interval
## this may take a long time since coi_Boot computes many
## resamples to create just one interval, and the expression
## below creates many intervals in order to approximate coverage
## result: about 0.000
cis_Boot <- replicate(500, coi_Boot(sim()), simplify = FALSE)
mean(sapply(cis_Boot, function(x) x$ci[1] < 1 && x$ci[2] > 1))

##############
## Sample bias
##############

## Suppose that a study is designed such that samples with larger values of 'y'
## are less likely to be observed.
## How does this affect nominal vs actual confidence interval coverage
## for effect of 'x' on 'y'?
## How does this affect nominal vs actual test error of prediction model?

## simulate data from "true" model
## omit samples with probability 0.5 if y > 1
sim <- function(n=200, int=0, beta=1, kprob=0.5) {
  x <- rnorm(n)
  y <- int + beta*x + rnorm(n)
  keep <- as.logical(rbinom(n, 1, prob = ifelse(y > 1.0, kprob, 1)))
  data.frame(x=x[keep], y=y[keep])
}

dat <- sim()
plot(dat$x, dat$y)
abline(a=0,b=1)
abline(lm(y~x,dat))

## compute actual coverage for Wald interval
cis_Wald <- replicate(500, coi_Wald(sim()), simplify = FALSE)
mean(sapply(cis_Wald, function(x) x$ci[1] < 1 && x$ci[2] > 1))

## 10-fold CV test error for predictions
pdat <- sim()
mean(cv(10, pdat))
## compute actual test error
pfit <- lm(y~x, pdat)
ptst <- sim(10000, kprob=1)
mean((ptst$y - predict(pfit, ptst))^2)

##################
## Sample bias lab
##################

## 1. Redo the simulation above. Does the bootstrap interval have better actual
##    coverage than the Wald interval under sample bias?

## compute actual coverage for bootstrap interval
## this may take a long time since coi_Boot computes many
## resamples to create just one interval, and the expression
## below creates many intervals in order to approximate coverage
## result: about 0.838
cis_Boot <- replicate(500, coi_Boot(sim()), simplify = FALSE)
mean(sapply(cis_Boot, function(x) x$ci[1] < 1 && x$ci[2] > 1))
