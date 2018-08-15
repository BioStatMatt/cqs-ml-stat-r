######################
## Boosting trees in R
######################

library('rpart')

## AdaBoost.M1 in complex simulated data
sim <- function(n) {
  x <- as.data.frame(matrix(rnorm(n*10),n,10))
  names(x) <- paste0('x',1:10)
  y <- ifelse(rowSums(x^2) > qchisq(0.5,10), 1, -1)
  cbind(x,y)
}

## generate training and testing data
train_dat <- sim(1000)
test_dat  <- sim(10000)

## fit a stump
stump <- function(dat, w=rep(1/nrow(dat),nrow(dat)), maxdepth=1) {
  rpart(y~., data=dat, method='class', weights=w,
        parms=list(loss=matrix(c(0,1,1,0),2,2)),
        minsplit=2,minbucket=1,maxdepth=maxdepth,
        cp=0,maxcompete=0,maxsurrogate=0,
        usesurrogate=0,xval=0)
}

## use custom sign function
sign <- function(x) ifelse(x>=0, 1, -1)

## adaboost algorithm
adaboost <- function(dat, M=10, fit=stump, ...) {
  n <- nrow(dat)
  w <- rep(1/n, n)
  l <- list()
  for(m in 1:M) {
    g <- fit(dat, w, ...)
    r <- residuals(g)
    e <- sum(w*r)/sum(w)
    a <- log((1-e)/e)
    w <- w*exp(a*r)
    l[[m]] <- list(g=g,r=r,e=e,a=a,w=w)
  }
  list(iter=l,
       G=function(x)
         sign(rowSums(sapply(l, function(m)
           as.numeric(as.character(
             predict(m$g, x, type='class')))))))
}


## Boost trees with maxdepth = 1
test_err_1 <- sapply(2^(0:10), function(m)
  mean(adaboost(train_dat, m)$G(test_dat) != test_dat$y))

## Boost trees with maxdepth = 2
test_err_2 <- sapply(2^(0:10), function(m)
  mean(adaboost(train_dat, m, maxdepth=2)$G(test_dat) != test_dat$y))

## Boost trees with maxdepth = 10
test_err_10 <- sapply(2^(0:10), function(m)
  mean(adaboost(train_dat, m, maxdepth=10)$G(test_dat) != test_dat$y))

save(test_err_1,
     test_err_2,
     test_err_10, 
     file="boosting_test_error.RData")
load(file="boosting_test_error.RData")

ylim <- range(c(
  test_err_1,
  test_err_2,
  test_err_10
))

## Plot test error as function of boosting iterations
plot(2^(0:10), test_err_1, type="b", pch=20,
     xlab="Boosting iterations",
     ylab="Test error", ylim=ylim)
lines(2^(0:10), test_err_2, type="b", pch=20,
      col="#117733")
lines(2^(0:10), test_err_10, type="b", pch=20,
      col="#882255")
legend('topright', c("maxdepth=1","maxdepth=2","maxdepth=10"), bty='n',
       lty=1, pch=20, col=c("black","#117733", "#882255"))


## AdaBoost.M1 in the Wisconsin Breast Cancer Data
## Predict whether a cancer is malignant or benign from biopsy details.
## Dimensions: 699 records, 11 attributes
## Inputs: Integer (Nominal)
## Output: Categorical, 2 class labels
## Published accuracy results: 97.36% 
## Source: https://doi.org/10.1177%2F1748301818756225
library('mlbench')
data('BreastCancer')

## Reformat data to work with 'adaboost' function
BreastCancer$y <- ifelse(BreastCancer$Class == "malignant", 1, -1)
BreastCancer$Class <- NULL
BreastCancer$Id <- NULL

## Create training/testing data
trn_idx <- sample(1:nrow(BreastCancer), size = 0.8*nrow(BreastCancer))
bc_trn <- BreastCancer[ trn_idx,]
bc_tst <- BreastCancer[-trn_idx,]

## fit with 10 iterations AdaBoost.M1
bc_fit <- adaboost(bc_trn, 100)
mean(bc_fit$G(bc_tst) == bc_tst$y)

library('caret')
bc_flds <- createFolds(bc_trn$y, k=10)
cvabm1 <- function(M, dat=bc_trn, flds=bc_flds) {
  err <- rep(NA, length(flds))
  for(idx in 1:length(flds)) {
    trn <- dat[-flds[[idx]],]
    tst <- dat[ flds[[idx]],]
    fit <- adaboost(trn, M=M)
    pre <- fit$G(tst)
    err[idx] <- mean(tst$y != pre)
  }
  return(err)
}

cverrs <- sapply(2^(0:9), cvabm1)
cverrs_mean <- apply(cverrs, 2, mean)
cverrs_sd   <- apply(cverrs, 2, sd)
plot(x=2^(0:9), y=cverrs_mean, 
     ylim=range(cverrs), pch=20,
     xlab="'k' in kNN", ylab="CV Estimate of Test Error")
segments(x0=2^(0:9), x1=2^(0:9),
         y0=cverrs_mean-cverrs_sd,
         y1=cverrs_mean+cverrs_sd)


## Redo breast cancer classifier using 'gbm'
library('gbm')
bc_trn$y <- (bc_trn$y + 1)/2 ## convert (-1, 1) to (0, 1)
bc_tst$y <- (bc_tst$y + 1)/2 ## convert (-1, 1) to (0, 1)
bc_fit <- gbm(y ~ ., distribution='bernoulli', data=bc_trn,
              n.trees=1000)
summary(bc_fit)
pre_tst <- predict(bc_fit, bc_tst, n.trees=bc_fit$n.trees, type='response') > 0.5
mean(pre_tst == bc_tst$y)

##########################
## Boosting trees in R Lab
##########################

## 1. Use the 'cv.folds' option to 'gbm' to perform 10-fold cross-validation 
##    of the 'gbm' model for breast cancer above by varying 'n.trees'. Select
##    an optimal 'n.trees' and compute the test error using the testing data.

## 2. Using the income data from previous examples to fit a boosted tree ('gbm') for
##    for predicting income, given education and seniority. Be sure to read
##    through the help file to make the appropriate modifications.
inc <- read.csv(url("http://www-bcf.usc.edu/~gareth/ISL/Income2.csv"),
                header=T, row.names = 1)

## 3. Create a figure using the 'persp' function to display the prediction surface
##    of the boosted tree model trained in part 1. (hint: use the 'plot_inc_data'
##    function from previous lab)