############################################
## Linear regression and k-nearest neighbors
############################################

## Download income data

inc <- read.csv(url("http://www-bcf.usc.edu/~gareth/ISL/Income2.csv"),
                header=T, row.names = 1)

## Function to plot income data and regression function ('fun')

library('manipulate')
plot_inc_data <- function(fun = function(x1, x2) x1*x2*NA, grid=15, dat = inc) {
  ## create x,y,z data for 'persp'
  xr <- range(dat$Education)
  xs <- seq(xr[1], xr[2], length.out=grid)
  yr <- range(dat$Seniority)
  ys <- seq(yr[1], yr[2], length.out=grid)
  z  <- outer(xs, ys, fun)
  ## use 'manipulate' to interact with view of 3D graphic
  manipulate({
    ## create the 3D plot, store the view information
    tx <- persp(xs, ys, z, zlim=range(c(z,dat$Income)),
                theta=theta_slider,phi=phi_slider,
                xlab="Education", ylab="Seniority", zlab="Income")
    
    ## translate 3D data so that they can be plotted in the current 3d view
    pt <- trans3d(dat$Education, dat$Seniority, dat$Income, pmat=tx)
    
    ## add the points to the plot
    points(pt, pch=20, col='red')
  }, theta_slider=slider(0, 90, 35), phi_slider=slider(0, 90, 40))
}


## Least squares regression: 'lm' function

## additive linear
ls_fit <- lm(Income ~ Education + Seniority, data=inc)
ls_fun <- function(x1, x2, fit=ls_fit) 
  predict(fit, data.frame(Education=x1, Seniority=x2))
plot_inc_data(ls_fun)

## additive splines
library('splines')
ls_fit <- lm(Income ~ ns(Education,3) + ns(Seniority,3), data=inc)
ls_fun <- function(x1, x2, fit=ls_fit) 
  predict(fit, data.frame(Education=x1, Seniority=x2))
plot_inc_data(ls_fun)

## k-nearest neighbors regression: 'cared::knnreg' function

library('caret')
knn_fit <- knnreg(Income ~ Education + Seniority, k=10, data=inc)
knn_fun <- function(x1, x2, fit=knn_fit) 
  predict(fit, data.frame(Education=x1, Seniority=x2))
plot_inc_data(knn_fun)


## 5-fold cross-validation of knnreg model
set.seed(1985)
inc_flds  <- createFolds(inc$Income, k=5)
cvknnreg <- function(kNN = 10, flds=inc_flds) {
  cverr <- rep(NA, length(flds))
  for(tst_idx in 1:length(flds)) {
    inc_trn <- inc[-flds[[tst_idx]],]
    inc_tst <- inc[ flds[[tst_idx]],]
    knn_fit <- knnreg(Income ~ Education + Seniority, k=kNN, data=inc_trn)
    pre_tst <- predict(knn_fit, inc_tst)
    cverr[tst_idx] <- mean((inc_tst$Income - pre_tst)^2)
  }
  return(cverr)
}

## Compute 5-fold CV for kNN, where k = 1:10
cverrs <- sapply(1:20, cvknnreg)
cverrs_mean <- apply(cverrs, 2, mean)
cverrs_sd   <- apply(cverrs, 2, sd)
plot(x=1:20, y=cverrs_mean, 
     ylim=range(cverrs),
     xlab="'k' in kNN", ylab="CV Estimate of Test Error")
segments(x0=1:20, x1=1:20,
         y0=cverrs_mean-cverrs_sd,
         y1=cverrs_mean+cverrs_sd)

################################################
## Linear regression and k-nearest neighbors lab
################################################

## 1. Recreate the knn regression analysis and plot using the income data,
##    but set k = 1. Show that the prediction error (sum of squared residuals
##    is zero)

library('caret')
knn_fit <- knnreg(Income ~ Education + Seniority, k=3, data=inc)
knn_fun <- function(x1, x2, fit=knn_fit) 
  predict(fit, data.frame(Education=x1, Seniority=x2))
mean((inc$Income - predict(knn_fit, inc))^2)


## 2. Make the linear model above more complex by adding an interaction term:
##    use the formula 'Income ~ Education * Seniority'. How does this change
##    affect the training and testing errors?

ls_fit <- lm(Income ~ Education + Seniority, data=inc)
ls_fun <- function(x1, x2, fit=ls_fit) 
  predict(fit, data.frame(Education=x1, Seniority=x2))
plot_inc_data(ls_fun)
train_err_add <- mean((inc$Income - predict(ls_fit, inc))^2)

ls_fit <- lm(Income ~ Education * Seniority, data=inc)
ls_fun <- function(x1, x2, fit=ls_fit) 
  predict(fit, data.frame(Education=x1, Seniority=x2))
plot_inc_data(ls_fun)
train_err_int <- sum((inc$Income - predict(ls_fit, inc))^2)

(train_err_add - train_err_int)/train_err_int*100
## training error reduces by about 2.4%

## 3. Implement 5-fold CV for the two linear regression models, compare the 
##    CV-estimated test errors with that for the kNN models

set.seed(1985)
inc_flds  <- createFolds(inc$Income, k=5)
cvlm <- function(fit, flds=inc_flds) {
  cverr <- rep(NA, length(flds))
  for(tst_idx in 1:length(flds)) {
    inc_trn <- inc[-flds[[tst_idx]],]
    inc_tst <- inc[ flds[[tst_idx]],]
    lm_fit <- lm(formula(fit), data=inc_trn)
    pre_tst <- predict(lm_fit, inc_tst)
    cverr[tst_idx] <- mean((inc_tst$Income - pre_tst)^2)
  }
  return(cverr)
}

library('caret')
knn_fit <- knnreg(Income ~ Education + Seniority, k=3, data=inc)
knn_fun <- function(x1, x2, fit=knn_fit) 
  predict(fit, data.frame(Education=x1, Seniority=x2))
plot_inc_data(knn_fun)
mean((inc$Income - predict(knn_fit, inc))^2)
summary(cvknnreg(kNN=3, flds=inc_flds))

## additive linear
ls_fit <- lm(Income ~ Education + Seniority, data=inc)
ls_fun <- function(x1, x2, fit=ls_fit) 
  predict(fit, data.frame(Education=x1, Seniority=x2))
plot_inc_data(ls_fun)
mean((inc$Income - predict(ls_fit, inc))^2)
summary(cvlm(ls_fit, inc_flds))

## additive splines
library('splines')
ls_fit <- lm(Income ~ ns(Education,3) + ns(Seniority,3), data=inc)
ls_fun <- function(x1, x2, fit=ls_fit) 
  predict(fit, data.frame(Education=x1, Seniority=x2))
plot_inc_data(ls_fun)
mean((inc$Income - predict(ls_fit, inc))^2)
summary(cvlm(ls_fit, inc_flds))

## The training and CV errors associated with the linear regression
## methods are much smaller than those for the best kNN model.