####################################################
## Classification/regression trees and random forest
####################################################

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


## default tuning parameters
library('rpart')
tree_fit <- rpart(Income ~ Education + Seniority, data = inc)
tree_fun <- function(x1, x2, fit=tree_fit)
  predict(fit, data.frame(Education=x1, Seniority=x2))
plot_inc_data(tree_fun)  
plot(tree_fit)
text(tree_fit, xpd=NA)

## modify tuning parameters; allow tree to grow more complex
tree_fit <- rpart(Income ~ Education + Seniority, data = inc,
                  control=rpart.control(minsplit=2, minbucket=1))
tree_fun <- function(x1, x2, fit=tree_fit)
  predict(fit, data.frame(Education=x1, Seniority=x2))
plot_inc_data(tree_fun)  
plot(tree_fit)
text(tree_fit, xpd=NA)

## fit tree to prostate screening data (predict lcavol)
library('ElemStatLearn')
data(prostate)
help(prostate)
prostate$train <- NULL
tree_fit <- rpart(lcavol ~ ., data=prostate)
plot(tree_fit)
text(tree_fit, xpd=NA)

## 5-fold CV for prostate data
set.seed(1985)
pro_flds  <- createFolds(prostate$lcavol, k=5)
cvrpart <- function(minsplit, flds=pro_flds) {
  cverr <- rep(NA, 5)
  for(tst_idx in 1:5) {
    pro_trn <- prostate[-flds[[tst_idx]],]
    pro_tst <- prostate[ flds[[tst_idx]],]
    tree_fit <- rpart(lcavol ~ ., data=pro_trn,
                      control=rpart.control(minsplit=minsplit, minbucket=1))
    pre_tst <- predict(tree_fit, pro_tst)
    cverr[tst_idx] <- mean((pro_tst$lcavol - pre_tst)^2)
  }
  return(cverr)
}

## Compute 5-fold CV for rpart, minsplit = 20:5
cverrs <- sapply(20:1, cvrpart)
cverrs_mean <- apply(cverrs, 2, mean)
cverrs_sd   <- apply(cverrs, 2, sd)
plot(x=20:1, y=cverrs_mean, 
     ylim=range(cverrs),
     xlab="minsplit", ylab="CV Estimate of Test Error")
segments(x0=20:1, x1=20:1,
         y0=cverrs_mean-cverrs_sd,
         y1=cverrs_mean+cverrs_sd)


## Random Forest for Regression
library('randomForest')
data(airquality)
set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)

## Show "importance" of variables: higher value mean more important:
## For each tree, the prediction error on the out-of-bag portion of
## the data is recorded (error rate for classification, MSE for
## regression). Then the same is done after permuting each predictor variable.
## The difference between the two are then averaged over all trees.

round(importance(ozone.rf), 2)
varImpPlot(ozone.rf)
ozone.rf$mse


########################################################
## Classification/regression trees and random forest lab
########################################################

## 1. Implement 5-fold CV for the random forest on the airquality data,
##    varying the value of mtry

## 10-fold CV for airquality data
set.seed(1985)
airquality <- na.omit(airquality)
dia_flds  <- createFolds(airquality$Ozone, k=10)
cvrf <- function(mtry, flds=dia_flds) {
  cverr <- rep(NA, 10)
  for(tst_idx in 1:10) {
    dia_trn <- airquality[-flds[[tst_idx]],]
    dia_tst <- airquality[ flds[[tst_idx]],]
    diab.rf <- randomForest(Ozone ~ ., data=dia_trn,
                        mtry=mtry, importance=TRUE, na.action=na.omit)
    pre_tst <- predict(diab.rf, dia_tst)
    cverr[tst_idx] <- mean((dia_tst$Ozone - pre_tst)^2)
  }
  return(cverr)
}

## Compute 10-fold CV for randomForest, mtry = 1:5
cverrs <- sapply(1:5, cvrf)
cverrs_mean <- apply(cverrs, 2, mean)
cverrs_sd   <- apply(cverrs, 2, sd)
plot(x=1:5, y=cverrs_mean, 
     ylim=range(cverrs),
     xlab="mtry", ylab="CV Estimate of Test Error")
segments(x0=1:5, x1=1:5,
         y0=cverrs_mean-cverrs_sd,
         y1=cverrs_mean+cverrs_sd)
