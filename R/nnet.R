#############################
## Basic Neural Networks in R
#############################

library('rgl')
library('ElemStatLearn')
library('nnet')

## load binary classification example data
data("mixture.example")
dat <- mixture.example

## create 3D graphic, rotate to view 2D x1/x2 projection
par3d(FOV=1,userMatrix=diag(4))
plot3d(dat$xnew[,1], dat$xnew[,2], dat$prob, type="n",
       xlab="x1", ylab="x2", zlab="",
       axes=FALSE, box=TRUE, aspect=1)
## plot points and bounding box
x1r <- range(dat$px1)
x2r <- range(dat$px2)
pts <- plot3d(dat$x[,1], dat$x[,2], 1,
              type="p", radius=0.5, add=TRUE,
              col=ifelse(dat$y, "orange", "blue"))
lns <- lines3d(x1r[c(1,2,2,1,1)], x2r[c(1,1,2,2,1)], 1)
## draw Bayes (True) classification boundary
dat$probm <- with(dat, matrix(prob, length(px1), length(px2)))
dat$cls <- with(dat, contourLines(px1, px2, probm, levels=0.5))
pls <- lapply(dat$cls, function(p) lines3d(p$x, p$y, z=1))
## plot marginal probability surface and decision plane
sfc <- surface3d(dat$px1, dat$px2, dat$prob, alpha=1.0,
            color="gray", specular="gray")
qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
            color="gray", lit=FALSE)

## fit single hidden layer NN with 10 units
## size - number of units (neurons) in hidden layer
fit.nn <- nnet(x=dat$x, y=dat$y, size=10, entropy=TRUE, decay=0)
preds.nn <- predict(fit.nn, dat$xnew, type="class")
probs.nn <- predict(fit.nn, dat$xnew, type="raw")[,1]

## plotting: clear the surface, decision plane, and decision boundary
par3d(userMatrix=diag(4)); pop3d(id=sfc); pop3d(id=qds)
for(pl in pls) pop3d(id=pl)
dat$probm.nn <- with(dat, matrix(probs.nn, length(px1), length(px2)))
dat$cls.nn <- with(dat, contourLines(px1, px2, probm.nn, levels=0.5))
## plot classification boundary
pls <- lapply(dat$cls.nn, function(p) lines3d(p$x, p$y, z=1))
## plot probability surface and decision plane
sfc <- surface3d(dat$px1, dat$px2, probs.nn, alpha=1.0,
                 color="gray", specular="gray")
qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
               color="gray", lit=FALSE)

## fit single hidden layer NN with 3 units
## size - number of units (neurons) in hidden layer
fit.nn <- nnet(x=dat$x, y=dat$y, size=3, entropy=TRUE, decay=0)
preds.nn <- predict(fit.nn, dat$xnew, type="class")
probs.nn <- predict(fit.nn, dat$xnew, type="raw")[,1]

## plotting: clear the surface, decision plane, and decision boundary
par3d(userMatrix=diag(4)); pop3d(id=sfc); pop3d(id=qds)
for(pl in pls) pop3d(id=pl)
dat$probm.nn <- with(dat, matrix(probs.nn, length(px1), length(px2)))
dat$cls.nn <- with(dat, contourLines(px1, px2, probm.nn, levels=0.5))
## plot classification boundary
pls <- lapply(dat$cls.nn, function(p) lines3d(p$x, p$y, z=1))
## plot probability surface and decision plane
sfc <- surface3d(dat$px1, dat$px2, probs.nn, alpha=1.0,
                 color="gray", specular="gray")
qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
               color="gray", lit=FALSE)

## fit single hidden layer NN with 10 units and weight decay
## size - number of units (neurons) in hidden layer
fit.nn <- nnet(x=dat$x, y=dat$y, size=10, entropy=TRUE, decay=0.02)
preds.nn <- predict(fit.nn, dat$xnew, type="class")
probs.nn <- predict(fit.nn, dat$xnew, type="raw")[,1]

## plotting: clear the surface, decision plane, and decision boundary
par3d(userMatrix=diag(4)); pop3d(id=sfc); pop3d(id=qds)
for(pl in pls) pop3d(id=pl)
dat$probm.nn <- with(dat, matrix(probs.nn, length(px1), length(px2)))
dat$cls.nn <- with(dat, contourLines(px1, px2, probm.nn, levels=0.5))
## plot classification boundary
pls <- lapply(dat$cls.nn, function(p) lines3d(p$x, p$y, z=1))
## plot probability surface and decision plane
sfc <- surface3d(dat$px1, dat$px2, probs.nn, alpha=1.0,
                 color="gray", specular="gray")
qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
               color="gray", lit=FALSE)


#################################
## Basic Neural Networks in R Lab
#################################

## 1. Using the income data from previous examples to fit a neural network for
##    for predicting income, given education and seniority. Be sure to read
##    through the help file to make the appropriate modifications.
inc <- read.csv(url("http://www-bcf.usc.edu/~gareth/ISL/Income2.csv"),
                header=T, row.names = 1)

## 2. Create a figure using the 'persp' function or functions from the 'rgl' 
##    package to display the prediction surface of the NN trained in part 1.
##    (hint: use the 'plot_inc_data' function from previous lab)

## 3. Use the 'decay' parameter as a tuning parameter. Use 10-fold CV to select
##    a value for this parameter.
