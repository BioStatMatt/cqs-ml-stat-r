## Timing operations using 'system.time'
system.time({
  for(i in 1:1e6) 
    exp(rnorm(1))
})
system.time({
  exp(rnorm(1e6))
})


## Enter debugger when error occurs
options(error=recover)

fun1 <- function(x)
  exp(x)

fun2 <- function(x) 
  sqrt(x)

fun1(fun2('a'))

## Debugger (browser) commands
## n or Enter	  Execute next statement
## s	        	Step into function
## f	        	Finish function/loop
## c	        	Continue running
## Q	        	Stop debugging

## Generate error and stop (the default) when error occurs
options(error=stop)

## Applying operations repeatedly

## Compute standard deviation of each column of a matrix using 'apply'
x <- matrix(1:24, 4, 6)
x
apply(X=x, MARGIN=2, FUN=sd)

## Get the lengths of each vector in a list
x <- list(rep(1, 5), rep(2, 8), rep(3, 10))
sapply(x, length)
lapply(x, length)

## Apply a custom function
fun <- function(x)  {
  #browser()
  median(x)
}
sapply(x, fun)
sapply(x, function(x) { median(x) })

## Additional Graphical and Numerical Summaries

load("Auto.RData")
plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
cylinders=as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower,mpg)
identify(horsepower,mpg,name)
summary(Auto)
summary(mpg)

## Attributes of objects
x <- 1
x
attr(x, 'foo') <- 'bar'
attr(x, 'bar') <- 'foo'
x
attributes(x)
exp(x)
x[1]