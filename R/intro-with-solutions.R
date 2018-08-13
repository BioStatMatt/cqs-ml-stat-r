####################
## Introduction to R
####################
## Expanded from http://www-bcf.usc.edu/~gareth/ISL/Chapter%202%20Lab.txt

## Basics
## Numeric vectors

x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)
length(x)
length(y)
x+y
x==y
x!=y
x>y
x>=y
2/10000
ls()
rm(x,y)
ls()
rm(list=ls())
?matrix
help(matrix)
help("matrix")
example("matrix")
args("matrix")
help.search("matrix")
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x
x=matrix(c(1,2,3,4),2,2)
matrix(c(1,2,3,4),2,2,byrow=TRUE)
sqrt(x)
x^2
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)
set.seed(1303)
rnorm(50)
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

## Character vectors

c("a", "b", "c")
c('a', 'b', 'c')
letters[1:3]
paste('group', letters[1:3])

## Factors

mf <- factor(m)
mf[1] < mf[2]
mf <- factor(m, ordered=TRUE)
mf[1] < mf[2]

# Indexing vectors/arrays

A=matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)
A[,1] <- rev(A[,1])

## Lists

x <- list(c(1,2,3), c('a','b'), matrix(rep(1,4),2,2))
x
x[1]
x[c(1,3)]
x[[1]]

## Data frames

as.data.frame(x)
x[[1]] <- NULL
x
x <- as.data.frame(x)
x
names(x) <- c('V1', 'V2', 'V3')
x

## Flow control

if(TRUE) { 
  'TRUE' 
} else {
  'FALSE'
}

ifelse(c('a','b','c') == 'a', 'match', 'no match')

for(i in 1:10) {
  print(i)
}

while(i >= 1) {
  print(i) 
  i <- i - 1
}

## Functions

f <- function(x) {
  x^2
}
f
f(2)
f(f(2))


## RStudio shortcuts

# <tab>                - suggest completion v1
# <control> + <up key> - suggest completion v2
# <control> + <enter>  - send current line to console 

## Installing/removing packages

install.packages("reshape2")
library("reshape2")
remove.packages("reshape2")
update.packages()

# Basic Graphics

x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")

pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()

x=seq(-pi,pi,length=50)
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)

########################
## Introduction to R Lab
########################

## 1. Look through the help file for 'plot', then recreate any figure above and set
##    the subtitle to "subtitle".

x=rnorm(100)
y=rnorm(100)
plot(x,y,sub='subtitle')

## 2. Find two different expressions to create a 3 x 3 (row x col) matrix with the
##    values 2, 4, and 6 in the rows.

matrix(rep(c(2,4,6),3),3,3,byrow=TRUE)
matrix(rep(seq(2,6,2),3),3,3,byrow=TRUE)
matrix(rep(c(2,4,6),c(3,3,3)),3,3)
t(matrix(rep(c(2,4,6),3),3,3))
cbind(rep(2,3), rep(4,3), rep(6,3))

## 3. What happens when you collate ("c()") a list and a vector? List and a list?

c(list(1,2), c(1,2))
c(list(1,2), list(1,2))

## 4. Install and load the "manipulate" package.

install.packages("manipulate")
library("manipulate")

## 5. Use the "manipulate" function to interactively vary the "phi" argument to
##    "persp" in the above example. Use persp(x,y,fa,theta=30,phi=phi_slider) 
##    as the first argument (see the "manipulate" help file and examples).

x=seq(-pi,pi,length=50)
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
fa=(f-t(f))/2
manipulate(persp(x,y,fa,theta=30,phi=phi_slider),
           phi_slider=slider(0, 90))

## 6. Generate 10 values from the normal distribution with mean 5 and sd 3 and
##    compute their sample mean.

mean(rnorm(10, 5, 3))

## 7. Use the "replicate" function to repeat item 6. 1000 times.

replicate(1000, mean(rnorm(10, 5, 3)))

## 8. Use the "hist" function to plot a histogram of the sample
##    means from item 7. Repeat where N = 50 instead of 10. Use
##    the 'add = TRUE' and 'col = "red"' arguments to the 'hist'
##    function to add the second histogram to the first for
##    for comparison.

hist(replicate(1000, mean(rnorm(10, 5, 3))), col='green')
hist(replicate(1000, mean(rnorm(50, 5, 3))), add=TRUE, col='red')



