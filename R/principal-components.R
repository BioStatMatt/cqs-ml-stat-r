############################
## Principal components in R
############################

## Load USArrests data
data("USArrests")
head(USArrests)

## Center and scale data
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
USArrests <- scale(USArrests, center=TRUE, scale=TRUE)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

## compute principal components
pca <- prcomp(USArrests)
print(pca)

## pc loadings stored in 'rotation'
pca$rotation

## pc scores stored in 'x'
dim(pca$x)
head(pca$x)

## create biplot
biplot(pca)
pca$rotation=-pca$rotation
pca$x=-pca$x
biplot(pca)

## sdev of each PC
pca$sdev
## compute variance
pca$var <- pca$sdev^2
## compute proportion variabilit explained
pca$pve <- pca$var/sum(pca$var)
pca$pve
par(mfrow=c(1,2))
plot(pca$pve,
     xlab="Principal Component",
     ylab="Proportion of Variance Explained",
     ylim=c(0,1),type='b')
plot(cumsum(pca$pve),
     xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0,1),type='b')
a=c(1,2,8,-3)
cumsum(a)

par(mfrow=c(1,1))
screeplot(pca)


################################
## Principal components in R Lab
################################

## 1. Load the 'BreastCancer' data from the  'mlbench' library. Subset to the 
##    input variables (exclude 'Id' and 'Class' variables) and convert them to 
##    numeric variables using 'as.numeric'. Perform PCA on the inputs and examine
##    a biplot of the first two PCs. Attempt to interpret the first two PCs
##    based on variable loadings. Create a scree plot and look for the 'elbow'
##    to determine how many PCs are sufficient.

library('mlbench')
data('BreastCancer')
bc <- subset(BreastCancer, select=-c(Id, Class))
bc <- na.omit(apply(bc, 2, as.numeric))
bc <- scale(bc, center=TRUE, scale=FALSE)

pca <- prcomp(bc)
pca$rotation=-pca$rotation
pca$x=-pca$x
biplot(pca, pch=20)

## sdev of each PC
pca$sdev
## compute variance
pca$var <- pca$sdev^2
## compute proportion variabilit explained
pca$pve <- pca$var/sum(pca$var)
pca$pve
par(mfrow=c(1,2))
plot(pca$pve,
     xlab="Principal Component",
     ylab="Proportion of Variance Explained",
     ylim=c(0,1),type='b')
plot(cumsum(pca$pve),
     xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0,1),type='b')
