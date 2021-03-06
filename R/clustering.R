##################
## Clustering in R
##################

## K-Means Clustering

## Generate bivariate data with two clusers
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)

## For first 25/50 obs, move mean to c(3,-4)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
par(mfrow=c(1,3))
plot(x, main="Simulated data",
     xlab="", ylab="", pch=20, cex=2)

## Implement kmeans k=2
km.out=kmeans(x,2,nstart=20)
km.out$cluster
plot(x, col=(km.out$cluster+1),
     main="K-Means Clustering Results with K=2",
     xlab="", ylab="", pch=20, cex=2)
legend('topright', bty='n',
       legend=paste('Total Within SS:',
                    round(km.out$tot.withinss,2)))

## Implement kmeans k=2
set.seed(4)
km.out=kmeans(x,3,nstart=20)
plot(x, col='black',
     main="K-Means Clustering Results with K=3",
     xlab="", ylab="", pch=20, cex=2)
legend('topright', bty='n',
       legend=paste('Total Within SS:',
                    round(km.out$tot.withinss,2)))
points(x[16,2],x[12,1], col="red")

## Plot within-cluster variance as function of 'k'
twss <- sapply(2:10, function(k)
  kmeans(x,k,nstart=20)$tot.withinss)
par(mfrow=c(1,1))
plot(2:10, twss, type='b', 
     xlab='k', ylab='Total Within SS')

## Hierarchical Clustering
## Do bottom-up clustering using Euclidean distance (dist)
## with complete, average, and single linkage
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

## Plot dendrograms
par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage",
     xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage",
     xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage",
     xlab="", sub="", cex=.9)

## Create clusters by cutting tree 
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

## Plot clusters
plot(x, col=cutree(hc.complete, 2),
     main="Hierarchical: Complete Linkage k=2",
     xlab="", ylab="", pch=20, cex=2)
plot(x, col=cutree(hc.average, 2),
     main="Hierarchical: Average Linkage k=2",
     xlab="", ylab="", pch=20, cex=2)
plot(x, col=cutree(hc.single, 2),
     main="Hierarchical: Single Linkage k=2",
     xlab="", ylab="", pch=20, cex=2)

plot(x, col=cutree(hc.complete, 2),
     main="Hierarchical: Complete Linkage k=2",
     xlab="", ylab="", pch=20, cex=2)
plot(x, col=cutree(hc.complete, 3),
     main="Hierarchical: Complete Linkage k=3",
     xlab="", ylab="", pch=20, cex=2)
plot(x, col=cutree(hc.complete, 4),
     main="Hierarchical: Complete Linkage k=4",
     xlab="", ylab="", pch=20, cex=2)

plot(x, col=cutree(hc.single, 2),
     main="Hierarchical: Single Linkage k=2",
     xlab="", ylab="", pch=20, cex=2)
plot(x, col=cutree(hc.single, 3),
     main="Hierarchical: Single Linkage k=3",
     xlab="", ylab="", pch=20, cex=2)
plot(x, col=cutree(hc.single, 4),
     main="Hierarchical: Single Linkage k=4",
     xlab="", ylab="", pch=20, cex=2)

#########################
## k-means Clustering Lab
#########################

## 1. Use the 'NCI60' data from the 'ISLR' package. This is a microarray
##    data set with expression measurements on 6830 genes and 64 cancer 
##    cell lines. Treat genes as the input variable and perform PCA. Create
##    a scree plot to examine the proportion of variability explained. How
##    many PCs are needed to explain 80%, 90%, and 95% of the variability 
##    in the expression data (note that if p > n, there are at most p PCs)?
##    Perform k-means clustering using the PC scores. Use only those 
##    PCs needed to achieve 80%, 90%, and 95% of the variability. Use k=4
##    to make four clusters. Explore how this PCA dimensionality reduction affects
##    how the four clusters of cell lines group by cancer type.

##############################
## Hierarchical Clustering Lab
##############################

## 1. Use hierarchical clustering to repeat the last part of the k-means lab
##    question Use only those PCs needed to achieve 80%, 90%, and 95% of the
##    variability. Use hclust and cutree to make 4 clusters. Explore how this
##    PCA dimensionality reduction affects how the four clusters of cell lines
##    group by cancer type.

## 2. Plot a heatmap of the expression data, but instead of raw expression use
##    the 64 PCA scores. Use the 'heatmap' function. Rename the rows using the 
##    cancer type. Use the 'hclustfun' argument to change the linkage used 
##    to do hierarchical clustering (and create dendrogams for the figure).

