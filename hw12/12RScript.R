#######  12 Clustering #######
#library: gclus, dplyr, NbClust, cluster, factoextra 

# Two methods of clustering
# (1) Partitioning clustering (2) Hierarchical clustering

### 1. Optimal Number of Cluster ###
####################################
library(gclus); library(dplyr); library(NbClust); library(cluster);

data(wine)
scaled_wine = scale(wine) %>% as.data.frame()
scaled_wine2 = scaled_wine[-1]
head(scaled_wine2, 2)

# (1) Determine the best numbers of clusters using NbClust
library(NbClust);
?NbClust
# distance default is euclidean
# complete means the maximum distance between two points
NbClust(scaled_wine2, method="complete")
#hartigan : max difference btw heirarchy levels of indices
NbClust(scaled_wine2, method="complete", index="hartigan")$Best.nc

#maximum value of the index
NbClust(scaled_wine2, method="complete", index="kl")$Best.nc

# (2) Gap statistics for estimating the number of clusters
library(cluster)
?clusGap
Gap = clusGap(scaled_wine2, FUNcluster = pam, K.max=15)
print(Gap, method="firstmax")

library(factoextra);
fviz_gap_stat(Gap)

### 2. Partitioning Clustering ###
##################################
# 3 kinds of PC : K-means / PAM / CLARA

### k-means
data(iris)
head(iris)
iris.scaled = scale(iris[,-5])

library(NbClust)
nb = NbClust(iris.scaled, distance = "euclidean", min.nc = 2,
             max.nc = 10, method = "complete", index= "all")
n = nb$Best.nc[1]
kc = kmeans(iris.scaled, centers=n, nstart=4)
kc

mean(subset(iris.scaled, kc$cluster=="1")[,1])
library(cluster)
clusplot(iris.scaled, kc$cluster, color=TRUE, shade=TRUE, labels=2)

# Silhouette analysis는 resulting cluster에 대해
# separation distance에 관해 분석함
sobj = silhouette(kc$cluster, dist(iris.scaled))
summary(sobj)
plot(sobj, col=2:4)

### PAM
library(cluster)
pamx = pam(iris.scaled, 3)
summary(pamx)
plot(pamx)

### 3. Hierarchical Clustering ###
##################################
# Divisive algorithm (one cluster --> n clusters)
head(USArrests)
library(NbClust)
NbClust(USArrests, method="complete", index="hartigan")$Best.nc

ds = dist(USArrests, method="euclidean")
hcst = hclust(ds, method="complete")
plot(hcst, labels=rownames(USArrests), cex=0.8)
rect.hclust(hcst, 3)

cn = cutree(hcst, k=3)
cn
table(cn)
aggregate(USArrests, FUN=mean, by=list(cn))

ds = dist(USArrests, method="euclidean")
library(cluster)
pamd = pam(ds, 3)
plot(pamd)

sobj = silhouette(pamd)
plot(sobj, col=2:4)
