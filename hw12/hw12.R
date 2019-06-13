###### HW12 ######
##################
# Library
library(HSAUR3); library(gclus); library(dplyr); library(NbClust); library(cluster);

# Problem 1
# (a)
data(toothpaste, package="HSAUR3")
tp <- toothpaste[-1]
(kc = kmeans(tp, centers=2, nstart=10))
clusplot(tp, kc$cluster, color=TRUE, shade=TRUE, label=2)

# (b)
sobj = silhouette(kc$cluster, dist(tp))
plot(sobj, col=2:3)

# Problem 2
# (a)
pe <- read.csv("ProteinEurope.csv", header=TRUE)
pec <- pe[-1]
(kc = kmeans(pec, centers=2, nstart=3))
clusplot(pec, kc$cluster, color=TRUE, shade=TRUE, label=2)

# (b)
sobj = silhouette(kc$cluster, dist(pec))
plot(sobj, col=2:3)

# (c)
cl1 = subset(pec, kc$cluster=="1")
cl2 = subset(pec, kc$cluster=="2")

t.test(cl1[,1],cl2[,1])$p.value #
t.test(cl1[,2],cl2[,2])$p.value #
t.test(cl1[,3],cl2[,3])$p.value #

t.test(cl1[,4],cl2[,4])$p.value #
t.test(cl1[,5],cl2[,5])$p.value
t.test(cl1[,6],cl2[,6])$p.value #

t.test(cl1[,7],cl2[,7])$p.value
t.test(cl1[,8],cl2[,8])$p.value #
t.test(cl1[,9],cl2[,9])$p.value

# (d)
ds = dist(pe, method="euclidean")
hcst = hclust(ds, method="complete")
plot(hcst, labels=rownames(pe), cex=0.8)
rect.hclust(hcst,2)
