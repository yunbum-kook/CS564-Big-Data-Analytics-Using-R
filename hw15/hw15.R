##### HW14 #####
###############
# Library
library(rela); library(psych); library(ca); library(ggplot2); library(factoextra)

# Problem 1
# (a)
paf_dat = paf(as.matrix(attitude))
 
paf_dat$KMO
paf_dat$Bartlett
pcacor = cor(USArrests)
cortest.bartlett(pcacor, n=186)
det(pcacor)

# (b)
scree(attitude, factors=FALSE, pc=TRUE) # 2 eigenvalues

# (c)
(pca <- principal(attitude, nfactors=2, rotate='none'))
# 0.8*rating + 0.85*complaints + 0.68*privileges +
# 0.83*learning + 0.86*raises + 0.58*advance

# (d)
fa.diagram(pca)

# Problem 2
# (a)
paf_dat = paf(as.matrix(USArrests))
paf_dat$KMO
paf_dat$Bartlett
pcacor = cor(USArrests)
cortest.bartlett(pcacor, n=186)
det(pcacor)
scree(USArrests, factors=FALSE, pc=TRUE) # 2 eigenvalues
(pca <- principal(USArrests, nfactors=2, rotate='none'))
# 0.84*Murder + 0.92*Assault + 0.86*Rape

# (b)
biplot.psych(pca, col=c("black", "red"), cex=c(0.3,1),
             arrow.len=0.08, main=NULL, labels=rownames(USArrests))
# UrbanPop & Rape, Assault, Murder
