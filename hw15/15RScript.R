### PCA ###
# library
library(rela); library(psych); library(ca); library(ggplot2); library(factoextra)

### 1. Principal Components Analysis
# (1) Formulation
# By linear transformation of A, create new linearly independent
# variable Y from Y = AX

# (2) Sample Data : Economic Freedom Dataset
edat17 <- read.csv('index2017_data.csv', header=TRUE)
names(edat17)

edat <- edat17[,-c(1:2)] #exclude 1:2 columns
str(edat)

# factor -> numeric
for(i in c(1:4,7:10))
  edat[,i] <- as.numeric(edat[,i])

# (3) Assumptions for PCA
# - Sampling adequecy : KMO Test
# - Sphericity : Bartlett's Test
# - Positive Determinant of a correlation

paf_dat = paf(as.matrix(edat))
#KMO Test
paf_dat$KMO #0.87 -> Adequate sample size

#Bartlett Test
paf_dat$Bartlett

pcacor = cor(edat)
cortest.bartlett(pcacor, n=186) #p.value < 0.05 ==> Data is suitable for PCA

#Determinant Test
det(pcacor)

# (4) Number of Components
library(psych)
# General rule to select eigenvalue >= 1
scree(edat, factors=FALSE, pc=TRUE) # 2 components

nc = dim(edat)[1]
#fm="pa" : principal factor solution
#fa="pc" : principal components
fa.parallel(edat, n.obs=nc, fm="pa", fa="pc")
abline(h=1, col="grey")

# (5) Principal Components Loading
pca <- principal(edat, nfactors=2, rotate='none')
pca
# Correlation value above 0.5 will be deemed important
# In this case, x4 is not important..?

# Skip (6) Cronbach's Alpha Reliability Coefficient
alpha(pcacor) # estimate of reliability

# (7) Communality
pca$communality #h2 / Higher communalities are better

# (8) Biplots
pca <- principal(edat, nfactors=2, rotate='none')
biplot.psych(pca, col=c("black","red"), cex=c(0.5,1), arrow.len=0.08,
             main=NULL, labels=edat17[,1])

#page 15
dat <- read.csv("USA2016PresidentElection.csv",header=T)
head(dat,4)
xd <- dat[,-c(1,4,5)] #exclude state names
library(psych)
pcusa <- principal(xd, nfactors=2, rotate="none")
pcusa$loadings





