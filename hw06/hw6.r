### HW6 ###
###########
# Library
library(gmodels); library(grid); library(vcd);

# Problem 1
# (1)
dt1 <- array(c(14,10,3, 4,15,11, 7,9,5), dim=c(3,3),
             dimnames=list("Preference"=c("Music","News-talk","Sports"),
                           "Age"=c("Young","Middle","Old")))
dt1 <- as.table(dt1)
chisq.test(dt1)

# (2)
CrossTable(dt1,prop.c=TRUE,prop.r=TRUE,prop.t=FALSE,prop.chisq=FALSE,format="SPSS")
mosaic(dt1, shade=TRUE, legend=TRUE)

# Problem 2
# (1)
ar <- Arthritis[,-c(1,4)]
(tar <- table(ar))

# (2)
mosaic(tar, shade=TRUE, legend=TRUE,
       gp_labels=gpar(fontsize=4.5))

# (3)
mantelhaen.test(tar)

# (4)
doubledecker(tar)

# (5)
arfemale <- ar[which(ar$Sex=="Female"),]
arfemale <- ar[,-2]
(tarf <- table(arfemale))
mosaic(tarf, shade=TRUE, legend=TRUE, gp=shading_max)