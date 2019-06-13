### HW11 ###
############
# Library
library(rpart); library(partykit)

# Problem 1
head(cu.summary)

# (a)
rfit = rpart(Mileage~Price+Country+Reliability,
             data=cu.summary, method="anova")
rfit
plot(rfit, uniform = TRUE,
     main="Regression Tree for Mileage")
text(rfit, use.n = TRUE, all=TRUE, cex=.5)

as.party(rfit)
plot(as.party(rfit), tp_args=list(id=FALSE))

# (b)
rfit$variable.importance

# Problem 2
head(kyphosis)
# (a)
rfit = rpart(Kyphosis~Age+Number+Start,
             data=kyphosis, method="class")
rfit
plot(rfit, uniform=TRUE,
     main="Regression Tree for Kyphosis")
text(rfit, use.n=TRUE, all=TRUE, cex=.5)
plot(as.party(rfit), tp_args=list(id=FALSE))

# (b)
rfit$variable.importance
