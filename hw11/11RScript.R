#######  11 Recursive Partitioning #######
#library: TH.data, rpart, grid, mvtnorm, partykit, rattle
library(TH.data); library(rpart); library(grid); library(mvtnorm); library(partykit); library(rattle); library(quint);

#### 1. Regression Trees ####
#############################
# Regression Tree는 1개 혹은 그 이상의 continuous or categorical predictor (x)가 주어졌을 때,
# continuous dependent variables를 예측하는 것.

data(bodyfat, package = "TH.data")
head(bodyfat)

rfit <- rpart(DEXfat~age+waistcirc+hipcirc+elbowbreadth+kneebreadth,
              data=bodyfat, method="anova", control=rpart.control(minsplit=10))
rfit

plot(rfit, uniform=TRUE,
     main="Regression Tree for bodyfat")
text(rfit, use.n=TRUE, all=TRUE, cex=.8)

plot(as.party(rfit), tp_args=list(id=FALSE))

DEXfat_pred = predict(rfit, newdata=bodyfat)
xlim = range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data=bodyfat, xlab='Observed',
     ylab="Predicted", ylim=xlim, xlim=xlim, pch=21, bg='cyan')
abline(a=0, b=1, col="red")

#### 2. Classification Trees ####
#################################
# Regression Tree는 1개 혹은 그 이상의 continuous or categorical predictor (x)가 주어졌을 때,
# categorical dependent variables를 예측하는 것.
data(GlaucomaM, package="TH.data")
dim(GlaucomaM)
head(GlaucomaM)
table(GlaucomaM$Class) #dependent variable

cfit = rpart(Class ~ ., data=GlaucomaM, method="class")
cfit
sub1 <- subset(GlaucomaM, varg<0.209)
table(sub1$Class)

win.graph(8,7.5)
plot(as.party(cfit), tp_args=list(id=FALSE))

fancyRpartPlot(cfit)

#### 3. Qualitative Interaction Trees ####
##########################################

data(bcrp); head(bcrp, 4)

form1 = I(cesdt1-cesdt3) ~ cond | nationality+marital + wcht1 + age +
  trext + comorbid + disopt1 + uncomt1 + negsoct1
control1 <- quint.control(maxl=6, B=2)

#Perfomr a quint analysis. We exclude cond=3(standard care)
quint1 = quint(form1, data=subset(bcrp, cond<3), control=control1)

#Split information
quint1$si

#Visualization of Qualitative Interaction Tree
plot(quint1)
form2 = I(physt3-physt1) ~ cond | cesdt1+negsoct1+uncomt1+
  disopt1 + comorbid + age + wcht1 + nationality + marital + trext
quint2 = quint(form2, data=subset(bcrp, cond<3), quint.control(maxl=6, B=2))
plot(quint2)

# Leaf information
quint2$li
