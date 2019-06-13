##### 06 Chi-Suare Test #####

#library: gplots, gmodels, sjPlot, grid, vcd

## 1. Chi-square test of goodness of fit
# 어떤 한 종류의 데이터에 대해 expected distribution과
# 주어진 분포가 얼마나 차이나는지 확인

# dchisq = cdf of chi-square distribution
# qchisq = quantile of chi-square distribution
#   qchisq(value, degree of freedom)
a=0.05; (qc=qchisq(1-a, df=7))

# Chi-square distribution
curve(dchisq(x, df=7), 0, 20, 200,
      col=4, ylab="Probability",
      xlab=expression(chi^2))
abline(v=qc, col=2)

# Table formulation
O = c(169,58,56,18,253,45,38,90)
tc = c("Frequency")
tr = c("Chinese","Indian","Korean","Maori","NZ European",
       "Other European","Pacific","Other")
mo = matrxi(O, dimnames=list(tr,tc))
as.table(mo)

# chisq.test(x)
# chisquare test를 하는데, null은 category의 frequency가 equllay divided
chisq.test(mo)
prop.table(mo)

n= sum(O); k=8; E=rep(n/k,k)
cgram <- (O-E)/sqrt(E)
barplot(cgram, col=ifelse(cgram>0, "red", "blue"),
        names.arg = tr)

## 2. Chi-Square Test of Independence
# 2개 혹은 그 이상의 카테고리들의 독립성을 체크할 수 있음

# array로 cross tabulation에 쓰일 table 만들기
dt1 <- array(c(11,23,22, 33,14,13, 7,9,14), dim=c(3,3),
             dimnames=list("Residence City"=c("Boston","Montreal","Montpellier"),
                           "Favorite Baseball Team"=c("Blue Jays","Red Socks","Yankees")))
(dt1 <- as.table(dt1))

# Table 형식으로 p-value와 각종 수치들 확인할 수 있음
library(gmodels)
CrossTable(dt1,prop.c=FALSE,prop.chisq=FALSE,prop.t=FALSE,
           expected=TRUE,format="SPSS")


#page 13
library(gplots)
balloonplot(t(dt1), label=TRUE, show.margins=FALSE,
            main="Balloon Plot for Residence City by Baseball Team")


# Cross-tabulation analysis by chisq.test
(ct1 <- chisq.test(dt1))

curve(dchisq(x,df=4),0,20,200,xlab= expression(chi^2),
      ylab="Probability Density")
qc <- qchisq(1-0.05,df=4)
abline(v=qc,col=4) #critical chi-square
abline(v=ct1$statistic,col=2) #statistic chi-square
legend("topright",c("Critical","Statistic"),lty=1,
       col=c(4,2))


#page 15
dt2 <- read.csv("SurveyData.csv",header=T)
head(dt2[,1:6],4)

#page 16
University <- factor(dt2$univ,levels=1:2,labels=c("Y","K"))
#c4 : I accept quickly a new fashion. (negative 1-5 positive)
FashionAcceptance <- factor(dt2$c4)
tb2 <- table(University,FashionAcceptance)
tb2
addmargins(tb2)

#page 16
library(gplots)
balloonplot(t(tb2), label=TRUE, show.margins=FALSE,
            main="Balloon Plot for Two Universities by FashionAcceptance")

#page 17
library(sjPlot)
set_theme(geom.label.size=4,axis.textsize=1.1,
          legend.pos="bottom")
sjp.xtab(University,FashionAcceptance,type="bar",y.offset=0.01,
         margin="row",coord.flip=TRUE,wrap.labels=7,
         geom.colors="Set2",show.summary=TRUE)


# Three-way table을 chisquare test로 분석하기
bs <- read.csv("BoyScout.csv",header=TRUE)
bs

# null : resonse variable이 나머지 두 개에 대해 independent하다
str(bs)
bs$Socio <- ordered(bs$Socio,
                    levels=c("Low", "Medium", "High"))
str(bs)

# data.frame -> three-way table
bs3 <- xtabs(Frequency~Socio+Scout+Delinquent, data=bs)
bs3
ft3 <- ftable(bs3)
ft3
prob.table(ft3, 1)
chisq.test(ft3)
#page 22
library(grid); library(vcd)
mosaic(bs3,shade=TRUE,legend=TRUE)
