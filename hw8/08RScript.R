#####  08 Analysis of Variance #####
#library: HSAUR3, Rmisc, ggplot2, psych

### 1. One-way ANOVA ###
########################

# ANOVA는 independent categorical var ~ dependent continuous var 비교
# 즉, categorical var에 속하는 그룹별 conti. var값의 차이를 체크할 수 있음
# null은 각 그룹별로 mean이 다 같다는 것
# 여기서 사용하는 statistic은 F-value
head(InspectSprays, 4)
table(InspectSpray$spray)

# a. Examine the mean diffrences
boxplot(count~spray, data=InsectSprays,
        xlab='Type of Insect Spray',
        ylab='Number of Dead Insects', col=2:7)
abline(h=mean(InsectSprays$count),col='gray')

# b. Compute ANOVA table for a fitted model
#Conducting One-way ANOVA
# aov도 되고 anove(lm(~))도 되는듯?
aov.out <- aov(count~spray, data=InspectSprays)
summary(aov.out)

an3 <- anova(lm(count ~ spray))
an3

# c. Conclude with the F test result
# qf(value, dof of category, dof of residual) = quantile of F distribution
df1 = 5; df2 = 66
a = 0.05
Fc = qf(1-a, df1, df2)
Fc

# Summary tables of model fit
print(model.tables(aov.out,"means"),digits=3)  
plot.design(InsectSprays)

# d. Perform Tukey HSD Post Hoc Test
# 각 카테고리별 mean 차이의 통계적 유의미함을 확인 가능
tkh <- TukeyHSD(aov.out, conf.level=0.95)
tkh
plot(tkh, las=1)

### 2. Two-way ANOVA ###
########################
# One-way ANOVA는 independent categorical varaible이 하나
# Two-way ANOVA는 independent categorical varaible이 두 개
# 두 독립변수 사이에 interaction이 존재 가능
# 체크해야되는 null이 3개?
# Interaction이 영향없음 / A는 영향없음 / B는 영향없음

library(HSAUR3)
head(schooldays,2)

# Summary statistics by Group
library(Rmisc)
sum = summarySE(schooldays,measurevar="absent", 
                groupvars=c("race","school"))

# Standard Error Plot Using Summary Statistics
library(ggplot2)
pd = position_dodge(0.3)
ggplot(sum, aes(x=school,y=absent,color=race)) + 
  geom_errorbar(aes(ymin=absent-se,ymax=absent+se), 
                width=0.2,size=0.7,position=pd) +
  geom_point(shape=16, size=3, position=pd) +
  scale_color_manual(values=c("red","blue")) +  
  theme(legend.position=c(0.13,0.85))

# Conducting Two-Way ANOVA
# race*school = race + school + race*school
aov2 <- aov(absent ~ race*school, data=schooldays)
schooldays$race
summary(aov2)

# Summary Tables for ANOVA Model Fits
print(model.tables(aov2,"means"),digits=4)

# Univariate Effects plot 
plot.design(absent ~ race+school, data=schooldays)

# Interaction Plot
with(schooldays, interaction.plot(x.factor=school, 
                                  trace.factor=race, response=absent, col=2:3))

### 3. Multivariate Analysis of Variance ###
############################################
# MANOVA는 dependent variable이 늘어날 때 쓰는 방법
cdt <- read.csv("ComparingColleges.csv",header=T)
attach(cdt); dim(cdt)
head(cdt)
table(cdt$School_Type)

# Conduct MANOVA
Y <- cbind(SAT,Acceptance,StudentP,PhDP,GradP,Top10P)
table(School_Type)
fit <- manova(Y ~ School_Type)
summary(fit,test="Pillai")

# 1개의 independent -> 6개의 dependent니까 6개의 ANOVA로 쪼개서 볼 수 있음
summary.aov(fit)

#page 24
tapply(StudentP,School_Type,mean)
tapply(PhDP,School_Type,mean)
tapply(Top10P,School_Type,mean)
par(mai=c(0.8,0.8,0.2,0.2),mfrow=c(1,3))
plot.design(StudentP~School_Type)
plot.design(PhDP ~ School_Type)
plot.design(Top10P ~ School_Type)

#page 25
library(psych)
db <- describeBy(cdt[,3:8],School_Type)
db$"Lib Arts"[,c(1:5,8:10,13)]
db$"Univ"[,c(1:5,8:10,13)]

