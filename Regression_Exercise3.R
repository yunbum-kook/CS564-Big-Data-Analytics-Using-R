# http://www.stat.wisc.edu/~mchung/teaching/MIA/reading/GLM.logistic.Rpackage.pdf
temp <- c(27.2,27.2,27.2,27.7,27.7,27.7,28.3,28.3,28.3,28.4,28.4,28.4,29.9,29.9,29.9)
male <- c(1,0,1,7,4,6,13,6,7,7,5,7,10,8,9)
female <- c(9,8,8,3,2,2,0,3,1,3,3,2,1,0,0)
total <- male + female
pmale <- male/total

#answer for 3(a)
tur.logit.wls <- glm(pmale~temp,family=binomial,weights=total)
summary(tur.logit.wls)
#hence temperature is significantly associated with the probability of male ratio
#logit(P)= -61.3183 + 2.2110xtemp

#answer for 3(b)
exp(coef(tur.logit.wls))
confint(tur.logit.wls)
#for every one degree increase the odds of success increases by 9.1241 times 

#answer for 3(c)
tx <- c(270:300/10)
tpihat <- exp(tyhat)/(1+exp(tyhat))
tyhat <- coefficients(tur.logit.wls)[c(1)] +
  coefficients(tur.logit.wls)[c(2)]*tx
tpihat <- exp(tyhat)/(1+exp(tyhat))

plot(temp,pmale,pch=21, col=ifelse(pmale>0.5,"blue","red"),
     xlim=c(27,30), xlab="Incubation Temperature",ylab="Proportion Male") 
lines(tx,tpihat)
