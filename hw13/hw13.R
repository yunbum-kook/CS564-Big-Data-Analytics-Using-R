### HW13 ###
############

## Problem1
# (a)
HP <- read.csv('HappyIndex.csv', header=TRUE)
HD <- data.frame(Rank=HP[,1],Country=HP[,2],LifeExpectancy=HP[,4],Wellbeing=HP[,5],
                 Footprint=HP[,7],InequalityOutcome=HP[,8],HPI=HP[,11])

HD$round_wb = factor(round(HD$Wellbeing))
an = aov(HPI ~ round_wb, data=HD)
summary(an)

# (b)
(tkh = TukeyHSD(an, conf.level = 0.95))
plot(tkh, las=1)
