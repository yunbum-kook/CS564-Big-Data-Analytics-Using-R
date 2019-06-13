## 13. What makes people happy
# library
library(psych); library(DescTools); library(coefplot);

load('HappyTrendData.RData')
str(happy_trend)

plot(happy_trend, type='l', col='blue', lwd=2,
     ylab='Google Search Relevance')
abline(lm(Happy~Time, data=happy_trend), col='red', lty=3, lwd=2)
legend('topleft', col=c('blue','red'),
       lty=c(1,3), lwd=c(2,2), bty="n",
       legend=c('Internet Searches for the word happy', 'Trend'))


### 2. Happy Planet Index ###
# (1) Happy Planet Index Data
HP <- read.csv('HappyIndex.csv', header=TRUE)
str(HP)
HD <- data.frame(Rank=HP[,1],Country=HP[,2],LifeExpectancy=HP[,4],Wellbeing=HP[,5],
                 Footprint=HP[,7],InequalityOutcome=HP[,8],HPI=HP[,11])
head(HD)

#InequalityOutcome : remove "%"
# sapply : apply function to some columns
# gsub : substitute some pattern by other string
# as.character(2) = "2"
# as.numeric("2") = 2
HD$InequalityOutcome = sapply(HD$InequalityOutcome, FUN=function(x)
  as.character(gsub("%", "", as.character(x), fixed=TRUE)))
HD$InequalityOutcome = as.numeric(HD$InequalityOutcome)

HRank = HD[order(HD$Rank, decreasing = FALSE),] #decresing's default is FALSE
dim(HRank)
head(HRank); tail(HRank)

# (2) Descriptive Statistics
summary(HD[,3:7])

##Boxplot
xa = c("LifeExpectancy","Wellbeing","Footprint",
       "InequalityLifeOutcome","HPI")
par(mfrow=c(2,3))
for(i in 3:7) boxplot(HD[,i],notch=T,col='azure',xlab=xa[i-2])
par(mfrow=c(1,1))

mad(HD[,3])
mean(DescTools::Trim(HD[,3]))

## Describe
describe(HD[,3:7])

# Density distribution
attach(HD); par(mfrow=c(3,2))
hist(LifeExpectancy,breaks=40,
     freq=FALSE,col='cyan')
lines(density(LifeExpectancy),col=2)
hist(Wellbeing,breaks=40,
     freq=FALSE,col='cyan')
lines(density(Wellbeing),col=2)
hist(Footprint,breaks=40,
     freq=FALSE,col='cyan')
lines(density(Footprint),col=2)
hist(InequalityOutcome,breaks=40,
     freq=FALSE,col='cyan')
lines(density(InequalityOutcome),col=2)
hist(HPI,breaks=40,
     freq=FALSE,col='cyan')
lines(density(HPI),col=2)
par(mfrow=c(1,1)); detach(HD)

HRank25 = subset(HRank, Rank<26)
par(mar = c(4.5, 5.5, 0.2, 0.5))
barplot(HRank25$HPI, names.arg=HRank25$Country,
        horiz=TRUE, col=cm.colors(25), las=1,
        cex.names=1, xlab="Happy Planet Index")

# (3) HPI Map
library(googleVis)
GC <- gvisGeoChart(HD, locationvar='Country', colorvar='LifeExpectancy', 
                   options=list(width=800,height=500, 
                                backgroundColor='lightblue',
                                colorAxis="{values:[48.9,57.6,66.3,75.0,83.6],
                                colors:[ \'magenta',\'orange',\'bisque',\'aquamarine',\'green']}"))
GT <- gvisTable(HRank[,c(1:3)],options=list(width=300,height=500))
plot(gvisMerge(GC,GT,horizontal=TRUE))


### 3. Factors for Happieness ###
## (1) Correlations
## Visulaization of Correlations
psych::pairs.panels(HD[,c(3:7)], lm=TRUE, ellipse=FALSE)

## (2) LifeExpectancy Map
GC = gvisGeoChart(HD, locationvar = 'Country', colorvar = 'LifeExpectancy',
                  options=list(width=800, height=500,
                               backgroundColor='lightblue',
                  colorAxis = "{values:[48.9, 57.6, 66.3, 75.0, 83.6],
                  colors:[ \'magenta', \'orange', \'bisque', \'aquamarine',\'green']}"))
GT = gvisTable(HRank[,c(1:3)], options = list(width=300, height=500))
plot(gvisMerge(GC, GT, horizontal = TRUE))

## (3) Wellbeing Map
GC = gvisGeoChart(HD, locationvar = 'Country', colorvar = 'Wellbeing',
                  options=list(width=800, height=500,
                               backgroundColor='lightblue',
                               colorAxis = "{values:[2.9, 4.1, 5.4, 6.6, 7.8],
                               colors:[ \'magenta', \'orange', \'bisque', \'aquamarine',\'green']}"))
GT = gvisTable(HRank[,c(1,2,4)], options = list(width=300, height=500))
plot(gvisMerge(GC, GT, horizontal = TRUE))

## (4) InequalityOutcome Map
GC = gvisGeoChart(HD, locationvar = 'Country', colorvar = 'InequalityOutcome',
                  options=list(width=800, height=500,
                               backgroundColor='lightblue',
                               colorAxis = "{values:[4.0, 15.8, 27.5, 39.3, 51.0],
                               colors:[ \'magenta', \'orange', \'bisque', \'aquamarine',\'green']}"))
GT = gvisTable(HRank[,c(1,2,6)], options = list(width=300, height=500))
plot(gvisMerge(GC, GT, horizontal = TRUE))

## (5) LinearRegression for Happiness vs. LifeExpectancy
fitL <- lm(HPI ~ LifeExpectancy, data=HD)
title = paste('HPI = ',round(fitL$coefficients[1],3),
              '+', round(fitL$coefficients[2],3), 'x LifeExpectancy')
plot(HD[,3],HD[,7], pch=21, bg='cyan', xlab='LifeExpectancy Score',
     ylab='Happy Planet Index (2016)', main=title)
abline(fitL, col=2)

## (6) LinearRegrssion for Happiness vs. Wellbeing 
fitW <- lm(HPI ~ Wellbeing, data=HD)
title = paste('HPI = ',round(fitL$coefficients[1],3),
              '+', round(fitL$coefficients[2],3), 'x Wellbeing')
plot(HD[,4],HD[,7], pch=21, bg='cyan', xlab='Wellbeing Score',
     ylab='Happy Planet Index (2016)', main=title)
abline(fitW, col=2)

## (7) LinearRegression for Happiness vs. (LifeExpectancy, Wellbeing)
fitA = lm(HPI ~ LifeExpectancy + Wellbeing, data=HD)
summary(fitA)

coefplot(fitA)

## (8) Clustering
kc = kmeans(HD[,c(3,4,7)], centers=3, nstart = 10)
HDC = data.frame(HD, Cluster = kc$cluster)
table(HDC$Cluster)

#page 19
library(dplyr)
Mean_by_Cluster <- HDC %>% 
  select(HPI, LifeExpectancy, Wellbeing, Cluster) %>%
  group_by(Cluster) %>% 
  summarise(mean_HPI=mean(HPI), mean_LifeExpectancy=mean(LifeExpectancy),
            mean_Wellbeing=mean(Wellbeing)) 
Mean_by_Cluster

subset(HDC$Country, HDC$Cluster==1)
subset(HDC$Country, HDC$Cluster==2)
subset(HDC$Country, HDC$Cluster==3)

#page 20 (skipped b/o SP?)
cols <- c("purple","green","magenta")
par(mfrow=c(3,1),mar=c(2,4,1,1))
boxplot(HPI~Cluster, boxwex=0.75,xlab="Cluster", 
        ylab="Happy Planet Index",col=cols, data=HDC)
boxplot(LifeExpectancy~Cluster, boxwex=0.75, xlab="Cluster", 
        ylab="Life Expectancy",col=cols, data=HDC)
boxplot(Wellbeing~Cluster, boxwex=0.75,xlab="Cluster", 
        ylab="Wellbeing", col=cols, data=HDC)
par(mfrow=c(1,1))

