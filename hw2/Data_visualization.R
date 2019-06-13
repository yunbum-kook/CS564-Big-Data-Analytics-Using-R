# week 2-1, Data Visualization

##### 03 Data Visualization #####

library: ggplot2, plotrix, boot, scatterplot3d, lattice, MASS

#page 4
head(faithful)
fa <- faithful[order(faithful$waiting),]
head(fa)

x <- fa[,2]; y <- fa[,1]
plot(x, y, type='l', col=4, xlab='Waintg Time(min)', ylab='Eruption Time(min)', main='Title')
points(x,y,pch=20, col=3)

#page 6

library(ggplot2)

ggplot(fa, aes(x, y), xtitle='Waiting Time (min)') + 
  
  geom_point(col=3) + geom_line(col=4) +
  
  xlab('Waiting Time (min)') + 
  
  ylab('Eruption Time (min)') +
  
  ggtitle('Old Faithful Eruptions')

head(diamonds)
ggplot(data=diamonds, aes(x=carat, y=price)) +
  geom_point(aes(color=color))


head(cars)
barplot(cars[,2])
barplot(cars[,2], col="cornsilk", names.arg = cars[,1], xlab="Speed", ylab="Distance")

attach(sleep) # without $ sign, we can access columns of data frame
sleep #list data
extra
y <- rbind(extra[1:10], extra[11:20])
y
barplot(y, beside=T, col=5:6, names.arg = ID[1:10], xlab="ID", ylab="Extra Sleep Hour")
abline(h=0)
abline(h=5)
legend('topleft', title='group', legend=1:2, fill=5:6)

ggplot(sleep, aes(x=ID, y=extra, fill=group))
ggplot(sleep, aes(x=ID, y=extra, fill=group)) +
  geom_bar(stat="Identity", position="dodge")
ggplot(sleep, aes(x=ID, y=extra, fill=group)) +
  geom_bar(stat="Identity", position="dodge") +
  theme_bw()

UPE <- USPersonalExpenditure
UPE
win.graph(8,5) # not working for mac user
barplot(UPE, beside=T, col=2:6, ylab="Expenditure", xlab="Year", main="Title")
legend('topleft', legend=row.names(UPE), fill=2:6)

#page 14

Age <- c('<=24','25-34','35-44','45-54','55-64','65-74','>=75')
Obese <- c(4.9,7.7,11.6,16.8,23.7,23.3,11.9)
#paste(Obese, '%') means that append the string to each word
pie(Obese, labels=paste(Obese, '%'), main="Title", col=rainbow(length(Age)))
legend("topleft", Age, cex=0.8, fill=rainbow(length(Age)))


#page 15

pie(Obese, labels=paste(Obese,'%'), main='Obesity Percents by Age Group',
    
    col=rainbow(length(Age)))

legend("topleft", Age, cex=0.8, fill=rainbow(length(Age)))



#page 16

library(plotrix)

xb <- paste(Age,"\n",Obese,'%',sep="")

pie3D(Obese, labels=xb, explode=0.1,
      
      col=rainbow(length(Age)), 
      
      main="3D Pie Chart of Obesity Percents by Age Group")

### Histograms
# Simple histograms
library(boot)
dim(cane)
str(cane)
head(cane)

ratio = cane$r/cane$n
hist(ratio, breaks=20, xlab="Diseased Shoot Ratio", col="aquamarine", main="Title")

# Histograms in ggplots2
ggplot(cane, aes(x=r/n, fill=block)) +
  geom_histogram(colour="black") + theme_bw() +
  facet_grid(block ~ .) + xlab('Diseased Shoot Ratio') +
  theme(legend.position="none")          


### Bubble plot
# Data exploration
attach(USArrests)
head(USArrests)
summary(USArrests)

radius = Rape/max(Rape)
N = nrow(USArrests)
op = palette(rainbow(N))
symbols(Murder, Assault, circles = radius,
        inches=0.25, fg='black', bg=1:N,
        xlab='Murder (/100,000)',
        ylab='Assault (/100,000)',
        main='Circle shows Rape (7.3~46)')
text(Murder, Assault, row.names(USArrests),
     cex=0.8)
palette(op)

# ggplot2
ggplot(USArrests, aes(Murder,Assault,size=Rape,label=row.names(USArrests))) +
  geom_point(colour="magenta")  + geom_text(size=3) + theme_bw() +
  xlab("Murders (/100,000)") + ylab("Assault (/100,000)")

### Boxplot
# Simple boxplot
head(ToothGrowth)
boxplot(len~supp, data=ToothGrowth,
        xlab='Supplement Type', ylab='Tooth Length')

# Boxplot of len against dose and supp factors
boxplot(len~supp*dose, data=ToothGrowth, notch=T,
        xlab="Suppliment and Dose", ylab="Tooth Length",
        col=c("cyan", "magenta"))
boxplot(len~dose*supp, data=ToothGrowth, notch=T,
        xlab="Suppliment and Dose", ylab="Tooth Length",
        col=c("cyan", "magenta"))


# Boxplot in ggplot2
ggplot(ToothGrowth, aes(x=factor(dose), y=len)) +
  geom_boxplot(aes(fill=supp)) +
  xlab("dose") + ylab("length") +
  ggtitle("Analyzing ToothGrowth Data")

### Saving plots
png("SampleSavePlot.png", width=580, height=640)
x = rnorm(100, mean=0, sd=1)
hist(x, freq=F, col='cyan')
lines(density(x), col='red')
dev.off()
#page 27

library(scatterplot3d) 

s3d <- scatterplot3d(iris[,1:3], color=c(2:4)[iris$Species],
                     
                     col.axis="blue", col.grid="lightblue", pch=16, cex.symbols=1)

legend(s3d$xyz.convert(2.5,3.1,8.8), legend=levels(iris$Species),
       
       col=2:4, pch=16, bty="n", title="Species")



#page 28

library(lattice)

x <- seq(-3, 3, .2); y=x

dat <- expand.grid(x,y)

dat$z <- dnorm(dat[,1])*dnorm(dat[,2])

names(dat) <- c('x','y','z')

wireframe(z ~ x*y, data=dat,  scales=list(arrows=FALSE),
          
          aspect=c(1,.6), drape=TRUE,
          
          par.settings=list(axis.line=list(col='transparent')))