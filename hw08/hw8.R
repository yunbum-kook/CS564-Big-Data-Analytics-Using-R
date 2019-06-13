### HW8 ###
###########
# Library
library(car);

# Problem 1
# (a)
education = c(rep("Less than High", 5), rep("High", 5),
              rep("College", 5));
breast = c(1, 6.5, 4.5, 2.0, 8.5, 1.5, 4.0, 3.5, 1.5, 5.0,
           11.0, 6.5, 4.5, 7.5, 9.0);
df1 = data.frame(breast, education)
out1 = aov(breast~education, data=df1)
summary(out1)

# (b)
plot(density(df1$breast[which(df1$education=="Less than High")]),
     xlab="Education", main="Density Graph", ylim=c(0,0.2), col="blue")
abline(v=mean(df1$breast[which(df1$education=="Less than High")]), col="blue")
lines(density(df1$breast[which(df1$education=="High")]), col="red")
abline(v=mean(df1$breast[which(df1$education=="High")]), col="red")
lines(density(df1$breast[which(df1$education=="College")]), col="green")
abline(v=mean(df1$breast[which(df1$education=="College")]), col="green")
legend("topright", legend=c("Less than High", "High", "College"),
       col=c("blue", "red", "green"), lwd=1:3, xpd=TRUE)

# (b) - other method
means = aggregate(df1$breast, list(df1$education), mean)
ggplot(df1, aes(x=df1$breast, col=df1$education)) +
  geom_density() + geom_vline(data=means, aes(xintercept = x, col=Group.1), linetype="dashed")

# Problem 2
# (a)
year = c(rep("year1",12),rep("year2",12),rep("year3",12),
         rep("year4",12),rep("year5",12))
month = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
              "Sep", "Oct", "Nov", "Dec"),5)
revenue = c(15,22,18,23,23,12,26,19,15,14,14,21,
            18,25,22,15,15,15,12,17,14,18,22,23,
            22,15,15,14,26,11,23,15,18,10,19,11,
            23,15,19,17,18,10,15,20,19,12,17,18,
            24,14,21,18,14,8,18,10,20,23,11,14)

df2 = data.frame(year, month, revenue)
out2 = aov(revenue ~ month + year, data=df2)
summary(out2)

# (b)
boxplot(revenue~month, data=df2,
        xlab='Month', ylab='Revenue', 
        main='Revenue vs Month', col=2:13)
abline(h=mean(df2$revenue), col='gray')

boxplot(revenue~year, data=df2,
        xlab='Year', ylab='Revenue',
        main='Revenue vs Year', col=2:13)
abline(h=mean(df2$revenue), col='gray')

# Problem 3
item = c(rep("Item1",4),rep("Item2",4),rep("Item3",4),
         rep("Item1",4),rep("Item2",4),rep("Item3",4)); item=factor(item);
region = c(rep("E",12),
         rep("W",12)); region=factor(region);
pop = c(25,36,31,26,39,42,39,35,36,24,28,29,
        51,47,47,52,43,39,53,46,42,36,32,33)

df3 = data.frame(Item=item, Region=region, Popularity=pop)
out3 = aov(pop ~ Item*Region, data=df3)
summary(out3)

# Problem 4
str(Baumann)
tail(Baumann)

attach(Baumann)
y = cbind(post.test.1,post.test.2,post.test.3)
fit = manova(y ~ group)
summary(fit)
