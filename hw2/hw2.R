### HW2 ###
###########
# Library
library(ggplot2); library(MASS);

# Problem 1
# (1)
animal <- Animals[order(Animals$brain),]
animal$row <- row.names(animal)
x <- log(animal[,2]); y <- log(animal[,1])

plot(x, y, type='l', col=4,
     xlab='log(brain)', ylab='log(body)', main='log(brain) vs log(body)')
points(x, y, pch=20, col=3)

# Modification of y coordinate for separation of labels
y[animal$row == 'Grey wolf'] =  y[animal$row == 'Grey wolf'] + 0.1
y[animal$row == 'Giraffe'] =  y[animal$row == 'Giraffe'] + 0.1
y[animal$row == 'Donkey'] =  y[animal$row == 'Donkey'] + 0.1
y[animal$row == 'Gorilla'] =  y[animal$row == 'Gorilla'] - 0.1
y[animal$row == 'Horse'] =  y[animal$row == 'Horse'] - 0.1

text(x, y, row.names(animal), cex=0.5)

# (2)
ggplot(animal, aes(x, y, label=row.names(animal))) + 
  geom_point(col=3) + geom_line(col=4) +
  xlab('log(brain)') + ylab('log(body)') +
  geom_text(size=3) +
  ggtitle('log(brain) vs log(body)')

# Problem 2
a <- c(652, 36, 218)
b <- c(1537, 46, 327)
c <- c(598, 38, 106)
d <- c(242, 21, 67)
caffeine <- matrix(c(a,b,c,d), nrow=4, byrow=T)
colnames(caffeine) <- c("Married", "Prev.married", "Single")
row.names(caffeine) <- c("0", "1-150", "151-300",">300")

barplot(caffeine, beside=T, col=2:5, xlab="Martial State", ylab="Caffeine Consumption",
        main="Caffeine Consumption against Martial State")
legend('topright', legend=rownames(caffeine), fill=2:5)

# Problem 3
attach(iris)
ggplot(iris, aes(x=Sepal.Length, fill=Species)) +
  geom_histogram(color="black") +
  facet_grid(Species~.)

# Problem 4
attach(airquality)
airquality$Month <- factor(airquality$Month, c("May", "June", "July", "August", "September"))

ggplot(airquality, aes(Day, Ozone, fill=Month, size=Wind, shape=Wind)) +
  geom_point(shape=21) +
  labs(x="Day of the Month", y="Ozone (ppb)",
       plot.title = element_text(hjust=0.5)) +
  ggtitle("Air Quality in New York by Day") +
  scale_x_continuous(breaks = seq(1,31,5)) +
  theme_gray()