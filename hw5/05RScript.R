##### 05 Probability, Distributions, and Hypothesis Test #####

#library:  

### 1. Random Sampling ###
##########################

# Picking six numbers at random from the set 1:45
sample(1:45,6)

# Simulating 10 coin tosses: B=bottom, T=top
x <- sample(c('B','T'), 10, replace=T)
table(x)/length(x)

x <- sample(c('B','T'), 100000, replace=T)
table(x)/length(x)

x <- sample(c('B','T'), 10000000, replace=T)
table(x)/length(x)

#page 3
xp <- sample(c("sucess","fail"), 100, replace=T, prob=c(0.8,0.2))
table(xp) # count how many success and fail came out

#총 20번 실행, 성공 확률 20%, 총 성공횟수가 x번인 확률
dbinom(x=6, size=20, prob=0.2)

### 2. Normal Distribution ###
##############################

# A simple normal curve
x1=-4; x2=4
x <- seq(x1, x2, 0.1); y <- dnorm(x)
plot(x, y, type='l', xlim=c(x1,x2), ylim=c(0,0.4), 
     xlab="x", ylab="Probability Density")

# Examination grades of 80 students
x <-  c(72,49,81,52,31, 38,81,58,58,73,
        43,56,45,54,40, 81,60,52,52,38,
        79,83,63,58,59, 71,89,73,77,60,
        65,60,69,88,75, 59,52,75,70,93,
        90,62,91,61,53, 83,32,49,39,57,
        39,28,67,74,61, 42,39,76,68,65,
        58,49,72,29,70, 56,48,60,36,79,
        72,65,40,49,37, 63,72,58,62,46)

hist(x, freq=F)
lines(density(x))

### Normality test ; p-value > 0.05 => Normal distribution
shapiro.test(x)

qqnorm(x)
qqline(x, col=3)

# dnorm = density functoin of normal distribution
# pnorm = cdf와 같음; pnorm(given value, mean, std) = cdf of given value
# qnorm = quantile of nomal distribution = inverse of cdf
m <- mean(x); s <- sd(x)
pnorm(m,m,s)
s1 <- pnorm(m+s, m, s)
s0 <- pnorm(m, m, s)
s10 <- round(s1-s0, 4)*100; s10

s2 <- pnorm(m+2*s, m, s)
s21 <- round(s2-s1,4)*100; s21

### 3. Sampling Distribution of Means ###
#########################################
# Sample size가 충분하면, 제시된 testing mean과 함께sample mean, sdv를 통해
# z-score를 계산해서 해당 sample mean이 주어질 확률을 구할 수 있음.


### 4. Sampling Distribution of Means ###
#########################################
# 하지만, sample size가 작거나 std를 모를 때, t-score로 판단 가능
# dt = density function of t-distribution
#   dt(value, degree of freedom)
# qt = quantile of t-value; inverse of cdf
#   qt(value, degree of freedom)
plot(function(x) dt(x,df=10), -4,4, ylim=c(0,0.4))
a=0.05; (tc=qt(1-a/2,df=15)) 
dt(x,df=15)
curve(dt(x,df=15),-5,5,200,
      col=4,ylab="Probability",
      xlab="t-value")
abline(v=tc,col='red')

# t.test(x)를 이용해서 CI(95%)를 쉽게 계산 가능
x = c(446,450,458,452,456,462,449,460,467,455)
cat("Confidence intervals:",ci," for x\n")

### 5. Hypothesis Tesing ###
############################

### 5-1. Independent two sets의 mean comparison
data(energy, package="ISwR")
str(energy)
head(energy)

# 두 집단의 variance가 같은지 test
var.test(expend~stature, data=energy)$p.value 

# t.test를 통해서 두 집단의 mean이 같다는 null을 테스트
two <- t.test(expend~stature, data=energy,
              alternative="two.sided", var.equal=TRUE)
two

# $statistic으로 t값에 접근가능
ts = round(two$statistic, 3) # Test statistic t
a=0.05
tc = round(qt(a/2, df=two$parameter), 3) # critical t
cat("Test statistic t = ", ts, "critical t = ", tc, "\n")

# Visualization
load("glib.RData")
x2=5; t_curve(x2,-tc)
text(ts,0.15,paste("t=",ts))
abline(v=ts)

#page 34
boxplot(expend~stature, data=energy, xlab="Woman group",
        ylab="24 hour energy expenditure in MJ", 
        col=c('cyan','magenta'))


### 5-2. Dependent two sets의 mean comparison (교육후 시험점수 비교)

dt <- read.csv("PrePost.csv", header=T)
head(dt)

# paired t-test; paired=TRUE를 해줘야 함
pt <- t.test(dt$x1, dt$x2, paired=TRUE)
pt
ts = round(pt$statistic, 3) #statistic t
dfp = pt$parameter; a=0.05
tc = round(qt(a/2, df=dfp), 3) #criticial t
cat("Test statistic t=", ts, "critical t=",tc,"\n")

boxplot(dy, ylab="Score", notch=TRUE,
        col=c('cyan','magenta'))
abline(h=mean(dt$x1), col='cyan')
abline(h=mean(dt$x2), col='magenta')
