### HW5 ###
###########
# Library
library(robustbase);

# Problem 1
# (1)
shapiro.test(mtcars$mpg) # p-value > 0.05; mpg is normally distributed

# (2)
mtcars$am_factor = factor(mtcars$am)
var.test(mpg~am_factor, data=mtcars)$p.value # 0.0669052 -> equal variance
two <- t.test(mpg~am_factor, data=mtcars,
              alternative="two.sided", var.equal=TRUE)
two
less <- t.test(mpg~am_factor, data=mtcars,
               alternative="less", var.equal=TRUE)
less

# Problem 2
data <- NOxEmissions$LNOx
m <- mean(data); s <- sd(data);
interval <- qnorm(0.975)/sqrt(length(data))
left <- m - interval
right <- m + interval
cat("The 95% confidence interval is [",left,", ",right,"]")
