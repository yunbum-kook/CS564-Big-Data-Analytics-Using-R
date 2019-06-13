### HW7 ###
###########
# Library
library(ISwR); library(HSAUR3); library(broom); library(dplyr)

# Problem 1
# (a)
high = c(3.3, 2.9, 2.5, 4.0, 2.8, 2.5, 3.7, 3.8, 3.5, 2.7, 2.6, 4.0);
college = c(2.7, 2.5, 1.9, 3.3, 2.7, 2.2, 3.1, 4.0, 2.9, 2.0, 3.1, 3.2);
df1 = data.frame(High_school=high, College=college)
(fit = lm(College ~ High_school, data=df1))

# (b)
with(df1, plot(High_school, College, pch=21, bg='cyan'))
lines(df1$High_school, fit$fitted.values, col='red')

# (c)
predict(fit, list(High_school=3.0))

# (d), (e)
summary(fit)
# CoD : 0.5975, CoND : 0.4025
# p-value : 0.0032 -> There exists significant relationships between variables in this model

# (f)
with(df1, plot(High_school, College, pch=21, bg='cyan'))
lines(df1$High_school, fit$fitted.values, col='red')
aug_fit = broom::augment(fit)
with(aug_fit, segments(x0 = high, y0 = college, x1 = high, y1 = .fitted))

# Problem 2
form <- dl.milk ~ sex + weight + ml.suppl + mat.weight + mat.height
mfit2 = lm(form, data=kfm)
summary(mfit2)

# Problem 4
# (a)
attach(womensrole)
fm <- cbind(agree, disagree) ~ education + gender;
out4 <- glm(fm, data=womensrole, family=binomial())
summary(out4)

# (b)
womensrole$agree_rate = agree / (agree + disagree)
fitvalues = predict(out4, type="response")
man_index = which(gender=="Male")
woman_index = which(gender=="Female")

plot(education[man_index], womensrole$agree_rate[man_index],
     xlab="Education", ylab="Probabilities of Agree", col="blue")
points(education[woman_index], womensrole$agree_rate[woman_index], col="red")
lines(education[man_index], fitvalues[man_index], col="blue")
lines(education[woman_index], fitvalues[woman_index], col="red")
legend("topright", legend=c("Man", "Woman"), col=c("blue", "red"), lwd=1:2)

